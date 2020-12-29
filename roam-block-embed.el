;;; roam-block-embed.el --- Block embed implement of roam-block -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: block roam convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/roam-block
;; Package-Requires: ((emacs "26.1") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (ov "1.0.6") (promise "1.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This Module implements block embed functions of roam-block.

;;; Code:

;;;; Requires

(require 'roam-block-db)

;;;; Declarations

;; (declare-function roam-block-ref-fontify-at-real-time "roam-block-ref")

;;;; Variables

(defvar roam-block-embed-id nil
  "The embed-id of block that identify all the same embed blocks.")

(defvar roam-block-embed-content nil
  "The content of block that will be inserted as a embed block.")

(defface roam-block-embed-face
  '((t :inherit hl-line))
  "Face for embed blocks.")

(defface roam-block-embed-margin-face
  '((t :inherit fringe))
  "Face for embed block left margin.")

(defvar roam-block-embed-highlight nil
  "Non-nil means to highlight the embed block and
distinguish it with the original block.")

(defvar roam-block-embed-ovs nil
  "The list of overlays for embed blocks.")

;;;; Functions

(defun roam-block-embed--search-replace (buf uuid content)
  "Search the specific UUID block and replace the content 
with new CONTENT in buffer BUF."
  (with-current-buffer buf
    (save-excursion
      (when-let* ((ov (car (ov-in 'uuid uuid)))
                  (ov-beg (ov-beg ov))
                  (ov-end (ov-end ov))
                  (ov-content (buffer-substring-no-properties
                               ov-beg ov-end)))
        (unless (string= ov-content content)
          (goto-char ov-end)
          (insert content)
          (delete-region ov-beg ov-end))))))

(defun roam-block-embed-sync-at-real-time ()
  "Synchronize all blocks with the same embed-id at real time
for buffers at live window list."
  (while-no-input
    (redisplay)
    (when (roam-block-work-on)
      (when-let*
          ((block (roam-block--block-uuid))
           (uuid (nth 0 block))
           (beg (nth 1 block))
           (end (nth 2 block))
           (content (buffer-substring-no-properties beg end))
           (embedp-promise (roam-block-db--block-embedp uuid))
           (embed-id-promise (roam-block-db--embed-id uuid)))
        (promise-then
         embedp-promise
         (lambda (embedp)
           (when (= embedp 1)
             (promise-then
              embed-id-promise
              (lambda (embed-id)
                (promise-chain (roam-block-db--embed-blocks embed-id)
                  (then
                   (lambda (data)
                     (mapcar
                      (lambda (item)
                        (when-let
                            ((buf (find-buffer-visiting (car item)))
                             (uuid-lst (cdr item)))
                          (dolist (block-uuid uuid-lst)
                            (unless (string= uuid block-uuid)
                              (roam-block-embed--search-replace
                               buf block-uuid content)))))
                      data)))
                  (promise-catch (lambda (reason)
                                   (message reason)))))))))
        ;; (roam-block-ref-fontify-at-real-time uuid content)
        ))))

(defun roam-block-embed-sync-from-db ()
  "Synchronize all blocks with embed-id by content in database."
  (let ((data (roam-block-db--all-embedp)))
    (when-let* ((blocks (assoc (buffer-file-name) data))
                (uuid-lst (cdr blocks)))
      (dolist (uuid uuid-lst)
        (when-let*
            ((ov (car (ov-in 'uuid uuid)))
             (ov-beg (ov-beg ov))
             (ov-end (ov-end ov))
             (content (buffer-substring-no-properties ov-beg ov-end))
             (db-content (roam-block-db--block-content uuid)))
          (unless (string= content db-content)
            (save-excursion
              (goto-char ov-end)
              (insert db-content)
              (delete-region ov-beg ov-end))))))))

;;;###autoload
(defun roam-block-embed-store ()
  "Store the block content to kill-ring,
use the block at point by default."
  (interactive)
  (if-let ((uuid (car (roam-block--block-uuid))))
      (let ((content (roam-block-db--block-content uuid))
            ;; FIXME: retrive from database or buffer-substring?
            ;; what about when buffer is not saved?
            (embed-id-promise (roam-block-db--embed-id uuid)))
        (setq roam-block-embed-content content)
        (promise-then
         embed-id-promise
         (lambda (embed-id)
           (setq roam-block-embed-id embed-id)
           (message "(roam-block) The embed block has been stored."))))
    (user-error "(roam-block) No valid block here!")))

;;;###autoload
(defun roam-block-embed-insert ()
  "Insert the embed block last stored at point."
  (interactive)
  (let ((file (buffer-file-name))
        (uuid (roam-block--get-uuid))
        (embed-id roam-block-embed-id)
        (content roam-block-embed-content)
        (beg (point)) end)
    (if (bolp)
        (progn
          (insert content)
          (setq end (point))
          (if roam-block-embed-highlight
              (progn
                (roam-block-overlay-block
                 beg end uuid)
                (roam-block-embed-overlay-block uuid))
            (roam-block-overlay-block beg end uuid))   
          ;; FIXME: It's fine to update the original block only.
          (roam-block-db-query
           `[:update blocks :set (= embedp 1)
                     :where (= embed-id ,embed-id)])
          (roam-block-db-query
           `[:insert :into blocks
                     :values ([,uuid ,embed-id 1 ,beg ,content ,file])]))
      (user-error
       "(roam-block) The embed block should be insert \
at the beginning of line!"))))

(defun roam-block-embed--uuids ()
  "Return the embed blocks' uuid grouped by file, 
except the original block."
  (let ((data (roam-block-db--all-embedp)))
    data))

(defun roam-block-embed-overlay-block (uuid)
  "Highlight the embed block with uuid UUID."
  (let* ((ov (car (ov-in 'uuid uuid)))
         (beg (ov-beg ov))
         (end (ov-end ov))
         (content (buffer-substring beg end)))
    (ov-set ov 'face 'roam-block-embed-face
            'line-prefix
            (propertize
             "▏" 'face 'roam-block-embed-margin-face)
            'wrap-prefix
            (propertize
             "▏" 'face 'roam-block-embed-margin-face))
    (push ov roam-block-embed-ovs)))

(defun roam-block-embed-overlay ()
  "Highlight all embed blocks for live buffers."
  (let ((data (roam-block-embed--uuids)))
    (setq roam-block-embed-ovs nil)
    (dolist (file-uuids data)
      (let ((file (car file-uuids))
            (uuids (cdr file-uuids)))
        (when-let* ((buf (find-buffer-visiting file)))
          (dolist (uuid uuids)
            (let ((embed-id-promise (roam-block-db--embed-id uuid)))
              (promise-then
               embed-id-promise
               (lambda (embed-id)
                 (unless (string= uuid embed-id)
                   (with-current-buffer buf
                     (when uuid
                       (roam-block-embed-overlay-block uuid)))))))))))))

;;;###autoload
(defun roam-block-embed-highlight-toggle ()
  "Determine whether to highlight the embed blocks."
  (interactive)
  (if roam-block-embed-highlight
      (setq roam-block-embed-highlight nil)
    (setq roam-block-embed-highlight t))
  (if roam-block-embed-highlight
      (progn
        (roam-block-embed-overlay)
        (message "(roam-block) Show embed blocks highlight"))
    (mapcar (lambda (ov)
              (ov-set ov 'face nil 'line-prefix nil 'wrap-prefix nil))
            roam-block-embed-ovs)
    (setq roam-block-embed-ovs nil)
    (message "(roam-block) Hide embed blocks highlight")))

(provide 'roam-block-embed)
;;; roam-block-embed.el ends here
