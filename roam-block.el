;;; roam-block.el --- An all-purpose block ref and block embed implement in emacs -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: block roam convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/roam-block
;; Package-Requires: ((emacs "26.1") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

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

;; An all-purpose block ref and block embed implement in emacs.

;;; Code:

;;;; Requires

(require 'subr-x)
(require 'org-element)

(require 'roam-block-ref)
(require 'roam-block-embed)

;;;; Variables

(defgroup roam-block nil
  "Block references and block embed implement in file.")

(defcustom roam-block-home nil
  "Home directories that `roam-block-mode' works on.
The value of this variable is a list, the item of the list 
is either a file or a directory. Roam-block-mode can work on 
all the files and directories in the list.

A directory should end with a directory separator character.
In Linux or Macos, the directory separator character is '/'.
In Windows, the directory separator character is '\\'."
  :type 'list
  :group 'roam-block)

(defvar roam-block-skip-start-re "\\(^ *#+.+:.+\n+\\)+"
  "Regular expression that matches contents needed to be skipped
at the beginning of file, usually are some meta settings.")

(defvar-local roam-block-linum nil)

(defvar-local roam-block-ovs nil)

;;;; Functions

(defun roam-block-insert-in-front (ov after &rest args)
  "Move overlays when insert in front of a OV overlay.
 When AFTER is non-nil, call function after change,
 otherwise call function before change. ARGS are the rest arguments."
  (if (null after)
      (setq roam-block-linum (line-number-at-pos))
    (unless (= roam-block-linum (line-number-at-pos))
      (ov-move ov (line-beginning-position) (ov-end ov)))))

(defun roam-block-overlay-block (beg end &optional uuid prop value)
  "Put uuid overlay between BEG and END. If UUID is non-nil, 
use it as the value of uuid overlay.  
If PROP and VALUE is non-nil, also add the overlay on this block."
  (let ((uuid (or uuid (roam-block--get-uuid)))
        (inhibit-read-only t)
        (ov (ov-make beg end nil nil t)))
    (push ov roam-block-ovs)
    (ov-set ov 'uuid uuid)
    (ov-set ov 'evaporate t)
    (ov-set ov 'insert-in-front-hooks
            '(roam-block-insert-in-front))
    (when (and prop value) (ov-set ov prop value))
    uuid))

(defun roam-block-restore-overlays ()
  "Restore uuid overlays from database in the file."
  (when (roam-block-work-home)
    (when-let ((regions (roam-block-db--have-regions)))
      (progn
        (dolist (region regions)
          (when-let*
              ((beg (car region))
               (end (cdr region))
               (uuid (caar (roam-block-db-query
                            `[:select uuid :from blocks
                                      :where
                                      (and (= begin ,beg)
                                           (= file ,(buffer-file-name)))]))))
            (roam-block-overlay-block beg end uuid)))
        ;; If the file have region in database to restore
        ;; overlays, return t. Otherwise return nil.
        t))))

(defun roam-block-overlay-buffer ()
  "Put uuid overlay to the whole buffer's valid blocks 
according to the major mode MODE."
  (pcase major-mode
    ('org-mode (roam-block-overlay-org-buffer))))

(defun roam-block-block-overlay-update (block-beg block-end)
  "Update current block's uuid overlay.

If there's no uuid overlay on current block, add it and return the uuid.
If there's uuid overlay, compare the overlay range with block range.

If the two are equal, return the uuid. Otherwise, set overlay range to
the new block range and return the uuid.

BLOCK-BEG is the beginning of the block. BLOCK-END is the end of the block."
  (if-let* ((ov (ov-at))
            (uuid (ov-val ov 'uuid))
            (beg (ov-beg ov))
            (end (ov-end ov)))
      (progn
        (unless (and (= beg block-beg) (= end block-end))
          (ov-reset ov)
          (roam-block-overlay-block block-beg block-end uuid))
        uuid)
    (let ((uuid (roam-block--get-uuid)))
      (roam-block-overlay-block block-beg block-end uuid)
      uuid)))

(defun roam-block-overlay-org-buffer ()
  "Put uuid overlay to current org buffer's valid blocks."
  (save-excursion
    (save-restriction
      (roam-block--narrow-to-content)
      (goto-char (point-min))
      (let ((file (buffer-file-name))
            uuid-lst)
        (while (< (point) (point-max))
          ;; FIXME: Use diff to compare the twice file save,
          ;; only update those changed blocks.
          (let* ((elem (org-element-at-point))
                 (elem-type (org-element-type elem)))
            (pcase elem-type
              ((guard (looking-at "^ *$"))
               (forward-line))
              ('headline
               (forward-line))
              ('property-drawer
               (goto-char (org-element-property :end elem)))
              ('paragraph
               (let* ((beg (org-element-property :contents-begin elem))
                      (end (org-element-property :contents-end elem))
                      (block-end (1- end))
                      (content (buffer-substring-no-properties beg block-end))
                      (uuid (roam-block-block-overlay-update beg block-end)))
                 ;; FIXME: consider the condition of the last line, do
                 ;; not need to minus 1 from end.
                 (roam-block-db--block-update uuid content)
                 (push uuid uuid-lst)
                 (goto-char end)))
              ('plain-list
               (let* ((structure (org-element-property :structure elem))
                      (tree (car structure))
                      (beg (car tree))
                      (end (car (last tree)))
                      (block-end (1- end))
                      (content (buffer-substring-no-properties beg block-end))
                      (uuid (roam-block-block-overlay-update beg block-end)))
                 (roam-block-db--block-update uuid content)
                 (push uuid uuid-lst)
                 (goto-char end)))
              (_ (let* ((beg (org-element-property :begin elem))
                        (end (org-element-property :end elem))
                        (blank (org-element-property :post-blank elem))
                        (block-end (- end blank 1))
                        (content (buffer-substring-no-properties beg block-end))
                        (uuid (roam-block-block-overlay-update beg block-end)))
                   (roam-block-db--block-update uuid content)
                   (push uuid uuid-lst)
                   (goto-char end))))))
        uuid-lst))))

;; Hook functions

(defun roam-block--buffer-setting ()
  "Some settings for roam-block buffer."
  (setq-local show-paren-mode nil))

(defun roam-block--find-file-hook-function ()
  "Roam-block function binded to `find-file-hook'.
If there exists caches of the file, restore overlays of the file buffer.
If there doesn't exist caches of the file, add overlays for each block in
file and cache them database."
  (when (roam-block-work-home)
    (unless (roam-block-restore-overlays)
      (roam-block-db-cache-file))
    (roam-block-embed-sync-from-db)
    (roam-block-ref-fontify-all)
    (roam-block--buffer-setting)))

(defun roam-block--after-save-hook-function ()
  "Roam-block function binded to `after-save-hook'.
Update caches of those changed blocks and fontify block ref links."
  ;; FIXME: Cannot work properly in markdown-mode.
  ;; After the md buffer is saved, the display attribute
  ;; of overlays will lost.
  (when (roam-block-work-home)
    (roam-block-db-cache-file)
    (roam-block-ref-fontify-all)
    (roam-block--buffer-setting)))

;; Minor mode

(define-minor-mode roam-block-mode
  "Minor mode for roam-block."
  :lighter " block"
  :keymap (let ((map (make-sparse-keymap))) map)
  :group 'roam-block
  :require 'roam-block
  :global t
  (if roam-block-mode
      (progn
        (roam-block-check-sqlite3)
        (roam-block-db)
        (jit-lock-register #'roam-block-ref-fontify)
        (roam-block-ref-fontify-all)
        (add-hook 'find-file-hook #'roam-block--find-file-hook-function)
        (add-hook 'post-command-hook #'roam-block-embed-sync-at-real-time)
        (add-hook 'after-save-hook #'roam-block--after-save-hook-function)
        (add-hook 'kill-emacs-hook #'roam-block-db--close-all))
    (jit-lock-unregister #'roam-block-ref-fontify)
    (remove-hook 'find-file-hook #'roam-block--find-file-hook-function)
    (remove-hook 'post-command-hook #'roam-block-embed-sync-at-real-time)
    (remove-hook 'after-save-hook #'roam-block--after-save-hook-function)
    (remove-hook 'kill-emacs-hook #'roam-block-db--close-all)
    (roam-block-ref-remove-properties))
  (jit-lock-refontify))

;; Commands

;;;###autoload
(defun roam-block-delete-block ()
  "Delete current block.  If there's no block at point, 
prompt a message."
  (interactive)
  (let ((data (roam-block--block-uuid))
        beg end)
    (if data
        (progn
          (setq beg (nth 1 data))
          (setq end (nth 2 data))
          (delete-region beg end))
      (message "(roam-block) No valid block here!"))))

(provide 'roam-block)
;;; roam-block.el ends here
