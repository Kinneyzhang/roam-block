;;; roam-block-ref.el --- Block ref functions -*- coding: utf-8; lexical-binding: t; -*-

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

;; This Module implements block ref funtions of roam-block.

;;; Code:

;;;; Requires

(require 'roam-block-util)
(require 'roam-block-db)

;;;; Variables

(defvar roam-block-ref-re "((\\([a-z0-9]\\{32\\}\\)))"
  "Regular expression that matches a `roam-block-link'.")

(defvar roam-block-ref-buf "*Roam Block Ref*"
  "Name of the block references buffer.")

(defvar roam-block-ref-edit-buf "*Roam Block Edit*"
  "Name of buffer for editing blocks.")

(defvar roam-block-ref-highlight t
  "Non-nil means to highlight the refered block display and
distinguish it with the original block.")

(defvar-local roam-block-ref-uuid nil
  "The uuid of block in edit buffer.")

(defvar-local roam-block-ref-content nil
  "The origin content of block in edite buffer.")

(defvar-local roam-block-ref-in-file nil
  "The file that the uuid link belongs to.")

(defvar-local roam-block-ref-original-file nil
  "The origin file of the edited uuid block.")

;;;; Functions

(define-button-type 'roam-block-ref
  'action #'roam-block-follow-ref
  'face nil
  'content nil
  'follow-link nil
  'help-echo "Jump to this block.")

(defun roam-block-follow-ref (btn)
  "Jump to block references buffer after follow roam-block ref link."
  (with-demoted-errors "Error when following the link: %s"
    (let ((content (button-get btn 'content)))
      (with-current-buffer (get-buffer-create roam-block-ref-buf)
        ;; Should set the buffer mode to the
        ;; origin uuid block file's major mode.
        (erase-buffer)
        (insert (propertize content 'face '(:height 1.2)))
        (setq-local header-line-format "View Buffer: Press 'q' to quit."))
      (view-buffer roam-block-ref-buf))))

(defun roam-block-ref-fontify (beg end)
  "Highlight roam-block ref between BEG and END."
  (when (roam-block-work-on)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward roam-block-ref-re end t)
        (let* ((uuid (match-string-no-properties 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (content (roam-block-db--block-content uuid))
               (propertized-content
                (when content
                  (if roam-block-ref-highlight
                      (propertize content 'face '(:underline "#aaa"))
                    content))))
          (if content
              (with-silent-modifications
                (add-text-properties beg end
                                     `(display ,propertized-content
                                               read-only t))
                (make-text-button beg end :type 'roam-block-ref
                                  'content content))
            (with-silent-modifications
              (remove-text-properties beg end '(display nil read-only nil face nil)))))))))

(defun roam-block-ref-fontify-all ()
  "Highlight roam-block link in all current frame's windows."
  ;; fontify current buffer
  (roam-block-ref-fontify (point-min) (point-max))
  ;; fontify all displayed windows
  (let ((wins (window-list)))
    (save-selected-window
      (dolist (win wins)
        (select-window win)
        (when (roam-block-work-on)
          (roam-block-ref-fontify (point-min) (point-max)))))))

;; Edit block

(define-minor-mode roam-block-ref-edit-mode
  "Minor mode for editing a refered read only block."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-k") #'roam-block-ref--edit-abort)
            (define-key map (kbd "C-c C-c") #'roam-block-ref--edit-finalize)
            map)
  :require 'roam-block
  (if roam-block-ref-edit-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   "\\<roam-block-ref-edit-mode-map>Edit block: `\\[roam-block-ref--edit-finalize]' \
to finish, `\\[roam-block-ref--edit-abort]' to abort."))
    (setq-local header-line-format nil))
  (setq truncate-lines nil))

(defun roam-block-ref--edit-abort ()
  "Abort editing the read only block content."
  (interactive)
  (switch-to-buffer (get-file-buffer roam-block-ref-in-file))
  (kill-buffer roam-block-ref-edit-buf))

(defun roam-block-ref--edit-finalize ()
  "Finish editing the read only block content."
  (interactive)
  (let ((file roam-block-ref-in-file)
        (origin-file roam-block-ref-original-file)
        (uuid roam-block-ref-uuid)
        (origin-content roam-block-ref-content)
        (content (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer (find-file-noselect origin-file)
      (save-excursion
        (goto-char (point-min))
        (catch 'break
          (while (search-forward origin-content nil t)
            (message "found the same content!")
            (when (string= uuid (get-char-property
                                 (line-beginning-position) 'uuid))
              (message "found the original uuid one!")
              (replace-match content)
              (let ((beg (match-beginning 0))
                    (len (length content)))
                (roam-block-propertize-block beg (+ beg len) uuid))
              (save-buffer)
              (throw 'break nil))))))
    (switch-to-buffer (get-file-buffer file))
    (save-excursion
      (goto-char (point-min))
      (search-forward (format "((%s))" uuid) nil t)
      (roam-block-ref-fontify (match-beginning 0) (match-end 0)))
    (kill-buffer roam-block-ref-edit-buf)))

(defvar roam-block-stored-ref nil
  "Roam block ref that have stored.")

;;;###autoload
(defun roam-block-ref-store ()
  "Store the roam-block ref to kill-ring, use the block at point by default.
If a region is active, copy all blocks' ref links that the region contains."
  (interactive)
  (cond
   ((region-active-p)
    (let ((beg (region-beginning))
          (end (region-end))
          ref-str block-end)
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (if-let ((uuid (roam-block--block-uuid)))
              (progn
                (setq block-end (roam-block--block-end))
                (setq ref-str (concat ref-str (format "((%s))" uuid) "\n"))
                (goto-char block-end)
                (forward-line))
            (setq ref-str (concat ref-str "\n"
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
            (forward-line)))
        (setq roam-block-stored-ref ref-str))))
   (t (save-excursion
        (let ((uuid (get-char-property (point) 'uuid)))
          (if uuid
              (setq roam-block-stored-ref (format "((%s))" uuid))
            (setq uuid (get-char-property (line-beginning-position) 'uuid))
            (if uuid
                (setq roam-block-stored-ref (format "((%s))" uuid))
              (user-error "(roam-block) No valid block here!")))))))
  (message "(roam-block) The block refs are stored."))

;;;###autoload
(defun roam-block-ref-insert ()
  "Insert the stored roam-block ref at point."
  (interactive)
  (let ((beg (point)) end)
    (insert roam-block-stored-ref)
    (setq end (point))
    (unless (roam-block--block-uuid)
      (roam-block-propertize-block beg end))
    (roam-block-ref-fontify beg end)))

;;;###autoload
(defun roam-block-ref-edit ()
  "Edit the content of the ref block."
  (interactive)
  (let* ((file (buffer-file-name))
         (uuid (car (roam-block--ref-uuid)))
         (origin-file (roam-block-db--block-file uuid))
         (content (roam-block-db--block-content uuid))
         (mode major-mode))
    (if uuid
        (with-current-buffer (get-buffer-create roam-block-ref-edit-buf)
          (insert content)
          (funcall mode)
          (setq roam-block-ref-in-file file)
          (setq roam-block-ref-original-file origin-file)
          (setq roam-block-ref-uuid uuid)
          (setq roam-block-ref-content content)
          (roam-block-ref-edit-mode)
          (switch-to-buffer roam-block-ref-edit-buf))
      (message "(roam-block) No block ref here!"))))

;;;###autoload
(defun roam-block-ref-delete ()
  "Delete the roam-block ref at point."
  (interactive)
  (let* ((lst (roam-block--ref-uuid))
         (uuid (nth 0 lst))
         (beg (nth 1 lst))
         (end (nth 2 lst)))
    (let ((inhibit-read-only t))
      (remove-text-properties beg end '(read-only nil))
      (delete-region beg end))))

(provide 'roam-block-ref)
;;; roam-block-ref.el ends here
