;;; roam-block-ref.el --- Block ref functions -*- lexical-binding: t; -*-

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
  'face '(:underline "#aaa")
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
               (content (roam-block-db--block-content uuid)))
          (if content
              (with-silent-modifications
                (add-text-properties beg end `(display ,content read-only t))
                (make-text-button beg end :type 'roam-block-ref
                                  'content content))
            (with-silent-modifications
              (remove-text-properties beg end '(display nil read-only nil)))))))))

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
            (define-key map (kbd "C-c C-k") #'roam-block--edit-abort)
            (define-key map (kbd "C-c C-c") #'roam-block--edit-finalize)
            map)
  :require 'roam-block
  (if roam-block-ref-edit-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   "\\<roam-block-ref-edit-mode-map>Edit block: `\\[roam-block--edit-finalize]' \
to finish, `\\[roam-block--edit-abort]' to abort."))
    (setq-local header-line-format nil))
  (setq truncate-lines nil))

(defun roam-block--edit-abort ()
  "Abort editing the read only block content."
  (interactive)
  (switch-to-buffer (get-file-buffer roam-block-ref-in-file))
  (kill-buffer roam-block-ref-edit-buf))

(defun roam-block--edit-finalize ()
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
        ;; have bugs!
        (catch 'break
          (while (search-forward origin-content nil t)
            (when (string= uuid (get-char-property
                                 (line-beginning-position) 'uuid))
              (replace-match content)
              (save-buffer)
              (throw 'break nil))))))
    (switch-to-buffer (get-file-buffer file))
    (save-excursion
      (goto-char (point-min))
      (search-forward (format "((%s))" uuid) nil t)
      (roam-block-ref-fontify (match-beginning 0) (match-end 0)))
    (kill-buffer roam-block-ref-edit-buf)))

;;;###autoload
(defun roam-block-copy-link ()
  "Save the roam-block link to kill-ring, use the block at point by default.
If a region is active, copy all blocks' ref links that the region contains."
  (interactive)
  (cond
   ((region-active-p)
    (let ((beg (region-beginning))
          (end (region-end))
          ref-str)
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (when-let ((uuid (get-char-property (point) 'uuid)))
            (setq ref-str (concat ref-str (format "((%s))" uuid) "\n")))
          (forward-line))
        (kill-new ref-str))))
   (t (save-excursion
        (goto-char (line-beginning-position))
        (kill-new (format "((%s))" (get-char-property (point) 'uuid))))))
  (message "Have copyed the block refs."))

;;;###autoload
(defun roam-block-edit-block ()
  "Edit the content of the read only block."
  (interactive)
  (let* ((file (buffer-file-name))
         (uuid (roam-block--ref-uuid))
         (origin-file (roam-block-db--block-file uuid))
         (content (roam-block-db--block-content uuid))
         (mode major-mode))
    (if uuid
        (with-current-buffer (get-buffer-create roam-block-ref-edit-buf)
          (insert content)
          (setq major-mode mode)
          (setq roam-block-ref-in-file file)
          (setq roam-block-ref-original-file origin-file)
          (setq roam-block-ref-uuid uuid)
          (setq roam-block-ref-content content)
          (roam-block-ref-edit-mode)
          (switch-to-buffer roam-block-ref-edit-buf))
      (message "No block needs to be edited!"))))

(provide 'roam-block-ref)
;;; roam-block-ref.el ends here
