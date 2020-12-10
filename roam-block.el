;;; roam-block.el --- An all-purpose block ref and block embed implement in emacs -*- lexical-binding: t; -*-

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

(require 'roam-block-db)

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

(defvar roam-block-link-re "((\\([a-z0-9]\\{32\\}\\)))"
  "Regular expression that matches a `roam-block-link'.")

(defvar-local roam-block-ovs nil
  "List of block overlays for each local buffer.")

(defvar roam-block-linum nil
  "The line number of a block.")

(defvar roam-block-block-re "^.+$")

(defvar roam-block-linked-buf "*Roam Block Linked*"
  "Name of linked references buffer.")

(defvar roam-block-edit-buf "*Roam Block Edit*"
  "Name of buffer for editing blocks.")

(defvar-local roam-block-edit-block-uuid nil
  "The uuid of block in edited block buffer.")

(defvar-local roam-block-edit-block-content nil
  "The origin content of block in edited block buffer.")

(defvar-local roam-block-edit-block-file nil
  "The file that the uuid link belongs to.")

(defvar-local roam-block-edit-block-origin-file nil
  "Origin file of the edited uuid block.")

;;;; Functions

(defun roam-block-insert-in-front (ov after &rest args)
  "Move overlays when insert in front of a OV overlay.
When AFTER is non-nil, call function after change,
otherwise call function before change. ARGS are the rest arguments."
  (if (null after)
      (setq roam-block-linum (line-number-at-pos))
    (unless (= roam-block-linum (line-number-at-pos))
      (move-overlay ov (line-beginning-position) (line-end-position)))))

(defun roam-block-insert-behind (ov after &rest args)
  "Move overlays when insert behind of a OV overlay.
When AFTER is non-nil, call function after change,
otherwise call function before change. ARGS are the rest arguments."
  (if (null after)
      (setq roam-block-linum (line-number-at-pos))
    (when (= roam-block-linum (line-number-at-pos))
      (move-overlay ov (line-beginning-position) (line-end-position)))))

(defun roam-block-overlay-block (beg end &optional uuid)
  "Put overlays between BEG and END. If UUID is non-nil, 
use it as the value of uuid overlay."
  (let ((uuid (or uuid (roam-block--get-uuid)))
        (ov (make-overlay beg end)))
    (push ov roam-block-ovs)
    (overlay-put ov 'uuid uuid)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'insert-behind-hooks '(roam-block-insert-behind))
    (overlay-put ov 'insert-in-front-hooks '(roam-block-insert-in-front))
    uuid))

(defun roam-block-restore-overlays ()
  "Restore overlays from database in the file."
  (when (roam-block-work-on)
    (when-let ((ovs (roam-block--has-overlay-caches)))
      (dolist (ov ovs)
        (when-let* ((beg (car ov))
                    (end (cdr ov))
                    (content (buffer-substring-no-properties beg end))
                    (uuid (caar (roam-block-db-query
                                 `[:select uuid :from blocks
                                           :where (= content ,content)]))))
          (roam-block-overlay-block beg end uuid))))))

(defun roam-block-put-overlays (file)
  "Put all block overlays in FILE and cache them in database.
Return a list of uuid used for deleting redundant block records."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (roam-block--narrow-to-content)
        (goto-char (point-min))
        (let ((file (buffer-file-name))
              (forward-line-flag 0)
              uuid-lst)
          (catch 'break
            (while (point)
              ;; FIXME: Use diff to compare the twice file save,
              ;; only update those changed blocks.
              (cond
               ((looking-at "^*+ .+?")
                (setq forward-line-flag (forward-line))
                (when (looking-at "^ *:PROPERTIES:\n")
                  (re-search-forward
                   "^ *:PROPERTIES:\n\\( *:.+?:.*\n\\)+ *:END:\n" nil t)))
               ((looking-at "^ *$")
                (setq forward-line-flag (forward-line)))
               (t (let ((uuid (get-char-property (point) 'uuid)))
                    ;; If current block doesn't have a uuid overlay,
                    ;; add it and cache it in database.
                    ;; If current block has a uuid overlay, query the database.
                    ;; If the database contains current uuid,
                    ;; check if the content and linum have changed, if changed,
                    ;; update them in database. Otherwise, do nothing.
                    (unless uuid
                      (let ((beg (car (roam-block--block-region)))
                            (end (cdr (roam-block--block-region))))
                        ;; FIXME: use regexp to match a block region for
                        ;; different situations in different types of files.
                        (setq uuid (roam-block-overlay-block beg end))))
                    (roam-block-db--block-update uuid)
                    (push uuid uuid-lst)
                    (setq forward-line-flag (forward-line)))))
              (when (= forward-line-flag 1)
                (throw 'break uuid-lst))))
          uuid-lst)))))

;; roam-block link

(define-button-type 'roam-block-link
  'action #'roam-block-follow-link
  'face nil
  'content nil
  'follow-link nil
  'help-echo "Jump to this block.")

(defun roam-block-follow-link (btn)
  "Jump to block references buffer after follow roam-block link."
  (with-demoted-errors "Error when following the link: %s"
    (let ((content (button-get btn 'content)))
      (with-current-buffer (get-buffer-create roam-block-linked-buf)
        ;; Should set the buffer mode to the
        ;; origin uuid block file's major mode.
        (erase-buffer)
        (insert (propertize content 'face '(:height 1.2)))
        (setq-local header-line-format "View Buffer: Press 'q' to quit."))
      (view-buffer roam-block-linked-buf))))

(defun roam-block-link-fontify (beg end)
  "Highlight roam-block link between BEG and END."
  (when (roam-block-work-on)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward roam-block-link-re end t)
        (let* ((uuid (match-string-no-properties 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (content (caar (roam-block-db-query
                               `[:select content :from blocks
                                         :where (= uuid ,uuid)]))))
          (if content
              (with-silent-modifications
                (add-text-properties beg end `(display ,content read-only t))
                (make-text-button beg end :type 'roam-block-link
                                  'content content))
            (with-silent-modifications
              (remove-text-properties beg end '(display nil read-only nil)))))))))

(defun roam-block-fontify-link ()
  "Highlight roam-block link in all current frame's windows."
  ;; fontify current buffer
  (roam-block-link-fontify (point-min) (point-max))
  ;; fontify all displayed windows
  (let ((wins (window-list)))
    (save-selected-window
      (dolist (win wins)
        (select-window win)
        (when (roam-block-work-on)
          (roam-block-link-fontify (point-min) (point-max)))))))

;; Edit block

(define-minor-mode roam-block-edit-mode
  "Minor mode for editing a refered read only block."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-k") #'roam-block--edit-abort)
            (define-key map (kbd "C-c C-c") #'roam-block--edit-finalize)
            map)
  :require 'roam-block
  (if roam-block-edit-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   "\\<roam-block-edit-mode-map>Edit block: `\\[roam-block--edit-finalize]' \
to finish, `\\[roam-block--edit-abort]' to abort."))
    (setq-local header-line-format nil))
  (setq truncate-lines nil))

(defun roam-block--edit-abort ()
  "Abort editing the read only block content."
  (interactive)
  (switch-to-buffer (get-file-buffer roam-block-edit-block-file))
  (kill-buffer roam-block-edit-buf))

(defun roam-block--edit-finalize ()
  "Finish editing the read only block content."
  (interactive)
  (let ((file roam-block-edit-block-file)
        (origin-file roam-block-edit-block-origin-file)
        (uuid roam-block-edit-block-uuid)
        (origin-content roam-block-edit-block-content)
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
      (roam-block-link-fontify (match-beginning 0) (match-end 0)))
    (kill-buffer roam-block-edit-buf)))

(defun roam-block--get-block-uuid ()
  "Get the uuid from block according to different postions of cursor."
  (cond
   ((button-at (point))
    (string-trim (button-label (button-at (point))) "((" "))"))
   ((save-excursion
      (re-search-backward roam-block-link-re (line-beginning-position) t))
    (match-string-no-properties 1))
   ((save-excursion
      (re-search-forward roam-block-link-re (line-end-position) t))
    (match-string-no-properties 1))))

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
         (uuid (roam-block--get-block-uuid))
         (origin-file
          (caar (roam-block-db-query
                 `[:select file :from blocks :where (= uuid ,uuid)])))
         (content (roam-block-db--block-content uuid))
         (mode major-mode))
    (if uuid
        (with-current-buffer (get-buffer-create roam-block-edit-buf)
          (insert content)
          (setq major-mode mode)
          (setq roam-block-edit-block-file file)
          (setq roam-block-edit-block-origin-file origin-file)
          (setq roam-block-edit-block-uuid uuid)
          (setq roam-block-edit-block-content content)
          (roam-block-edit-mode)
          (switch-to-buffer roam-block-edit-buf))
      (message "No block needs to be edited!"))))

;; Hook functions

(defun roam-block--buffer-setting ()
  "Some settings for roam-block buffer."
  (setq-local show-paren-mode nil))

(defun roam-block--find-file-hook-function ()
  "Roam-block function binded to `find-file-hook'.
If there exists caches of the file, restore overlays of the file buffer.
If there doesn't exist caches of the file, put overlays for each block in
file and cache them database."
  (when (roam-block-work-on)
    (unless (roam-block-restore-overlays)
      (roam-block-db-cache-file))
    (roam-block-link-fontify (point-min) (point-max))
    (roam-block--buffer-setting)))

(defun roam-block--after-save-hook-function ()
  "Roam-block function binded to `after-save-hook'.
Update caches of those changed blocks and fontify block ref links."
  ;; FIXME: Cannot work properly in markdown-mode.
  ;; After the md buffer is saved, the display attribute
  ;; of text properties will lost.
  (when (roam-block-work-on)
    (roam-block-db-cache-file)
    (roam-block-fontify-link)
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
        (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                         (file-executable-p emacsql-sqlite3-executable))
                    (executable-find "sqlite3"))
          (lwarn '(roam-block) :error "Cannot find executable 'sqlite3'. \
Please make sure it is installed and can be found within `exec-path'."))
        (jit-lock-register #'roam-block-link-fontify)
        (add-hook 'find-file-hook #'roam-block--find-file-hook-function)
        (add-hook 'after-save-hook #'roam-block--after-save-hook-function))
    (jit-lock-unregister #'roam-block-link-fontify)
    (remove-hook 'find-file-hook #'roam-block--find-file-hook-function)
    (remove-hook 'after-save-hook #'roam-block--after-save-hook-function)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward roam-block-link-re nil t)
        (with-silent-modifications
          (let ((inhibit-read-only t))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display nil read-only nil)))))))
  (jit-lock-refontify))

(provide 'roam-block)
;;; roam-block.el ends here
