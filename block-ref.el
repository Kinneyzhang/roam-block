;;; block-ref.el --- Implement of versatile block ref and embed -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/Kinneyzhang/block-ref
;; Package-Requires: ((emacs "26.1"))

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

;; 

;;; Code:

;;;; Requires

(require 'subr-x)
(require 'org-id)

(require 'block-ref-db)

;;;; Variables

(defgroup block-ref nil
  "Block references and block embed implement in file.")

(defcustom block-ref-directory "~/block-ref/"
  "Directories or files that `block-ref-mode' works on.
If the value is a string, block-ref-mode works on the single directory.
If the value is a list, block-ref-mode works on all the directories in list."
  :type '(string list)
  :group 'block-ref)

(defvar block-ref-skip-start-re "\\(^ *#+.+:.+\n+\\)+"
  "Regular expression that matches contents needed to be skipped
at the beginning of file, usually are some meta settings.")

(defvar block-ref-link-re "((\\([a-z0-9]\\{32\\}\\)))"
  "Regular expression that matches a `block-ref-link'.")

(defvar-local block-ref-ovs nil
  "List of block overlays for each local buffer.")

(defvar block-ref-linum nil
  "The line number of a block.")

(defvar block-ref-block-re "^.+$")

(defvar block-ref-edit-buf "*Block Ref Edit*"
  "Name of buffer for editing blocks.")

(defvar-local block-ref-edit-block-uuid nil
  "The uuid of block in edited block buffer.")

(defvar-local block-ref-edit-block-content nil
  "The origin content of block in edited block buffer.")

(defvar-local block-ref-edit-block-file nil
  "The file that the uuid link belongs to.")

(defvar-local block-ref-edit-block-origin-file nil
  "Origin file of the edited uuid block.")

;;;; Functions

(defun block-ref--get-uuid ()
  "Format a downcase uuid."
  (string-join (split-string (org-id-uuid) "-") ""))

(defun block-ref--narrow-to-content ()
  "Narrow region to file contents."
  (save-excursion
    (let ((content-beg (point-min))
          (content-end (point-max)))
      (goto-char (point-min))
      (when (re-search-forward block-ref-skip-start-re nil t)
        (setq content-beg (point)))
      (narrow-to-region content-beg content-end))))

(defun block-ref--block-region ()
  "Return the region of a block that matches `block-ref-block-re'."
  (save-excursion
    (re-search-forward block-ref-block-re nil t)
    (cons (match-beginning 0) (match-end 0))))

(defun block-ref--block-string ()
  "Return the string of a block that matches `block-ref-block-re'."
  (save-excursion
    (re-search-forward block-ref-block-re nil t)
    (match-string-no-properties 0)))

(defun block-ref--ovs-region ()
  "Return a list of (beg . end) cons of all block-ref overlays."
  (reverse
   (delete-dups
    (mapcar (lambda (ov)
              (cons (overlay-start ov)
                    (overlay-end ov)))
            block-ref-ovs))))

(defun block-ref-work-on ()
  "Judge if current buffer is at the block-ref work home."
  (when-let ((home block-ref-directory))
    (cond
     ((stringp home)
      (cond
       ((file-directory-p home)
        (member (buffer-file-name) (directory-files home 'full)))
       (t (user-error "The value of `block-ref-directory' is neither a valid file or directory!"))))
     ((listp home)
      (catch 'break
        (dolist (dir home)
          (cond
           ((file-directory-p dir)
            (when (member (buffer-file-name) (directory-files dir 'full))
              (throw 'break t)))
           (t (user-error "%s is not a valid value of `block-ref-directory'!" file))))))
     (t (user-error "The value of `block-ref-directory' is neither a string or a list!")))))

(defun block-ref-insert-in-front (ov after &rest args)
  "Move overlays when insert in front of a OV overlay.
When AFTER is non-nil, call function after change, otherwise call function before change. ARGS are the rest arguments."
  (if (null after)
      (setq block-ref-linum (line-number-at-pos))
    (unless (= block-ref-linum (line-number-at-pos))
      (move-overlay ov (line-beginning-position) (line-end-position)))))

(defun block-ref-insert-behind (ov after &rest args)
  "Move overlays when insert behind of a OV overlay.
When AFTER is non-nil, call function after change, otherwise call function before change. ARGS are the rest arguments."
  (if (null after)
      (setq block-ref-linum (line-number-at-pos))
    (when (= block-ref-linum (line-number-at-pos))
      (move-overlay ov (line-beginning-position) (line-end-position)))))

(defun block-ref-overlay-block (beg end &optional uuid)
  "Put overlays between BEG and END. If UUID is non-nil, 
use it as the value of uuid overlay."
  (let ((uuid (or uuid (block-ref--get-uuid)))
        (ov (make-overlay beg end)))
    (push ov block-ref-ovs)
    (overlay-put ov 'uuid uuid)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'insert-behind-hooks '(block-ref-insert-behind))
    (overlay-put ov 'insert-in-front-hooks '(block-ref-insert-in-front))
    uuid))

(defun block-ref-restore-overlays ()
  "Restore overlays from database in the file."
  (when (block-ref-work-on)
    (when-let ((ovs (block-ref--has-overlay-caches)))
      (dolist (ov ovs)
        (when-let* ((beg (car ov))
                    (end (cdr ov))
                    (content (buffer-substring-no-properties beg end))
                    (uuid (caar (block-ref-db-query
                                 `[:select uuid :from blocks
                                           :where (= content ,content)]))))
          (block-ref-overlay-block beg end uuid))))))

(defun block-ref-put-overlays (file)
  "Put all block overlays in FILE and cache them in database.
Return a list of uuid used for deleting redundant block records."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (save-restriction
        (block-ref--narrow-to-content)
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
                      (let ((beg (car (block-ref--block-region)))
                            (end (cdr (block-ref--block-region))))
                        ;; FIXME: use regexp to match a block region for
                        ;; different situations in different types of files.
                        (setq uuid (block-ref-overlay-block beg end))))
                    (block-ref-db--block-update uuid)
                    (push uuid uuid-lst)
                    (setq forward-line-flag (forward-line)))))
              (when (= forward-line-flag 1)
                (throw 'break uuid-lst))))
          uuid-lst)))))

;; Block-ref link

(define-button-type 'block-ref-link
  'action #'block-ref-follow-link
  'face nil
  'follow-link t
  'help-echo "Jump to this block.")

(defun block-ref-follow-link (btn)
  "Jump to block references buffer after follow block-ref link."
  (with-demoted-errors "Error when following the link: %s"
    (message "Successfully jump to block")))

(defun block-ref-link-fontify (beg end)
  "Highlight block-ref link between BEG and END."
  (when (block-ref-work-on)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward block-ref-link-re end t)
        (let* ((uuid (match-string-no-properties 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (content (caar (block-ref-db-query
                               `[:select content :from blocks
                                         :where (= uuid ,uuid)]))))
          (if content
              (with-silent-modifications
                ;;; dislpay a propertized content!!!!!!!!!!!!!
                (add-text-properties beg end `(display ,content read-only t))
                (make-text-button beg end :type 'block-ref-link))
            (with-silent-modifications
              (remove-text-properties beg end '(display nil)))))))))

(defun block-ref--fontify-link ()
  "Highlight block-ref link in all current frame's windows."
  (let ((wins (window-list)))
    (save-selected-window
      (dolist (win wins)
        (select-window win)
        (when (block-ref-work-on)
          (block-ref-link-fontify (point-min) (point-max)))))))

;; Edit block

(define-minor-mode block-ref-edit-mode
  "Minor mode for editing a refered read only block."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-k") #'block-ref--edit-abort)
            (define-key map (kbd "C-c C-c") #'block-ref--edit-finalize)
            map)
  :require 'block-ref
  (if block-ref-edit-mode
      (setq-local header-line-format
                  (substitute-command-keys
                   "\\<block-ref-edit-mode-map>Edit block: `\\[block-ref--edit-finalize]' \
to finish, `\\[block-ref--edit-abort]' to abort."))
    (setq-local header-line-format nil))
  (setq truncate-lines nil))

(defun block-ref--edit-abort ()
  "Abort editing the read only block content."
  (interactive)
  (switch-to-buffer (get-file-buffer block-ref-edit-block-file))
  (kill-buffer block-ref-edit-buf))

(defun block-ref--edit-finalize ()
  "Finish editing the read only block content."
  (interactive)
  (let ((file block-ref-edit-block-file)
        (origin-file block-ref-edit-block-origin-file)
        (uuid block-ref-edit-block-uuid)
        (origin-content block-ref-edit-block-content)
        (content (buffer-string)))
    (with-current-buffer (find-file-noselect origin-file)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward origin-content nil t)
          (when (string= uuid (get-char-property
                               (line-beginning-position) 'uuid))
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (insert content)
              (delete-region beg end)
              (save-buffer))))))
    (switch-to-buffer (get-file-buffer file))
    (kill-buffer block-ref-edit-buf)))

(defun block-ref--get-block-uuid ()
  "Get the uuid from block according to different postions of cursor."
  (cond
   ((button-at (point))
    (string-trim (button-label (button-at (point))) "((" "))"))
   ((save-excursion
      (re-search-backward block-ref-link-re (line-beginning-position) t))
    (match-string-no-properties 1))
   ((save-excursion
      (re-search-forward block-ref-link-re (line-end-position) t))
    (match-string-no-properties 1))))

;;;###autoload
(defun block-ref-copy-link ()
  "Save the block-ref link to kill-ring, use the block at point by default.
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
        (kill-new (format "((%s))" (get-char-property (point) 'uuid)))))))

;;;###autoload
(defun block-ref-edit-block ()
  "Edit the content of the read only block."
  (interactive)
  (let* ((file (buffer-file-name))
         (uuid (block-ref--get-block-uuid))
         (origin-file
          (caar (block-ref-db-query
                 `[:select file :from blocks :where (= uuid ,uuid)])))
         (content (block-ref-db--block-content uuid))
         (mode major-mode))
    (if uuid
        (with-current-buffer (get-buffer-create block-ref-edit-buf)
          (insert content)
          (setq major-mode mode)
          (setq block-ref-edit-block-file file)
          (setq block-ref-edit-block-origin-file origin-file)
          (setq block-ref-edit-block-uuid uuid)
          (setq block-ref-edit-block-content content)
          (block-ref-edit-mode)
          (switch-to-buffer block-ref-edit-buf))
      (message "No block needs to be edited!"))))

;; Hook functions

(defun block-ref--buffer-setting ()
  "Some settings for block-ref buffer."
  (setq-local show-paren-mode nil))

(defun block-ref--find-file-hook-function ()
  "Block-ref function binded to `find-file-hook'.
If there exists caches of the file, restore overlays of the file buffer.
If there doesn't exist caches of the file, put overlays for each block in
file and cache them database."
  (unless (block-ref-restore-overlays)
    (block-ref-db-cache-file))
  (block-ref-link-fontify (point-min) (point-max))
  (block-ref--buffer-setting))

(defun block-ref--after-save-hook-function ()
  "Block-ref function binded to `after-save-hook'.
Update caches of those changed blocks and fontify block ref links."
  (block-ref-db-cache-file)
  (block-ref--fontify-link))

;; Minor mode

(define-minor-mode block-ref-mode
  "Minor mode for block-ref."
  :lighter " ref"
  :keymap (let ((map (make-sparse-keymap))) map)
  :group 'block-ref
  :require 'block-ref
  :global t
  (if block-ref-mode
      (progn
        (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                         (file-executable-p emacsql-sqlite3-executable))
                    (executable-find "sqlite3"))
          (lwarn '(block-ref) :error "Cannot find executable 'sqlite3'. \
Please make sure it is installed and can be found within `exec-path'."))
        (add-hook 'find-file-hook #'block-ref--find-file-hook-function)
        (add-hook 'after-save-hook #'block-ref--after-save-hook-function)
        (jit-lock-register #'block-ref-link-fontify))
    (remove-hook 'find-file-hook #'block-ref--find-file-hook-function)
    (remove-hook 'after-save-hook #'block-ref--after-save-hook-function)
    (jit-lock-unregister #'block-ref-link-fontify)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward block-ref-link-re nil t)
        (with-silent-modifications
          (let ((inhibit-read-only t))
            (remove-text-properties (match-beginning 0)
                                    (match-end 0) '(display nil))
            (remove-text-properties (match-beginning 0)
                                    (match-end 0) '(read-only nil)))))))
  (jit-lock-refontify))

(provide 'block-ref)
;;; block-ref.el ends here
