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
(require 'roam-block-ref)
;; (require 'roam-block-embed)

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

(defvar-local roam-block-ovs nil
  "List of block overlays for each local buffer.")

(defvar roam-block-linum nil
  "The line number of a block.")

(defvar roam-block-block-re "^.+$")

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

(defun roam-block-overlay-block (beg end &optional uuid prop value)
  "Put overlays between BEG and END. If UUID is non-nil, 
use it as the value of uuid overlay.  
If PROP and VALUE is non-nil, also put the overlay on this block."
  (let ((uuid (or uuid (roam-block--get-uuid)))
        (ov (make-overlay beg end)))
    (push ov roam-block-ovs)
    (overlay-put ov 'uuid uuid)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'insert-behind-hooks '(roam-block-insert-behind))
    (overlay-put ov 'insert-in-front-hooks '(roam-block-insert-in-front))
    (when (and prop value)
      (overlay-put ov prop value))
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
    (roam-block-ref-fontify (point-min) (point-max))
    (roam-block--buffer-setting)))

(defun roam-block--after-save-hook-function ()
  "Roam-block function binded to `after-save-hook'.
Update caches of those changed blocks and fontify block ref links."
  ;; FIXME: Cannot work properly in markdown-mode.
  ;; After the md buffer is saved, the display attribute
  ;; of text properties will lost.
  (when (roam-block-work-on)
    (roam-block-db-cache-file)
    ;; (roam-block-embed-sync)
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
        (unless (or (and (bound-and-true-p emacsql-sqlite3-executable)
                         (file-executable-p emacsql-sqlite3-executable))
                    (executable-find "sqlite3"))
          (lwarn '(roam-block) :error "Cannot find executable 'sqlite3'. \
Please make sure it is installed and can be found within `exec-path'."))
        (jit-lock-register #'roam-block-ref-fontify)
        (add-hook 'find-file-hook #'roam-block--find-file-hook-function)
        (add-hook 'after-save-hook #'roam-block--after-save-hook-function)
        (add-hook 'kill-emacs-hook #'roam-block-db--close-all))
    (jit-lock-unregister #'roam-block-ref-fontify)
    (remove-hook 'find-file-hook #'roam-block--find-file-hook-function)
    (remove-hook 'after-save-hook #'roam-block--after-save-hook-function)
    (remove-hook 'kill-emacs-hook #'roam-block-db--close-all)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward roam-block-ref-re nil t)
        (with-silent-modifications
          (let ((inhibit-read-only t))
            (remove-text-properties (match-beginning 0) (match-end 0)
                                    '(display nil read-only nil)))))))
  (jit-lock-refontify))

(provide 'roam-block)
;;; roam-block.el ends here
