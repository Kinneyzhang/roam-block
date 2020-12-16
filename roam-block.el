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

;;;; Functions

(defun roam-block-insert-in-front (beg end)
  "Function added to `insert-in-front-hooks'.
BEG is the start position of inserted text.
END is the end position of inserted text.
This functiton reset the uuid propery region to BEG and block end."
  (let* ((match (save-excursion (text-property-search-forward 'uuid)))
         (match-end (prop-match-end match))
         (uuid (prop-match-value match)))
    (let ((inhibit-read-only t))
      (add-text-properties (line-beginning-position) match-end
                           `(uuid ,uuid
                                  insert-in-front-hooks
                                  (roam-block-insert-in-front)
                                  insert-behind-hooks
                                  (roam-block-insert-behind))))))

(defun roam-block-insert-behind (beg end) 
  "Function added to `insert-behind-hooks'.
BEG is the start position of inserted text.
END is the end position of inserted text.
This functiton reset the uuid propery region to BEG and block end."
  (let* ((match (save-excursion (text-property-search-backward 'uuid)))
         (match-beg (prop-match-beginning match))
         (uuid (prop-match-value match))
         (inserted-str (buffer-substring-no-properties beg end)))
    (cond
     ((string= inserted-str "\n")
      (set-text-properties beg end nil))
     (t (let ((inhibit-read-only t))
          (add-text-properties match-beg (line-end-position)
                               `(uuid ,uuid
                                      insert-in-front-hooks
                                      (roam-block-insert-in-front)
                                      insert-behind-hooks
                                      (roam-block-insert-behind))))))))

(defun roam-block-propertize-block (beg end &optional uuid prop value)
  "Put uuid propery between BEG and END. If UUID is non-nil, 
use it as the value of uuid property.  
If PROP and VALUE is non-nil, also add the property on this block."
  (let ((uuid (or uuid (roam-block--get-uuid)))
        (inhibit-read-only t))
    (add-text-properties beg end `(uuid ,uuid
                                        insert-in-front-hooks
                                        (roam-block-insert-in-front)
                                        insert-behind-hooks
                                        (roam-block-insert-behind)))
    (when (and prop value)
      (add-text-properties beg end `(,prop ,value)))
    uuid))

(defun roam-block-restore-properties ()
  "Restore uuid properties from database in the file."
  (when (roam-block-work-home)
    (when-let ((regions (roam-block-db--have-regions)))
      (dolist (region regions)
        (when-let* ((beg (car region))
                    (end (cdr region))
                    (content (buffer-substring-no-properties beg end))
                    ;; FIXME: If the two blocks have the same content,
                    ;; it doesn't make sense that choose the first one.
                    (uuid (caar (roam-block-db-query
                                 `[:select uuid :from blocks
                                           :where (= content ,content)]))))
          (roam-block-propertize-block beg end uuid))))))

(defun roam-block-propertize-buffer ()
  "Put uuid property to the whole buffer's valid blocks 
according to the major mode MODE."
  (pcase major-mode
    ('org-mode (roam-block-propertize-org-buffer))))

(defun roam-block-propertize-org-buffer ()
  "Put uuid property to current org buffer's valid blocks."
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
               (let* ((uuid (get-char-property (point) 'uuid))
                      (beg (org-element-property :contents-begin elem))
                      (end (org-element-property :contents-end elem))
                      (block-end (1- end))
                      (match (save-excursion
                               (text-property-search-forward 'uuid)))
                      (match-end (prop-match-end match))
                      content)
                 ;; FIXME: consider the condition of the last line, do
                 ;; not need to minus 1 from end.
                 (if uuid
                     (unless (= match-end block-end)
                       ;; Make sure all lines in this paragraph are propertized
                       (roam-block-propertize-block beg block-end uuid))
                   (setq uuid (roam-block-propertize-block beg block-end)))
                 (setq content (buffer-substring-no-properties beg block-end))
                 (roam-block-db--block-update uuid content)
                 (push uuid uuid-lst)
                 (goto-char end)))
              ('plain-list
               (let* ((uuid (get-char-property (point) 'uuid))
                      (structure (org-element-property :structure elem))
                      (tree (car structure))
                      (beg (car tree))
                      (end (car (last tree)))
                      (block-end (1- end))
                      (match (save-excursion
                               (text-property-search-forward 'uuid)))
                      (match-end (prop-match-end match))
                      content)
                 (if uuid
                     (unless (= match-end block-end)
                       ;; Make sure all lines in this paragraph are propertized
                       (roam-block-propertize-block beg block-end uuid))
                   (setq uuid (roam-block-propertize-block beg block-end)))
                 (setq content (buffer-substring-no-properties beg block-end))
                 (roam-block-db--block-update uuid content)
                 (push uuid uuid-lst)
                 (goto-char end)))
              (_ (let* ((uuid (get-char-property (point) 'uuid))
                        (beg (org-element-property :begin elem))
                        (end (org-element-property :end elem))
                        (content (string-trim-right
                                  (buffer-substring-no-properties beg end) "\n"))
                        (blank (org-element-property :post-blank elem))
                        (block-end (- end blank 1))
                        (match (save-excursion
                                 (text-property-search-forward 'uuid)))
                        (match-end (prop-match-end match))
                        content)
                   (if uuid
                       (unless (= match-end block-end)
                         ;; Make sure all lines in this paragraph are propertized
                         (roam-block-propertize-block beg block-end uuid))
                     (setq uuid (roam-block-propertize-block beg block-end)))
                   (setq content (buffer-substring-no-properties beg block-end))
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
If there exists caches of the file, restore properties of the file buffer.
If there doesn't exist caches of the file, add properties for each block in
file and cache them database."
  (when (roam-block-work-home)
    (unless (roam-block-restore-properties)
      (roam-block-db-cache-file))
    (roam-block-ref-fontify (point-min) (point-max))
    (roam-block--buffer-setting)))

(defun roam-block--after-save-hook-function ()
  "Roam-block function binded to `after-save-hook'.
Update caches of those changed blocks and fontify block ref links."
  ;; FIXME: Cannot work properly in markdown-mode.
  ;; After the md buffer is saved, the display attribute
  ;; of text properties will lost.
  (when (roam-block-work-home)
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
        (roam-block-db)
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
