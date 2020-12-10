;;; roam-block-db.el --- Database API for roam-block functions -*- lexical-binding: t; -*-

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

;; This Module implements some database API for roam-block functions.

;;; Code:

;;;; Requires

(require 'seq)
(require 'emacsql)
(require 'emacsql-sqlite3)

(require 'roam-block-util)

;;;; Variables

(defcustom roam-block-db-location
  (expand-file-name "roam-block.db" (concat user-emacs-directory "roam-block"))
  "Database of roam-block."
  :type 'string
  :group 'roam-block)

(defconst roam-block-db--table-schemata
  '((files
     [(path :primary-key)
      (ovs)])
    (blocks
     [(uuid :primary-key)
      (content :not-null)
      (file :not-null)]
     (:foreign-key [file] :references files [path] :on-delete :cascade)))
  "Table schemata of block-re-db.")

(defvar roam-block-db--connection (make-hash-table :test #'equal)
  "Database connection to roam-block database.")

;;;; Functions

(defun roam-block-db--get-connection (path)
  "Return the roam-block database connection with key PATH."
  (gethash path roam-block-db--connection))

(defun roam-block-db--init (db)
  "Initialize database DB with the `roam-block-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) roam-block-db--table-schemata)
      (emacsql db `[:create-table ,table ,schema]))))

(defun roam-block-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection."
  (let* ((path (buffer-file-name))
         (home (roam-block-check-home))
         (curr-home (roam-block-work-on path)))
    (unless (and (roam-block-db--get-connection curr-home)
                 (emacsql-live-p (roam-block-db--get-connection curr-home)))
      (let ((init-db (not (file-exists-p roam-block-db-location))))
        (make-directory (file-name-directory roam-block-db-location) t)
        (let ((conn (emacsql-sqlite3 roam-block-db-location)))
          (set-process-query-on-exit-flag (emacsql-process conn) nil)
          (puthash curr-home conn roam-block-db--connection)
          (when init-db
            (roam-block-db--init conn)))))
    (roam-block-db--get-connection curr-home)))

;; when to close db connection?

(defun roam-block-db-query (sql &rest args)
  "Run SQL query on roam-block database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (roam-block-db) (apply #'format sql args))
    (apply #'emacsql (roam-block-db) sql args)))

(defun roam-block-db-clear ()
  "Clear all records in roam-block database."
  (interactive)
  (when (file-exists-p roam-block-db-location)
    (dolist (table (mapcar #'car roam-block-db--table-schemata))
      (roam-block-db-query `[:delete :from ,table]))))

(defun roam-block-db--block-content (uuid)
  "Return block content in database by UUID."
  (caar (roam-block-db-query `[:select content :from blocks :where (= uuid ,uuid)])))

;; Cache blocks

(defun roam-block-db--block-update (uuid)
  "Insert the block cache in database if there doesn't exist.
Update the block cache if there exists in database and has 
changes in file. Otherwise, do nothing."
  (let ((content (roam-block--block-string))
        (file (buffer-file-name)))
    (if-let ((cached-content
              (caar (roam-block-db-query
                     `[:select content :from blocks :where (= uuid ,uuid)]))))
        (progn
          ;; (message "cached-content: %s" cached-content)
          ;; (message "content: %s" content)
          (unless (string= cached-content content)
            (roam-block-db-query `[:update blocks :set (= content ,content)
                                           :where (= uuid ,uuid)])))
      (roam-block-db-query `[:insert :into blocks
                                     :values ([,uuid ,content ,file])]))))

(defun roam-block--has-overlay-caches ()
  "Judge if current buffer has overlay caches.
Return the overlay region (beg . end) list."
  (let ((file (buffer-file-name)))
    (caar (roam-block-db-query `[:select ovs :from files :where (= path ,file)]))))

(defun roam-block--has-file-caches ()
  "Judge if current buffer has file caches."
  (let ((file (buffer-file-name)))
    (roam-block-db-query `[:select * :from files :where (= path ,file)])))

(defun roam-block-db--init-files-table (file)
  "Initialize FILE in the 'files' table for safety of foreign key constrain."
  (roam-block-db-query `[:insert :into files :values [,file nil]]))

(defun roam-block-db--update-files-table (file)
  "Update overlays in the 'files' table with the FILE file."
  (roam-block-db-query `[:update files :set (= ovs ',(roam-block--ovs-region))
                                 :where (= path ,file)]))

(defun roam-block-db--delete-redundant-blocks (file uuid-lst)
  "Delete redundant caches in the FILE by comparing the uuid list in database
with UUID-LST list."
  (let* ((cached-uuid-lst
          (mapcar #'car (roam-block-db-query
                         `[:select uuid :from blocks :where (= file ,file)])))
         (redundant-uuid-lst (seq-filter (lambda (item)
                                           (not (member item uuid-lst)))
                                         cached-uuid-lst)))
    ;; (message "redundant: %S" (redundant-uuid-lst))
    (when redundant-uuid-lst
      (dolist (uuid redundant-uuid-lst)
        ;; delete overlay and pop in `roam-block-ovs'.!!!!!!!!!!!!!!!!!!!!
        (roam-block-db-query `[:delete :from blocks
                                       :where (= uuid ,uuid)])))))

(defun roam-block-db-cache-file ()
  "Store current buffer's blocks in database."
  (when (roam-block-work-on)
    (let ((file (buffer-file-name))
          uuid-lst)
      (unless (roam-block--has-file-caches)
        (roam-block-db--init-files-table file))
      (setq uuid-lst (roam-block-put-overlays file))
      (roam-block-db--update-files-table file)
      (roam-block-db--delete-redundant-blocks file uuid-lst))))

(provide 'roam-block-db)
;;; roam-block-db.el ends here
