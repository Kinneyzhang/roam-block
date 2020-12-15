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
  (expand-file-name "roam-block.db"
                    (concat user-emacs-directory "roam-block"))
  "Database of roam-block."
  :type 'string
  :group 'roam-block)

(defconst roam-block-db--table-schemata
  '((files
     [(path :primary-key)
      (regions)])
    (blocks
     [(uuid :not-null)
      (content :not-null)
      (type :not-null)
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

(defun roam-block-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for nil."
  (unless db
    (setq db (roam-block-db--get-connection nil)))
  (when (and db (emacsql-live-p db))
    (emacsql-close db)))

(defun roam-block-db--close-all ()
  "Closes all database connections made by roam-block"
  (dolist (conn (hash-table-values roam-block-db--connection))
    (roam-block-db--close conn))
  (let ((keys (hash-table-keys roam-block-db--connection)))
    (dolist (key keys)
      (remhash key roam-block-db--connection))))

(defun roam-block-db--block-content (uuid)
  "Return block content in database by UUID."
  (caar (roam-block-db-query `[:select content :from blocks
                                       :where (= uuid ,uuid)])))

(defun roam-block-db--block-file (uuid)
  "Return the file that blocks belongs to in database by UUID."
  (caar (roam-block-db-query `[:select file :from blocks
                                       :where (= uuid ,uuid)])))

;; Cache blocks

(defun roam-block-db--block-update (uuid content)
  "Insert the block cache in database if there doesn't exist.
Update the block cache if there exists in database and has
changes in file. Otherwise, do nothing."
  (let ((file (buffer-file-name)))
    (if-let ((cached-content
              (caar (roam-block-db-query
                     `[:select content :from blocks
                               :where (= uuid ,uuid)]))))
        (unless (string= cached-content content)
          (roam-block-db-query
           `[:update blocks :set (= content ,content)
                     :where (= uuid ,uuid)]))
      (roam-block-db-query `[:insert :into blocks
                                     :values ([,uuid ,content "origin" ,file])]))))

(defun roam-block-db--have-regions ()
  "Judge if current buffer has properties caches.
Return the uuid property region (beg . end) list."
  (let ((file (buffer-file-name)))
    (caar (roam-block-db-query `[:select regions :from files
                                         :where (= path ,file)]))))

(defun roam-block-db--have-file ()
  "Judge if current buffer has file caches."
  (let ((file (buffer-file-name)))
    (roam-block-db-query `[:select * :from files
                                   :where (= path ,file)])))

(defun roam-block-db--init-files-table ()
  "Initialize current buffer file in the 'files' table 
for safety of foreign key constrain."
  (let ((file (buffer-file-name)))
    (roam-block-db-query `[:insert :into files
                                   :values [,file nil]])))

(defun roam-block-db--update-block-region ()
  "Update all block's (beg . end) cons of current buffer in database."
  (save-excursion
    (goto-char (point-min))
    (let (region-lst match)
      (while (setq match (text-property-search-forward 'uuid))
        (push `(,(prop-match-beginning match) . ,(prop-match-end match)) region-lst))
      (setq region-lst (reverse region-lst))
      (when (roam-block-db--have-file)
        (roam-block-db-query `[:update files :set
                                       (= regions ',region-lst)
                                       :where (= path ,(buffer-file-name))])))))

(defun roam-block-db--delete-redundant-blocks (uuid-lst)
  "Delete redundant caches of current buffer file by comparing 
the uuid list in database with UUID-LST list."
  (let ((file (buffer-file-name)))
    (let* ((cached-uuid-lst
            (mapcar #'car (roam-block-db-query
                           `[:select uuid :from blocks
                                     :where (= file ,file)])))
           (redundant-uuid-lst
            (seq-filter (lambda (item)
                          (not (member item uuid-lst)))
                        cached-uuid-lst)))
      (when redundant-uuid-lst
        (dolist (uuid redundant-uuid-lst)
          (roam-block-db-query `[:delete :from blocks
                                         :where (= uuid ,uuid)]))))))

(defun roam-block-db-cache-file ()
  "Store current buffer's blocks in database."
  (when (roam-block-work-on)
    (let (uuid-lst)
      (unless (roam-block-db--have-file)
        (roam-block-db--init-files-table))
      (setq uuid-lst (roam-block-propertize-buffer))
      (roam-block-db--update-block-region)
      (roam-block-db--delete-redundant-blocks uuid-lst))))

(provide 'roam-block-db)
;;; roam-block-db.el ends here
