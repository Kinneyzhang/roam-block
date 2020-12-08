;;; block-ref-db.el --- Database API for block-ref functions -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: http://github.com/usrname/block-ref
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

(require 'cl-lib)
(require 'seq)
(require 'emacsql)
(require 'emacsql-sqlite3)

;;;; Variables

(defcustom block-ref-db-location
  (expand-file-name "block-ref.db" (concat user-emacs-directory "block-refs"))
  "Database of block-ref."
  :type 'string
  :group 'block-ref)

(defconst block-ref-db--table-schemata
  '((files
     [(path :primary-key)
      (ovs)])
    (blocks
     [(uuid :primary-key)
      (content :not-null)
      (file :not-null)]
     (:foreign-key [file] :references files [path] :on-delete :cascade)))
  "Table schemata of block-re-db.")

(defvar block-ref-db--connection (make-hash-table :test #'equal)
  "Database connection to Block-ref database.")

;;;; Functions

(defun block-ref-db--get-connection ()
  "Return the Block-ref database connection."
  (gethash (expand-file-name block-ref-directory) block-ref-db--connection))

(defun block-ref-db--init (db)
  "Initialize database DB with the `block-ref-db--table-schemata'."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) block-ref-db--table-schemata)
      (emacsql db `[:create-table ,table ,schema]))))

(defun block-ref-db ()
  "Entrypoint to the Org-roam sqlite database.
Initializes and stores the database, and the database connection."
  (unless (and (block-ref-db--get-connection)
               (emacsql-live-p (block-ref-db--get-connection)))
    (let ((init-db (not (file-exists-p block-ref-db-location))))
      (make-directory (file-name-directory block-ref-db-location) t)
      (let ((conn (emacsql-sqlite3 block-ref-db-location)))
        (set-process-query-on-exit-flag (emacsql-process conn) nil)
        (puthash (expand-file-name block-ref-directory) conn
                 block-ref-db--connection)
        (when init-db
          (block-ref-db--init conn)))))
  (block-ref-db--get-connection))

(defun block-ref-db-query (sql &rest args)
  "Run SQL query on Block-ref database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
      (emacsql (block-ref-db) (apply #'format sql args))
    (apply #'emacsql (block-ref-db) sql args)))

(defun block-ref-db-clear ()
  "Clear all records in Block-ref database."
  (interactive)
  (when (file-exists-p block-ref-db-location)
    (dolist (table (mapcar #'car block-ref-db--table-schemata))
      (block-ref-db-query `[:delete :from ,table]))))

(defun block-ref-db--block-content (uuid)
  "Return block content in database by UUID."
  (caar (block-ref-db-query `[:select content :from blocks :where (= uuid ,uuid)])))

;; Cache blocks

(defun block-ref-db--block-update (uuid)
  "Insert the block cache in database if there doesn't exist.
Update the block cache if there exists in database and has 
changes in file. Otherwise, do nothing."
  (let ((content (block-ref--block-string))
        (file (buffer-file-name)))
    (if-let ((cached-content
              (caar (block-ref-db-query
                     `[:select content :from blocks :where (= uuid ,uuid)]))))
        (progn
          ;; (message "cached-content: %s" cached-content)
          ;; (message "content: %s" content)
          (unless (string= cached-content content)
            (block-ref-db-query `[:update blocks :set (= content ,content)
                                          :where (= uuid ,uuid)])))
      (block-ref-db-query `[:insert :into blocks
                                    :values ([,uuid ,content ,file])]))))

(defun block-ref--has-overlay-caches ()
  "Judge if current buffer has overlay caches.
Return the overlay region (beg . end) list."
  (let ((file (buffer-file-name)))
    (caar (block-ref-db-query `[:select ovs :from files :where (= path ,file)]))))

(defun block-ref--has-file-caches ()
  "Judge if current buffer has file caches."
  (let ((file (buffer-file-name)))
    (block-ref-db-query `[:select * :from files :where (= path ,file)])))

(defun block-ref-db--init-files-table (file)
  "Initialize FILE in the 'files' table for safety of foreign key constrain."
  (block-ref-db-query `[:insert :into files :values [,file nil]]))

(defun block-ref-db--update-files-table (file)
  "Update overlays in the 'files' table with the FILE file."
  (block-ref-db-query `[:update files :set (= ovs ',(block-ref--ovs-region))
                                :where (= path ,file)]))

(defun block-ref-db--delete-redundant-blocks (file uuid-lst)
  "Delete redundant caches in the FILE by comparing the uuid list in database
with UUID-LST list."
  (let* ((cached-uuid-lst
          (mapcar #'car (block-ref-db-query
                         `[:select uuid :from blocks :where (= file ,file)])))
         (redundant-uuid-lst (seq-filter (lambda (item)
                                           (not (member item uuid-lst)))
                                         cached-uuid-lst)))
    ;; (message "redundant: %S" (redundant-uuid-lst))
    (when redundant-uuid-lst
      (dolist (uuid redundant-uuid-lst)
        ;; delete overlay and pop in `block-ref-ovs'.!!!!!!!!!!!!!!!!!!!!
        (block-ref-db-query `[:delete :from blocks
                                      :where (= uuid ,uuid)])))))

(defun block-ref-db-cache-file ()
  "Store current buffer's blocks in database."
  (when (block-ref-work-on)
    (let ((file (buffer-file-name))
          uuid-lst)
      (unless (block-ref--has-file-caches)
        (block-ref-db--init-files-table file))
      (setq uuid-lst (block-ref-put-overlays file))
      (block-ref-db--update-files-table file)
      (block-ref-db--delete-redundant-blocks file uuid-lst))))

(provide 'block-ref-db)
;;; block-ref-db.el ends here
