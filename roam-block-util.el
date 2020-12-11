;;; roam-block-util.el --- Utility functions -*- lexical-binding: t; -*-

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

;; This Module implements utility functions used throughout roam-block.

;;; Code:

;;;; Requires

(require 'cl-lib)
(require 'org-id)
(require 'seq)

;;;; Declarations

(defvar roam-block-skip-start-re)
(defvar roam-block-block-re)
(defvar roam-block-ovs)
(defvar roam-block-home)

;;;; Utility Functions

(defun roam-block--get-uuid ()
  "Format a downcase uuid."
  (string-join (split-string (org-id-uuid) "-") ""))

(defun roam-block--narrow-to-content ()
  "Narrow region to file contents."
  (save-excursion
    (let ((content-beg (point-min))
          (content-end (point-max)))
      (goto-char (point-min))
      (when (re-search-forward roam-block-skip-start-re nil t)
        (setq content-beg (point)))
      (narrow-to-region content-beg content-end))))

(defun roam-block--block-uuid ()
  "Return the uuid of current block."
  (get-char-property (line-beginning-position) 'uuid))

(defun roam-block--block-content ()
  "Return the content of current block."
  (let* ((ov (car (overlays-at (line-beginning-position))))
         (ov-beg (overlay-start ov))
         (ov-end (overlay-end ov)))
    (buffer-substring-no-properties ov-beg ov-end)))

(defun roam-block--ref-uuid ()
  "Return the uuid from block ref according to different postions of cursor."
  (cond
   ((button-at (point))
    (string-trim (button-label (button-at (point))) "((" "))"))
   ((save-excursion
      (re-search-backward roam-block-link-re (line-beginning-position) t))
    (match-string-no-properties 1))
   ((save-excursion
      (re-search-forward roam-block-link-re (line-end-position) t))
    (match-string-no-properties 1))))

(defun roam-block--block-region ()
  "Return the region of a block that matches `roam-block-block-re'."
  (save-excursion
    (re-search-forward roam-block-block-re nil t)
    (cons (match-beginning 0) (match-end 0))))

(defun roam-block--block-string ()
  "Return the string of a block that matches `roam-block-block-re'."
  (save-excursion
    (re-search-forward roam-block-block-re nil t)
    (match-string-no-properties 0)))

(defun roam-block--ovs-region ()
  "Return a list of (beg . end) cons of all roam-block overlays."
  (reverse
   (delete-dups
    (mapcar (lambda (ov)
              (cons (overlay-start ov)
                    (overlay-end ov)))
            roam-block-ovs))))

(defun roam-block-check-home ()
  "Check the value of `roam-block-home' variable.
If the value is nil, throw a user error message.
If the file or directory in the variable doesn't exist, create it.
Return the value of 'roam-block-home'."
  (if-let ((home roam-block-home))
      (progn
        (dolist (file home)
          (let ((file (expand-file-name file)))
            (unless (file-exists-p file)
              (if (directory-name-p file)
                  (make-directory file t)
                ;; create file if it's not a directory.
                (with-current-buffer (find-file-noselect file)
                  (save-buffer))))))
        home)
    (user-error "Please set `roam-block-home' variable properly!")))

(defun roam-block-work-on (&optional file)
  "Return the home directory or file in `roam-block-home' 
that FILE belongs to.  If FILE is nil, use current buffer file."
  (let ((file (or file (buffer-file-name)))
        (home (roam-block-check-home))
        in-home)
    (when file
      (if-let ((match (cl-member file home :test #'file-equal-p)))
          (setq in-home (car match))
        ;; file belongs to directory/sub-directory in home.
        (catch 'found
          (dolist (home-file home)
            (when (file-directory-p home-file)
              (when (file-in-directory-p file home-file)
                (setq in-home home-file)
                (throw 'found nil))))))
      (when in-home
        (expand-file-name in-home)))))

(defun roam-block--duplicate-uuids (seq)
  "Return the list of duplicate elements in sequence SEQ."
  (let ((seq (seq-sort #'string< seq))
        (index "")
        res)
    (mapcar (lambda (elem)
              (if (string= index elem)
                  (unless (member index res)
                    (push index res))
                (setq index elem)))
            seq)
    res))

(provide 'roam-block-util)
;;; roam-block-util.el ends here

