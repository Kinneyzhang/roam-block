;;; roam-block-ref.el --- Block ref functions -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: block roam convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/roam-block
;; Package-Requires: ((emacs "26.1") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (ov "1.0.6") (promise "1.1"))

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

(require 'roam-block-db)

;;;; Variables

(defvar roam-block-ref-re "((\\([-a-z0-9]\\{36\\}\\)))"
  "Regular expression that matches a `roam-block-link'.")

(defvar roam-block-ref-buf "*Roam Block Ref*"
  "Name of the block references buffer.")

(defvar roam-block-ref-edit-buf "*Roam Block Edit*"
  "Name of buffer for editing blocks.")

(defvar roam-block-ref-highlight nil
  "Non-nil means to highlight the refered block display and
distinguish it with the original block.")

(defvar roam-block-ref-face '(:underline (:color "#222"))
  "Faces to highlight roam block ref.")

(defvar roam-block-ref-stored nil
  "Roam block ref that have stored.")

(defvar roam-block-contents nil
  "A list of all block contents")

;;;; Functions

(define-button-type 'roam-block-ref
  'action #'roam-block-follow-ref
  'face nil
  'content nil
  'follow-link nil
  'mouse-face nil
  'help-echo "Jump to this block.")

(defun roam-block-follow-ref (button)
  "Jump to block references buffer after follow roam-block ref link."
  (with-demoted-errors "Error when following the link: %s"
    (let* ((content (button-get button 'content))
           (uuid (string-trim (button-label button) "((" "))"))
           (data (roam-block-db--linked-ref-data uuid))
           (num (length data))
           (groups (seq-group-by #'car data)))
      (with-current-buffer (get-buffer-create roam-block-ref-buf)
        ;; Should set the buffer mode to the
        ;; origin uuid block file's major mode.
        (let ((inhibit-read-only t))
          (erase-buffer))
        (org-mode)
        (insert (propertize content 'font-lock-face
                            '((:height 1.1))) "\n\n")
        (insert (format "* %d Block References\n\n" num))
        (dolist (group groups)
          (let ((file (car group))
                (items (cdr group)))
            (if-let ((title (roam-block--org-title file)))
                (insert (format "** [[file:%s][%s]]\n\n" file title))
              (insert (format "** [[file:%s][%s]]\n\n" file file)))
            (dolist (item items)
              (let ((content (nth 1 item))
                    (uuid (nth 2 item)))
                (insert (format "%s\n\n" content))))))
        (indent-region (point-min) (point-max))
        (goto-char (point-min))
        (roam-block-ref-fontify (point-min) (point-max))
        (roam-block--buffer-setting)
        (setq-local header-line-format "View Buffer: Press 'q' to quit."))
      (view-buffer roam-block-ref-buf))))

(defun roam-block-ref--inherit-display (content)
  "Inherit all the block refs' display in the CONTENT."
  (let ((start 0))
    (while (and content
                (string-match roam-block-ref-re content start))
      (let* ((data (match-data))
             (beg (match-beginning 0))
             (uuid (match-string 1 content))
             (display (unwind-protect
                          (roam-block-db--block-content uuid)
                        (set-match-data data))))
        (setq content (replace-match display t t content))
        (setq start beg)))
    content))

(defun roam-block-ref--org-link-preserve (content)
  "Preserve the org-link face for org links in CONTENT."
  (let ((start 0))
    (while (string-match (org-link-make-regexps) content start)
      (let ((beg (match-beginning 0)))
        ;; org-link-make-regexps also match the pure link.
        ;; here we only expect org link.
        (if (string= (substring content beg (1+ beg)) "[")
            (let* ((display (or (match-string 3 content)
                                (match-string 2 content)))
                   (propertized-display
                    (propertize display 'face 'org-link))
                   (len (length propertized-display))
                   (end (+ len beg)))
              (setq content (replace-match propertized-display t nil content))
              (setq start end))
          (setq start (match-end 0)))))
    content))

(defun roam-block-ref--highlight-display (content)
  "Return the highlighted CONTENT with face `roam-block-ref-face'
except the region with org-link face."
  (if (text-property-any 0 (length content)
                         'face 'org-link content)
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (let (match)
          (while (setq match (text-property-search-forward
                              'face 'org-link nil))
            (add-text-properties
             (prop-match-beginning match)
             (prop-match-end match)
             `(face ,roam-block-ref-face)))
          (buffer-string)))
    (propertize content 'face roam-block-ref-face)))

(defun roam-block-ref-fontify (beg end)
  "Highlight roam-block ref between BEG and END."
  (when (roam-block-work-on)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward roam-block-ref-re end t)
        (let* ((uuid (match-string-no-properties 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (db-content (roam-block-db--block-content uuid))
               (content
                (when db-content
                  (roam-block-ref--org-link-preserve
                   (roam-block-ref--inherit-display db-content))))
               (propertized-content
                (when content
                  (if roam-block-ref-highlight
                      (concat (propertize " " 'face '(:height 0.9)
                                          'display '(raise 0.1))
                              (roam-block-ref--highlight-display content))
                    content))))
          (if content
              (with-silent-modifications
                (add-text-properties
                 beg end `(display ,propertized-content read-only t))
                (make-text-button
                 beg end :type 'roam-block-ref 'content content))
            (with-silent-modifications
              (remove-text-properties
               beg end '(display nil read-only nil face nil)))))))))

;; (defun roam-block-ref-fontify-at-real-time (uuid content)
;;   "Change the display property of UUID block ref to CONTENT at real time
;; when the original block or embed blocks change."
;;   (promise-then
;;    (roam-block-db--block-content-promise uuid)
;;    (lambda (db-content)
;;      (unless (string= content db-content)
;;        (promise-then
;;         (roam-block-db--embed-id uuid)
;;         (lambda (embed-id)
;;           (promise-then
;;            (roam-block-db--embed-block-uuid embed-id)
;;            (lambda (uuids)
;;              ;; (message "uuids: %S" uuids)
;;              ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;              ;; Strange: block with embed blocks and block without!!
;;              ;; embed blocks share different UUIDS value!         !!
;;              ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;              (dolist (win (window-list))
;;                ;; It's better to loop in window list than files list
;;                ;; because windows in window list are less than files
;;                ;; with the same UUID block ref usually.
;;                (when-let* ((buf (window-buffer win))
;;                            (file (buffer-file-name buf))
;;                            (_ (roam-block-work-home file))
;;                            (uuids (mapcar #'cadr uuids)))
;;                  (with-current-buffer buf
;;                    ;; (message "buf, uuids: %S, %S" buf uuids)
;;                    (dolist (uuid uuids)
;;                      (let ((ref (format "((%s))" uuid)))
;;                        (save-excursion
;;                          (goto-char (point-min))
;;                          (while (search-forward ref nil t)
;;                            (let ((inhibit-read-only t)
;;                                  (beg (match-beginning 0))
;;                                  (end (match-end 0)))
;;                              (with-silent-modifications
;;                                (add-text-properties
;;                                 beg end `(display ,content)))))))))))))))))))

(defun roam-block-ref-fontify-all (&optional uuid)
  "Highlight roam-block ref in all current frame's windows.
If UUID is non-nil, highlight all refs with this uuid."
  ;; fontify current buffer
  (roam-block-ref-fontify (point-min) (point-max))
  ;; fontify all displayed windows
  (dolist (win (window-list))
    (when-let* ((buf (window-buffer win))
                (file (buffer-file-name buf))
                (_ (roam-block-work-home file)))
      (with-current-buffer buf
        (roam-block-ref-fontify (point-min) (point-max))))))

(defun roam-block-ref-remove-properties ()
  "Remove text properties on all block refs."
  (dolist (win (window-list))
    (when-let* ((file (buffer-file-name (window-buffer win)))
                (_ (roam-block-work-home file)))
      (save-selected-window
        (select-window win)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward roam-block-ref-re nil t)
            (with-silent-modifications
              (let ((inhibit-read-only t))
                (remove-text-properties
                 (match-beginning 0) (match-end 0)
                 '(display nil read-only nil))))))))))

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
          (if-let* ((data (roam-block--block-uuid))
                    (uuid (nth 0 data))
                    (block-end (nth 2 data)))
              (progn
                (setq ref-str (concat ref-str (format "((%s))" uuid) "\n"))
                (goto-char block-end)
                (forward-line))
            (setq ref-str (concat ref-str "\n"
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
            (forward-line)))
        (setq roam-block-ref-stored ref-str))))
   (t (save-excursion
        (let ((uuid (get-char-property (point) 'uuid)))
          (if uuid
              (setq roam-block-ref-stored (format "((%s))" uuid))
            (setq uuid (get-char-property (line-beginning-position) 'uuid))
            (if uuid
                (setq roam-block-ref-stored (format "((%s))" uuid))
              (user-error "(roam-block) No valid block here!")))))))
  (message "(roam-block) The block refs are stored."))

;;;###autoload
(defun roam-block-ref-insert ()
  "Insert the stored roam-block ref at point."
  (interactive)
  (let ((beg (point)) end)
    (insert roam-block-ref-stored)
    (setq end (point))
    (roam-block-ref-fontify beg end)))

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

;;;###autoload
(defun roam-block-ref-highlight-toggle ()
  "Determine whether to highlight block refs."
  (interactive)
  (if roam-block-ref-highlight
      (setq roam-block-ref-highlight nil)
    (setq roam-block-ref-highlight t))
  (roam-block-ref-fontify-all)
  (if roam-block-ref-highlight
      (message "(roam-block) Show block refs highlight")
    (message "(roam-block) Hide block refs highlight")))

;; completion

(defun roam-block-blocks-contents ()
  (let ((promise (roam-block-db--all-contents)))
    (promise-then
     promise
     (lambda (data)
       (setq roam-block-contents (mapcar #'car data))))
    roam-block-contents))

(defun roam-block-ref-completion-exit-function (string status)
  "Function to run after completion is performed.
STRING is the text to which the field was completed, 
and STATUS indicates what kind of operation happened:

‘finished’ - text is now complete
‘sole’     - text cannot be further completed but
             completion is not finished
‘exact’    - text is a valid completion but may be further
             completed."
  (promise-chain (roam-block-db--content-uuid string)
    (then
     (lambda (uuid)
       (when (eq status 'finished)
         ;; (message "uuid:%s" uuid)
         ;; FIXME: 'roam-block-db--content-uuid' cannot  get the
         ;; value uuid stablely. It may be a bug of emacsql!
         (save-excursion
           (when (search-backward string (line-beginning-position) t)
             (replace-match uuid t))))))
    (promise-catch (lambda (reason)
                     (message reason)))))

;;;###autoload
(defun roam-block-ref-completion-at-point ()
  "Function to complete block ref at point."
  (interactive)
  (when (roam-block-work-home)
    (when (and (save-excursion
                 (re-search-backward
                  "(("  (line-beginning-position) t))
               (save-excursion
                 (re-search-forward
                  "))" (line-end-position) t)))
      (let (beg end)
        (save-excursion
          (goto-char (line-beginning-position))
          (when (re-search-forward "((\\(.+\\)))" (line-end-position) t)
            (setq beg (match-beginning 1))
            (setq end (match-end 1))))
        `(,beg ,end ,(roam-block-blocks-contents)
               :exit-function
               ,#'roam-block-ref-completion-exit-function)))))

;; Edit block

;; (defvar-local roam-block-ref-uuid nil
;;   "The uuid of block in edit buffer.")

;; (defvar-local roam-block-ref-content nil
;;   "The origin content of block in edite buffer.")

;; (defvar-local roam-block-ref-in-file nil
;;   "The file that the uuid link belongs to.")

;; (defvar-local roam-block-ref-original-file nil
;;   "The origin file of the edited uuid block.")

;; (define-minor-mode roam-block-ref-edit-mode
;;   "Minor mode for editing a refered read only block."
;;   :lighter ""
;;   :keymap (let ((map (make-sparse-keymap)))
;;             (define-key map (kbd "C-c C-k") #'roam-block-ref--edit-abort)
;;             (define-key map (kbd "C-c C-c") #'roam-block-ref--edit-finalize)
;;             map)
;;   :require 'roam-block
;;   (if roam-block-ref-edit-mode
;;       (setq-local header-line-format
;;                   (substitute-command-keys
;;                    "\\<roam-block-ref-edit-mode-map>Edit block: `\\[roam-block-ref--edit-finalize]' \
;; to finish, `\\[roam-block-ref--edit-abort]' to abort."))
;;     (setq-local header-line-format nil))
;;   (setq truncate-lines nil))

;; (defun roam-block-ref--edit-abort ()
;;   "Abort editing the read only block content."
;;   (interactive)
;;   (switch-to-buffer (get-file-buffer roam-block-ref-in-file))
;;   (kill-buffer roam-block-ref-edit-buf))

;; (defun roam-block-ref--edit-finalize ()
;;   "Finish editing the read only block content."
;;   (interactive)
;;   (let* ((file roam-block-ref-in-file)
;;          (origin-file roam-block-ref-original-file)
;;          (uuid roam-block-ref-uuid)
;;          (ref (format "((%s))" uuid))
;;          (origin-content roam-block-ref-content)
;;          (content (buffer-substring-no-properties (point-min) (point-max))))
;;     (with-current-buffer (find-file-noselect origin-file)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (catch 'break
;;           (while (search-forward origin-content nil t)
;;             (when (string= uuid (get-char-property
;;                                  (line-beginning-position) 'uuid))
;;               (let ((inhibit-read-only t))
;;                 (replace-match content t))
;;               (let ((beg (match-beginning 0))
;;                     (len (length content)))
;;                 (roam-block-overlay-block beg (+ beg len) uuid))
;;               (save-buffer)
;;               (throw 'break nil))))))
;;     (switch-to-buffer (get-file-buffer file))
;;     (kill-buffer roam-block-ref-edit-buf)))

;; ;;;###autoload
;; (defun roam-block-ref-edit ()
;;   "Edit the content of the ref block."
;;   (interactive)
;;   (let* ((file (buffer-file-name))
;;          (uuid (car (roam-block--ref-uuid)))
;;          (origin-file (roam-block-db--block-file uuid))
;;          (content (roam-block-db--block-content uuid))
;;          (mode major-mode))
;;     (if uuid
;;         (with-current-buffer (get-buffer-create roam-block-ref-edit-buf)
;;           (erase-buffer)
;;           (insert content)
;;           (funcall mode)
;;           (setq roam-block-ref-in-file file)
;;           (setq roam-block-ref-original-file origin-file)
;;           (setq roam-block-ref-uuid uuid)
;;           (setq roam-block-ref-content content)
;;           (roam-block-ref-edit-mode)
;;           (switch-to-buffer roam-block-ref-edit-buf))
;;       (message "(roam-block) No block ref here!"))))

(provide 'roam-block-ref)
;;; roam-block-ref.el ends here
