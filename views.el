;;; views.el --- Save/restore window configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL: http://github.com/jensecj/views.el
;; Keywords: views, workgroups, windows
;; Package-Version: 20190304
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (s "1.12.0") (f "0.20.0") (ht "2.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple functionality for saving and restoring window configurations.

;; See `views--collect-buffer-info' for what information is saved for each window.

;;; Code:

(require 'dash)
(require 'f)
(require 'ht)
(require 's)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;
;; persisting views ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar views-file (concat user-emacs-directory "views.el")
  "File used for saving views to disk.")

(defun views--save-views (views)
  "Save VIEWS to `views-file'."
  (unless (f-exists-p views-file)
    (f-touch views-file))
  (with-temp-file views-file
    (pp views (current-buffer))))

(defun views--load-views ()
  "Load saved views from `views-file'."
  (if (f-exists-p views-file)
      (with-temp-buffer
        (insert-file-contents views-file)
        (or (read (current-buffer)) (ht)))
    (message "views file '%s' not found" views-file)
    (ht)))

(defun views--add (name view)
  "Add VIEW with NAME to the list of saved views."
  (let ((views (views--load-views)))
    (ht-set views name view)
    (views--save-views views)))

(defun views--remove (name)
  "Remove a view by NAME, from the list of saved views."
  (let ((views (views--load-views)))
    (ht-remove views name)
    (views--save-views views)))

;;;;;;;;;;;;;;;
;; accessors ;;
;;;;;;;;;;;;;;;

(defun views--view-type (view)
  "Get the type of VIEW."
  (alist-get 'type view))

(defun views--view-path (view)
  "Get the path of VIEW."
  (alist-get 'path view))

(defun views--view-name (view)
  "Get the name of VIEW."
  (alist-get 'name view))

(defun views--view-point (view)
  "Get the location of point in VIEW."
  (alist-get 'point view))

;;;;;;;;;;;;;;;;;;;;;
;; info collection ;;
;;;;;;;;;;;;;;;;;;;;;

(defun views--collect-buffer-info (buf)
  "Collect information about WIN to save."
  (with-current-buffer (get-buffer buf)
    (cond
     ((buffer-file-name)
      `((type . file)
        (path . ,(buffer-file-name))
        (point . ,(point))))
     ((eq major-mode 'dired-mode)
      `((type . file)
        (path . ,default-directory)
        (point . ,(point))))
     ((eq major-mode 'term-mode)
      `((type . term)
        (name . ,(buffer-name))
        (path . ,default-directory)))
     (t
      `((type . unknown)
        (name . ,(buffer-name)))))))

(defun views--window-tree-buffers (wt)
  "Return all buffers open in the `window-tree' WT."
  (if (consp wt)
      (-flatten (-map #'views--window-tree-buffers (cddr wt)))
    (with-current-buffer (window-buffer wt) (current-buffer))))

(defun views--frame-buffers (frame)
  "Return all buffers open in FRAME."
  (views--window-tree-buffers (car (window-tree frame))))

(defun views--current-view ()
  "Get the view for the current window."
  (let* ((frame (selected-frame))
         (frameset (views--frameset frame))
         (buffers (views--frame-buffers frame))
         (collected (-map #'views--collect-buffer-info buffers)))
    (cons collected frameset)))

(defun views--frameset (frame)
  "Get the frameset configuration from FRAME."
  (let ((frameset (frameset-save (list frame)))
        (current-display (frame-parameter frame 'display)))

    ;; don't store frame properties, we only want to keep the buffers and their
    ;; sizes/locations
    (setf (caar (elt frameset 7)) `((display . ,current-display)))
    frameset))

;;;;;;;;;;;;;;;;;
;; restoration ;;
;;;;;;;;;;;;;;;;;

(defun views--restore-frameset (frameset)
  "Restore FRAMESET in the current frame."
  (frameset-restore frameset
                    ;; restore in the current frame
                    :reuse-frames (lambda (f) (eq f (selected-frame)))
                    ;; on the current display
                    :force-display t))

(defun views--restore-file (buf)
  "Restore file stored in VIEW."
  (let* ((path (views--view-path buf))
         (buffer (get-buffer path)))
    (if (not (file-exists-p path))
        (error "File %s does not exist" path))

    (unless buffer
      (with-current-buffer (find-file-noselect path)
        ;; restore point position if saved
        (when-let ((p (views--view-point buf)))
          (goto-char p))
        ))))

(defun views--make-term (name path)
  "Create a terminal buffer with NAME and working directory PATH."
  (let ((default-directory path)
        (term-buffer))
    (with-temp-buffer
      (cd path)
      (setq term-buffer (make-term name shell-file-name))
      (set-buffer term-buffer)
      (cond
       ;; use multi-term is available
       ((fboundp #'multi-term)
        (multi-term-internal)
        (setq multi-term-buffer-list (nconc multi-term-buffer-list (list term-buffer))))
       (t ;; otherwise use term.el
        (term-mode)
        (term-char-mode)))
      term-buffer)))

(defun views--restore-term (view)
  "Restore terminal stored in VIEW."
  (let* ((name (views--view-name view))
         (clean-name (s-chop-suffix "*" (s-chop-prefix "*" name)))
         (path (views--view-path view))
         (buffer (get-buffer name)))
    (cond
     ;; if the terminal buffer already exists, theres nothing to do
     (buffer buffer)
     (t ;; otherwise we need to create it
      (views--make-term clean-name path)))))

(defun views--restore-buffers (buffers)
  (dolist (b buffers)
    (when-let ((type (views--view-type b)))
      (cond
       ((eq type 'file) (views--restore-file b))
       ((eq type 'term) (views--restore-term b))
       ((eq type 'unknown)
        (message
         "Views.el: Unknown buffer type for '%s', skipping restoration."
         (alist-get 'name b)))))))

(defun views--set-view (name)
  "Change the current window-configuration to the view of NAME."
  (let* ((views (views--load-views))
         (view (ht-get views name)))
    (if view
        (let ((inhibit-message t))
          (delete-other-windows)
          (views--restore-buffers (car view))
          (views--restore-frameset (cdr view)))
      (message "view '%s' not found" name))))

;;;;;;;;;;;;;;;
;; interface ;;
;;;;;;;;;;;;;;;

;; ###autoload
(defun views-push (&optional force)
  "Save the current window view.
Given a prefix-argument FORCE, overwrite a name if it is already in
use."
  (interactive "P")
  (let* ((views (ht-keys (views--load-views)))
         (name (completing-read "View name: " views))
         (view (views--current-view)))
    (if (and (not force) (member name views))
        (message "that name is already in use")
      (views--add name view))))

;;;###autoload
(defun views-pop ()
  "Remove a view from saved views."
  (interactive)
  (let* ((views (ht-keys (views--load-views)))
         (name (completing-read "Pick view: " views nil t)))
    (views--remove name)))

;;;###autoload
(defun views-switch ()
  "Switch to a saved view."
  (interactive)
  (let* ((views (ht-keys (views--load-views)))
         (view (completing-read "Pick view: " views nil t)))
    (when view
      (views--set-view view))))

(provide 'views)
;;; views.el ends here
