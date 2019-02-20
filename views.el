;;; views.el --- Save/restore window configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: views, workgroups, windows
;; Package-Version: 20190220
;; Version: 0.1.0
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

;; Currently restores:
;; - Window splits (not sizes or locations)
;; - Closed buffers
;; - Location of point

;; TODO: restore: split sizes, window locations, closed terminals

;;; Code:

(require 'dash)
(require 'f)
(require 'ht)
(require 's)

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
    (prin1 views (current-buffer))))

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
  (let ((typ (car view)))
    (when (symbolp typ)
      typ)))

(defun views--view-name (view)
  "Get the name of VIEW."
  (let ((name (nth 1 view)))
    (when (stringp name)
      name)))

(defun views--view-point (view)
  "Get the location of point in VIEW."
  (let ((p (nth 2 view)))
    (when (numberp p)
      p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun views--collect-buffer-info (buf)
  "Collect information to save about buffer BUF."
  (with-current-buffer buf
    (cond
     (buffer-file-name
      (list 'file buffer-file-name (point)))
     ((eq major-mode 'dired-mode)
      (list 'file default-directory (point)))
     (t
      (list 'buffer (buffer-name) (point))))))


(defun views--parse-window-tree (wt)
  "Construct a view from a window-tree.

Currently saves:
- Type of window (split / file-visiting / non file-visiting)
- Location of point in the buffer. "
  ;; if the window-tree is a cons-pair, it is split into multiple windows, and
  ;; we need to figure out what they are.
  (if (consp wt)
      ;; if (car window-tree) is `t', the window is split vertically, if it is
      ;; `nil', it is split horizontally.
      (if (eq (car wt) t)
          (cons 'vertical (-map #'views--parse-window-tree (cddr wt)))
        (cons 'horizontal (-map #'views--parse-window-tree (cddr wt))))
    ;; if WT is not a cons-pair, it is a leaf window
    (views--collect-buffer-info (window-buffer wt))))

(defun views--current-view ()
  "Get the view for the current window."
  (views--parse-window-tree (car (window-tree))))

(defun views--restore-file (view)
  "Restore file saved in VIEW."
  (let* ((name (views--view-name view))
         (buffer (get-buffer name)))
    (cond
     (buffer
      ;; if a buffer visiting the file already exists, use that
      (switch-to-buffer buffer nil 'force-same-window))
     ((file-exists-p name)
      ;; otherwise open the file if it exists on disk
      (find-file name))))

  ;; restore point position if saved
  (when-let ((p (views--view-point view)))
    (goto-char p)))

(defun views--restore-buffer (view)
  "Restore buffer stored in VIEW."
  (switch-to-buffer (views--view-name view))

  ;; restore point position if saved
  (when-let ((p (views--view-point view)))
    (goto-char p)))

(defun views--set-view-recur (view)
  "Set VIEW recursively."
  (cond
   ;; current child is a vertical split, walk children recursively
   ((eq (views--view-type view) 'vertical)
    (let* ((wnd1 (selected-window))
           (wnd2 (split-window-vertically))
           (views (cdr view))
           (v (pop views)))
      (with-selected-window wnd1
        (views--set-view-recur v))
      (while (setq v (pop views))
        (with-selected-window wnd2
          (views--set-view-recur v))
        (when views
          (setq wnd2 (split-window-vertically))))))

   ;; current child is a horizontal split, walk children recursively
   ((eq (views--view-type view) 'horizontal)
    (let* ((wnd1 (selected-window))
           (wnd2 (split-window-horizontally))
           (views (cdr view))
           (v (pop views)))
      (with-selected-window wnd1
        (views--set-view-recur v))
      (while (setq v (pop views))
        (with-selected-window wnd2
          (views--set-view-recur v))
        (when views
          (setq wnd2 (split-window-horizontally))))))

   ;; current child is a window showing a file
   ((eq (views--view-type view) 'file)
    (views--restore-file view))

   ;; current child is a window showing a non-file buffer
   ((eq (views--view-type view) 'buffer)
    (views--restore-buffer view))))

(defun views--set-view (name)
  "Change the current window-configuration to the view of NAME."
  (let* ((views (views--load-views))
         (view (ht-get views name)))
    (if view
        (let ((inhibit-message t))
          (delete-other-windows)
          (views--set-view-recur view))
      (message "view '%s' not found" name))))

;;;;;;;;;;;;;;;
;; interface ;;
;;;;;;;;;;;;;;;

(defun views-push ()
  "Save the current window view."
  (interactive)
  (let* ((views (ht-keys (views--load-views)))
         (name (completing-read "View name: " views))
         (view (views--current-view)))
    (if (member name views)
        (message "that name is already in use")
      (views--add name view))))

(defun views-pop ()
  "Remove a view from saved views."
  (interactive)
  (let* ((views (ht-keys (views--load-views)))
         (name (completing-read "Pick view: " views nil t)))
    (views--remove name)))

(defun views-switch ()
  "Switch to a saved view."
  (interactive)
  (let* ((views (views--load-views))
         (view (completing-read "Pick view: " views nil t)))
    (when view
      (views--set-view view))))

(provide 'views)
