;;; views.el --- Save/restore window configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: views, workgroups, windows
;; Package-Version: 20190220
;; Version: 0.1.0

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
    (with-current-buffer (window-buffer wt)
      (cond
       (buffer-file-name
        (list 'file buffer-file-name (point)))
       ((eq major-mode 'dired-mode)
        (list 'file default-directory (point)))
       (t
        (list 'buffer (buffer-name) (point)))))))

(defun views--current-view ()
  "Get the view for the current window."
  (views--parse-window-tree (car (window-tree))))


(defun views--set-view (name)
  "Change the current window-configuration to the view of NAME.")

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
