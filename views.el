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

;; See `views--collect-window-info' for what information is saved for each window.

;; TODO: restore: window locations, closed terminals

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
  (ht-get view :type))

(defun views--view-path (view)
  "Get the path of VIEW."
  (ht-get view :path))

(defun views--view-name (view)
  "Get the name of VIEW."
  (ht-get view :name))

(defun views--view-point (view)
  "Get the location of point in VIEW."
  (ht-get view :point))

(defun views--view-width (view)
  "Get the width of window VIEW."
  (ht-get view :width))

(defun views--view-height (view)
  "Get the height of window VIEW."
  (ht-get view :height))

(defun views--view-subwindows (view)
  "Get the subwindows stored in split window VIEW"
  (ht-get view :subwindows))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun views--get-window-width (window)
  "Get the width of WINDOW in FRAME, in percentage."
  (let* ((frame (window-frame window))
         (total-width (frame-width frame))
         (win-width (window-width window)))
    (/ (float win-width) (float total-width))))

(defun views--get-window-height (window)
  "Get the height of WINDOW in FRAME, in percentage."
  (let* ((frame (window-frame window))
         (total-height (frame-height frame))
         (win-height (window-height window)))
    (/ (float win-height) (float total-height))))

(defun views--set-window-width (window percent)
  "Set WINDOW from FRAME to the percentage of WIDTH."
  (let* ((frame (window-frame window))
         (total-width (frame-width frame))
         (percent-width (floor (* total-width percent)))
         (cur-win-width (window-width window))
         (delta (- percent-width cur-win-width)))
    (window-resize window delta t)))

(defun views--set-window-height (window percent)
  "Set WINDOW from FRAME to the percentage of HEIGHT."
  (let* ((frame (window-frame window))
         (total-height (frame-height frame))
         (percent-height (floor (* total-height percent)))
         (cur-win-height (window-height window))
         (delta (- percent-height cur-win-height)))
    (window-resize window delta)))

(defun views--collect-window-info (win)
  "Collect information to save about buffer BUF."
  (with-current-buffer (window-buffer win)
    (cond
     (buffer-file-name
      (ht (:type 'file)
          (:path buffer-file-name)
          (:point (point))
          (:width (views--get-window-width win))
          (:height (views--get-window-height win))))
     ((eq major-mode 'dired-mode)
      (ht (:type 'file)
          (:path default-directory)
          (:point (point))
          (:width (views--get-window-width win))
          (:height (views--get-window-height win))))
     (t
      (ht (:type 'buffer)
          (:name (buffer-name))
          (:point (point))
          (:width (views--get-window-width win))
          (:height (views--get-window-height win)))))))

(defun views--parse-window-tree (wt)
  "Construct a view from a window-tree.

Currently saves:
- Type of window (split / file-visiting / non file-visiting)
- Location of point in the buffer.
- Width/Height of windows."
  ;; if the window-tree is a cons-pair, it is split into multiple windows, and
  ;; we need to figure out what they are.
  (if (consp wt)
      ;; if (car window-tree) is `t', the window is split vertically, if it is
      ;; `nil', it is split horizontally.
      (if (eq (car wt) t)
          (ht (:type 'vertical) (:subwindows (-map #'views--parse-window-tree (cddr wt))))
        (ht (:type 'horizontal) (:subwindows (-map #'views--parse-window-tree (cddr wt)))))
    ;; if WT is not a cons-pair, it is a leaf window
    (views--collect-window-info wt)))

(defun views--current-view ()
  "Get the view for the current window."
  (views--parse-window-tree (car (window-tree))))

(defun views--restore-file (view)
  "Restore file stored in VIEW."
  (let* ((path (views--view-path view))
         (buffer (get-buffer path)))
    (cond
     (buffer
      ;; if a buffer visiting the file already exists, use that
      (switch-to-buffer buffer nil 'force-same-window))
     ((file-exists-p path)
      ;; otherwise open the file if it exists on disk
      (find-file path))))

  ;; restore width and height if saved
  (when-let ((width (views--view-width view)))
    (views--set-window-width (selected-window) width))
  (when-let ((height (views--view-height view)))
    (views--set-window-height (selected-window) height))

  ;; restore point position if saved
  (when-let ((p (views--view-point view)))
    (goto-char p)))

(defun views--restore-buffer (view)
  "Restore buffer stored in VIEW."
  (switch-to-buffer (views--view-name view))

  ;; restore width and height if saved
  (when-let ((width (views--view-width view)))
    (views--set-window-width (selected-window) width))
  (when-let ((height (views--view-height view)))
    (views--set-window-height (selected-window) height))

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
           (views (views--view-subwindows view))
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
           (views (views--view-subwindows view))
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
