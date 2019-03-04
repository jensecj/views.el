;;; views.el --- Save/restore window configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL: http://github.com/jensecj/views.el
;; Keywords: views, workgroups, windows
;; Package-Version: 20190304
;; Version: 0.3.0
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

;; Simple functionality for saving and restoring window configurations and open
;; buffers.

;; See `views--collect-buffer' for what information is saved for each window.

;;; Code:

(require 'dash)
(require 'f)
(require 'ht)
(require 's)
(require 'subr-x)
(require 'frameset)

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

;;;;;;;;;;;;;;;;;;;;;
;; info collection ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar views-collect-functions '()
  "Functions to collect information about buffer.
The functions are called with the buffer to collect information
from as an argument, and should return a single or a list of
key-value-pairs.")

(defvar views-restore-functions '()
  "Functions to restore properties of a buffer.
The functions are called with an alist of saved information about
the restored buffer.  The restored buffer is also the current
buffer for the function call.")

(defvar views-resurrect-functions '()
  "Functions to resurrect buffers.
The functions are called with an alist of information about the
dead buffer, and should return a buffer.")

(defun views--collect-dired-buffer (buf)
  "Return information if BUF is a `dired' buffer."
  (when (eq major-mode 'dired-mode)
    `((type . dired)
      (path . ,default-directory)
      (point . ,(point)))))

(defun views--collect-term-buffer (buf)
  "Return information if BUF is a terminal buffer."
  (when (eq major-mode 'term-mode)
    `((type . term)
      (name . ,(buffer-name))
      (path . ,default-directory))))

(defun views--collect-file-buffer (buf)
  "Return information if BUF is a file-visiting buffer."
  (when (buffer-file-name)
    `((type . file)
      (path . ,(buffer-file-name))
      (point . ,(point))
      (window-start . ,(window-start)))))

(defun views--collect-buffer (buf)
  "Return information collected about BUF from all
`views-collect-functions'."
  (with-current-buffer buf
    (let ((collected))
      (dolist (collector views-collect-functions)
        (when-let ((result (funcall collector buf)))
          (push result collected)))

      (-flatten collected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-tree / frameset parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun views--window-tree-buffers (windowtree)
  "Return all buffers open in the WINDOWTREE."
  (-flatten
   (if (consp windowtree)
       (-map #'views--window-tree-buffers (cddr windowtree))
     (with-current-buffer (window-buffer windowtree) (current-buffer)))))

(defun views--frame-buffers (frame)
  "Return all buffers open in FRAME."
  (views--window-tree-buffers (car (window-tree frame))))

(defun views--current-view ()
  "Return the view for the current window."
  (let* ((frame (selected-frame))
         (frameset (views--frameset frame))
         (buffers (views--frame-buffers frame))
         (collected (-map #'views--collect-buffer buffers)))
    (cons collected frameset)))

(defun views--frameset (frame)
  "Return the frameset configuration for FRAME."
  (let ((frameset (frameset-save (list frame)))
        (current-display (frame-parameter frame 'display)))

    ;; don't store frame properties, we only want to keep the buffers and their
    ;; sizes/locations
    (setf (caar (elt frameset 7)) `((display . ,current-display)))
    frameset))

;;;;;;;;;;;;;;;;;;
;; resurrection ;;
;;;;;;;;;;;;;;;;;;

(defun views--resurrect-file (desc)
  "Resurrect file described in DESC."
  (when (eq 'file (alist-get 'type desc))
    (let* ((path (alist-get 'path desc)))
      (if (not (file-exists-p path))
          (error "File '%s' does not exist" path))

      (find-file-noselect path))))

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
        (setq multi-term-buffer-list (-cons* term-buffer multi-term-buffer-list)))
       (t ;; otherwise use term.el
        (term-mode)
        (term-char-mode)))
      term-buffer)))

(defun views--resurrect-term (desc)
  "Resurrect terminal described in DESC."
  (when (eq 'term (alist-get 'type desc))
    (let* ((name (alist-get 'name desc))
           (clean-name (s-chop-suffix "*" (s-chop-prefix "*" name)))
           (path (alist-get 'path desc))
           (buffer (get-buffer name)))
      ;; if the buffer already exists, don't recreate it
      (or buffer (views--make-term clean-name path)))))

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

(defun views--restore-point (alist)
  "Restore position of `point' if stored in ALIST."
  (when-let ((p (alist-get 'point alist)))
    (message "restoring point!")
    (goto-char p)))

(defun views--restore-window-start (alist)
  "Restore position of `window-start' if stored in ALIST."
  (when-let ((s (alist-get 'window-start alist)))
    (message "restoring window-start!")
    (set-window-start (selected-window) s)))

(defun views--resurrect-and-restore-buffers (descriptions)
  "Resurrect buffers described in DESCRIPTIONS, and restore their
properties."
  (dolist (alist descriptions)
    (when-let ((type (alist-get 'type alist))
               (restored (-some (lambda (fn) (funcall fn alist)) views-resurrect-functions)))
      (dolist (restorer views-restore-functions)
        (with-current-buffer restored
          (funcall restorer alist))))))

(defun views--set-view (name)
  "Change view to the view of NAME, restoring buffers if needed."
  (let* ((views (views--load-views))
         (view (ht-get views name)))
    (if view
        (let ((inhibit-message t))
          (delete-other-windows)
          (views--resurrect-and-restore-buffers (car view))
          (views--restore-frameset (cdr view)))
      (message "view '%s' not found" name))))

;;;;;;;;;;;;;;;
;; interface ;;
;;;;;;;;;;;;;;;

;;;###autoload
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

(-map
 (lambda (fn)
   (unless (member fn views-collect-functions)
     (add-to-list 'views-collect-functions fn)))
 (list
  #'views--collect-file-buffer
  #'views--collect-term-buffer
  #'views--collect-dired-buffer))

(-map
 (lambda (fn)
   (unless (member fn views-resurrect-functions)
     (add-to-list 'views-resurrect-functions fn)))
 (list
  #'views--resurrect-file
  #'views--resurrect-term))

(-map
 (lambda (fn)
   (unless (member fn views-restore-functions)
     (add-to-list 'views-restore-functions fn)))
 (list
  #'views--restore-point
  #'views--restore-window-start))


;; TODO: nuke point saving from frameset?
;; TODO: save remote files
;; TODO: save font size
;; TODO: save frame size

(provide 'views)
;;; views.el ends here
