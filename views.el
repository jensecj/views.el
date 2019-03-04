;;; views.el --- Save/restore window configurations. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; URL: http://github.com/jensecj/views.el
;; Keywords: views, workgroups, windows
;; Package-Version: 20190304
;; Version: 0.2.1
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

(defvar views-collect-functions '()
  "Given a buffer, return a key-value-pair.")

(defvar views-restore-functions '()
  "Given a buffer and map, do something.")

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

(defun views--collect-buffer (buf)
  "Return information collected about BUF from all
`views-collect-functions'."
  (with-current-buffer buf
    (let ((collected))
      (dolist (collector views-collect-functions)
        (when-let ((result (funcall collector buf)))
          (push result collected)))

      (-flatten collected))))

(defun views--collect-dired-buffer (buf)
  "Return information if BUF is a `dired' buffer."
  (when (eq major-mode 'dired-mode)
    `((type . dired)
      (path . ,default-directory)
      (point . ,(point)))))
(add-to-list 'views-collect-functions #'views--collect-dired-buffer)

(defun views--collect-term-buffer (buf)
  "Return information if BUF is a terminal buffer."
  (when (eq major-mode 'term-mode)
    `((type . term)
      (name . ,(buffer-name))
      (path . ,default-directory))))
(add-to-list 'views-collect-functions #'views--collect-term-buffer)

(defun views--collect-file-buffer (buf)
  "Return information if BUF is a file-visiting buffer."
  (when (buffer-file-name)
    `((type . file)
      (path . ,(buffer-file-name))
      (point . ,(point))
      (window-start . ,(window-start)))))
(add-to-list 'views-collect-functions #'views--collect-file-buffer)

(defun views--window-tree-buffers (wt)
  "Return all buffers open in the `window-tree' WT."
  (-flatten
   (if (consp wt)
       (-map #'views--window-tree-buffers (cddr wt))
     (with-current-buffer (window-buffer wt) (current-buffer)))))

(defun views--frame-buffers (frame)
  "Return all buffers open in FRAME."
  (views--window-tree-buffers (car (window-tree frame))))

(defun views--current-view ()
  "Get the view for the current window."
  (let* ((frame (selected-frame))
         (frameset (views--frameset frame))
         (buffers (views--frame-buffers frame))
         (collected (-map #'views--collect-buffer buffers)))
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
  "Restore file stored in BUF."
  (let* ((path (alist-get 'path buf)))
    (if (not (file-exists-p path))
        (error "File '%s' does not exist" path))

    (find-file-noselect path)))

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

(defun views--restore-term (buf)
  "Restore terminal stored in BUF."
  (let* ((name (alist-get 'name buf))
         (clean-name (s-chop-suffix "*" (s-chop-prefix "*" name)))
         (path (alist-get 'path buf))
         (buffer (get-buffer name)))
    ;; if the buffer already exists, don't recreate it
    (or buffer (views--make-term clean-name path))))

(defun views--restore-buffers (buffers)
  "Restore all buffers described in BUFFERS."
  (dolist (alist buffers)
    (when-let ((type (alist-get 'type alist))
               (restored
                (cond
                 ((eq type 'file) (views--restore-file alist))
                 ((eq type 'dired) (views--restore-file alist))
                 ((eq type 'term) (views--restore-term alist)))))
      (dolist (restorer views-restore-functions)
        (with-current-buffer restored
          (funcall restorer alist))))))

(defun views--buffers-restore-point (alist)
  "Restore position of point if stored in ALIST."
  (when-let ((p (alist-get 'point alist)))
    (message "restoring point!")
    (goto-char p)))
(add-to-list 'views-restore-functions #'views--buffers-restore-point)

(defun views--buffers-restore-window-start (alist)
  (when-let ((s (alist-get 'window-start alist)))
    (message "restoring window-start!")
    (set-window-start (selected-window) s)))
(add-to-list 'views-restore-functions #'views--buffers-restore-window-start)

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



;; TODO: save remote files
;; TODO: save font size
;; TODO: save frame size

(provide 'views)
;;; views.el ends here
