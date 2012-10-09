;;; real-auto-save.el --- enable real auto save

;; Copyright (C) 2008, Chaoji Li

;; Author: Chaoji Li <lichaoji AT gmail DOT com>
;; Version: 0.3
;; Date: May 17, 2008

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Put this file in a folder where Emacs can find it.
;;
;; Add following lines to your .emacs initialization file:
;;
;;     (require 'real-auto-save)
;;     (add-hook 'text-mode-hook 'turn-on-real-auto-save)
;;     (add-hook 'muse-mode-hook 'turn-on-real-auto-save)
;;
;; Auto save interval is 10 seconds by default. You can change it:
;;
;;     (setq real-auto-save-interval 5) ;; in seconds
;;

(defvar real-auto-save-alist nil
  "List of buffers that will be auto saved truely.")

(defvar real-auto-save-interval 10
  "Time interval of real auto save.")

(defvar real-auto-save-p t
  "Toggle real auto save.")

(defvar real-auto-save-timer nil
  "real auto save timer.")

(defun real-auto-save()
  (interactive)
  (if real-auto-save-p
      (progn
	(save-excursion
	  (dolist (elem real-auto-save-alist)
	    (set-buffer elem)
	    (if (and (buffer-file-name) (buffer-modified-p))
		(progn
		  (write-file (buffer-file-name)))))))))

(defun turn-on-real-auto-save()
  (interactive)
  (if (buffer-file-name)
      (progn
	(unless real-auto-save-timer
	    (progn 
	      (setq real-auto-save-timer (timer-create))
	      (timer-set-time real-auto-save-timer (current-time) real-auto-save-interval)
	      (timer-set-function real-auto-save-timer 'real-auto-save)
	      (timer-activate real-auto-save-timer)))
	(add-to-list 'real-auto-save-alist (buffer-name)))))

(defun turn-off-real-auto-save ()
  (interactive)
  (when (buffer-file-name)
    (setq real-auto-save-alist (remove (buffer-name) real-auto-save-alist))))

(provide 'real-auto-save)
