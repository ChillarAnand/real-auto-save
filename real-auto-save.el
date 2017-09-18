;;; real-auto-save.el --- Automatically save your all your buffers/files at regular intervals.


;; Copyright (C) 2008-2015, Chaoji Li, Anand Reddy Pandikunta

;; Author: Chaoji Li <lichaoji AT gmail DOT com>
;;         Anand Reddy Pandikunta <anand21nanda AT gmail DOT com>
;; Version: 0.4
;; Date: January 27, 2015

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
;; Add following lines to your .emacs initialization file
;; to enable auto save in all programming modes.
;;
;;     (require 'real-auto-save)
;;     (add-hook 'prog-mode-hook 'real-auto-save-mode)
;;
;;
;; Auto save interval is 10 seconds by default.
;; You can change it to whatever value you want at any point.
;;
;;     (setq real-auto-save-interval 5) ;; in seconds
;;
;;


;;; Code:


(defgroup real-auto-save nil
  "Save buffers automatically."
  :group 'convenience
  :prefix "real-auto-save-")

(defcustom real-auto-save-interval 10
  "Time interval of real auto save."
  :type 'integer
  :group 'real-auto-save)

(defvar real-auto-save-buffers-list nil
  "List of buffers that will be saved automatically.")

(defvar real-auto-save-timer nil
  "Real auto save timer.")

(defun real-auto-save-start-timer ()
  "Start real-auto-save-timer."
  (setq real-auto-save-timer
        (run-at-time
         (time-add (current-time) (seconds-to-time real-auto-save-interval))
         real-auto-save-interval 'real-auto-save-buffers)))

(defun real-auto-save-restart-timer ()
  "Restart real-auto-save-timer."
  (if real-auto-save-timer
      (cancel-timer real-auto-save-timer))
  (real-auto-save-start-timer))

(defun real-auto-save-buffers ()
  "Automatically save all buffers in real-auto-save-buffers-list."
  (progn
    (save-excursion
      (dolist (elem real-auto-save-buffers-list)
        (if (get-buffer elem)
            (progn
              (set-buffer elem)
              (if (buffer-modified-p)
                  (save-buffer)))
          (delete elem real-auto-save-buffers-list))))
    (real-auto-save-restart-timer)))

(defun real-auto-save-remove-buffer-from-list ()
  "If a buffer is killed, remove it from real-auto-save-buffers-list."
  (if (member (current-buffer) real-auto-save-buffers-list)
      (setq real-auto-save-buffers-list
            (delete (current-buffer) real-auto-save-buffers-list))))

(define-minor-mode real-auto-save-mode
  "Save your buffers automatically."
  :lighter " RAS"
  :keymap nil
  :version "0.5"

  (when (not real-auto-save-mode) ;; OFF
    (when (buffer-file-name)
      (real-auto-save-remove-buffer-from-list)))

  (when real-auto-save-mode ;; ON
    (if (buffer-file-name)
        (progn
          (real-auto-save-restart-timer)
          (add-to-list 'real-auto-save-buffers-list (current-buffer))
          (add-hook 'kill-buffer-hook 'real-auto-save-remove-buffer-from-list)))))


(provide 'real-auto-save)
;;; real-auto-save.el ends here
