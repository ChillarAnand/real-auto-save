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
;; On Makefile, Emacs confirming really save every and every insert
;; <tab> to edit it.
;; Suppress this confirming, call `real-auto-save-activate-advice'
;; on your init.el
;;
;;     (real-auto-save-activate-advice)
;;
;; If disable this advice, call `real-auto-save-remove-advice'.
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

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun real-auto-save-buffers ()
  "Automatically save all buffers in real-auto-save-buffers-list."
  (save-current-buffer
    (dolist (elem real-auto-save-buffers-list)
      (if (or (not (buffer-live-p elem))
              (with-current-buffer elem (buffer-file-name)))
          (delete elem real-auto-save-buffers-list)
        (with-current-buffer elem
          (when (buffer-modified-p)
            (with-suppressed-message (save-buffer))))))))

(defun real-auto-save-remove-buffer-from-list ()
  "If a buffer is killed, remove it from real-auto-save-buffers-list."
  (when (member (current-buffer) real-auto-save-buffers-list)
    (setq real-auto-save-buffers-list
          (delete (current-buffer) real-auto-save-buffers-list))))

(defalias 'real-auto-save--disable 'ignore)
(defun real-auto-save-activate-advice ()
  "Suppress confirming when writing incomplete lines in Makefile.
Call `real-auto-save-remove-advice' to remove advice."
  (interactive)
  (with-eval-after-load 'make-mode
    (advice-add 'makefile-warn-suspicious-lines :override 'real-auto-save--disable)
    (advice-add 'makefile-warn-continuations    :override 'real-auto-save--disable)))

(defun real-auto-save-remove-advice ()
  "Remove advice of `real-auto-save-activate-advice'."
  (interactive)
  (advice-remove 'makefile-warn-suspicious-lines 'real-auto-save--disable)
  (advice-remove 'makefile-warn-continuations    'real-auto-save--disable))

;;;###autoload
(define-minor-mode real-auto-save-mode
  "Save your buffers automatically."
  :lighter " RAS"
  :keymap nil
  (if real-auto-save-mode
      (progn
        (unless real-auto-save-timer
          (setq real-auto-save-timer
                (run-with-idle-timer real-auto-save-interval t 'real-auto-save-buffers)))
        (add-to-list 'real-auto-save-buffers-list (current-buffer)))
    (real-auto-save-remove-buffer-from-list)))


(provide 'real-auto-save)
;;; real-auto-save.el ends here
