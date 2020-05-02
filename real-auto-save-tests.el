;;; real-auto-save-tests.el --- Test definitions for real-auto-save  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/ChillarAnand/real-auto-save

;; This program is free software: you can redistribute it and/or modify
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

;; Test definitions for `real-auto-save'.


;;; Code:

(require 'cort)
(require 'ert-x)
(require 'subr-x)
(require 'real-auto-save)

(defmacro cort-deftest-with-equal (name form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example:
  (p (cort-deftest-with-equal leaf/disabled
       '((asdf asdf-fn)
         (uiop uiop-fn))))
   => (cort-deftest leaf/disabled
        '((:equal 'asdf asdf-fn)
          (:equal 'uiop uiop-fn)))"
  (declare (indent 1))
  `(cort-deftest ,name
                 ',(mapcar (lambda (elm)
                             `(:equal ,(cadr elm) ,(car elm)))
                           (cadr form))))

(defmacro cort-deftest-with-macroexpand-let (name letform form)
  "Return `cort-deftest' compare by `equal' for NAME, LETFORM FORM.

Example:
  (p (cort-deftest-with-macroexpand-let leaf/leaf
         ((leaf-expand-leaf-protect t))
       '(((leaf leaf
            :config (leaf-init))
          (prog1 'leaf
            (leaf-handler-leaf-protect leaf
              (leaf-init)))))))
   => (cort-deftest leaf/leaf
        '((:equal
           '(prog1 'leaf
              (leaf-handler-leaf-protect leaf
                (leaf-init)))
           (let ((leaf-expand-leaf-protect t))
             (macroexpand-1
              '(leaf leaf
                 :config (leaf-init)))))))"
  (declare (indent 2))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal
                   ',(cadr elm)
                   (let ,letform (macroexpand-1 ',(car elm)))))
               (cadr form))))


;;; test definition

(cort-deftest-with-equal real-auto-save/save-buffers
  '(((let ((file (make-temp-file "real-auto-save-test--")))
       (prog1 (progn
                (with-current-buffer (find-file-noselect file)
                  (real-auto-save-mode +1)
                  (insert "real-auto-save test")
                  (ert-run-idle-timers)
                  (set-buffer-modified-p nil)
                  (kill-buffer (current-buffer)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (string-trim (buffer-string))))
         (ignore-errors (delete-file file))))
     "real-auto-save test")

    ((let ((file (make-temp-file "real-auto-save-test--")))
       (prog1 (progn
                (with-current-buffer (find-file-noselect file)
                  (real-auto-save-mode +1)
                  (insert "real-auto-save test")
                  ;; (ert-run-idle-timers)
                  (set-buffer-modified-p nil)
                  (kill-buffer (current-buffer)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (string-trim (buffer-string))))
         (ignore-errors (delete-file file))))
     "")))


;; (provide 'real-auto-save-tests)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; real-auto-save-tests.el ends here
