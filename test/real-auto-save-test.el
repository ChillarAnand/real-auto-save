;; How to test:
;; load this file
;; M-x ert t
;;  cf. http://www.gnu.org/software/emacs/manual/html_mono/ert.html

(setq load-path (cons "../" load-path))
(require 'ert)
(require 'real-auto-save)

(ert-deftest real-auto-save-test/real-auto-save-start-timer ()
  "Test of 'real-auto-save-start-timer'"
  (real-auto-save-start-timer)
  (should (timerp real-auto-save-timer)))

