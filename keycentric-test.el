;;; keycentric-test.el --- Tests for keycentric.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Hai Nguyen

;; Author: Hai Nguyen <h@z5131530>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'xtest)


(defun keycentric-t-return ()
  (interactive)
  "This is the <return> key.")


(keycentric `(("<return>" (keycentric-t-1-mode (,keycentric-t-1-mode-map keycentric-t-return)))))

(with-temp-buffer
  (keycentric-t-1-mode)
  (let* ((key-command
          (key-binding (kbd "<return>")))
         (result (call-interactively key-command)))
    (cl-assert (string-equal result "This is the <return> key."))
    (kill-buffer (current-buffer))
    result))


(and (featurep 'keycentric-t-1-mode) (unload-feature 'keycentric-t-1-mode))


(defmacro keycentric-t--defun-key-test (testnum)
  nil ; (declare (debug 3))
  (let ((fun-name (intern (format "key-test-%s" testnum)))
        (map-name (intern (format "keycentric-t-%s-mode-map" testnum))))
    `(defun ,fun-name ()
        (interactive)
        (message (format "Command: \"%s\"." major-mode)))))

(keycentric-t--defun-key-test 1)

(define-derived-mode keycentric-t-1-mode fundamental-mode "keycentric1"
  :syntax-table nil ; use parent's table, thus avoid cluttering abbrev-table
  )



(with-temp-buffer
  (keycentric-t-1-mode)
  (let* ((key-command (key-binding (kbd "<kp-enter>")))
         (result (call-interactively key-command)))
    (kill-buffer (current-buffer))
    result))

;; (and (featurep 'keycentric-t-1-mode) (unload-feature 'keycentric-t-1-mode))


;;; Test if eval-after-load form working properly
;;;
;;; create a mode and load it now and execute its action

;;; TODO: test when 2 definition used, to see if the second definition works
;;;
;;; like so:
;; ("<M-S-backspace>" (nil (global-map . h/kill-beginning-of-line) (eshell (eshell-mode-map . h/kill-beginning-of-line-eshell))))

(provide 'keycentric-test)
;;; keycentric-test.el ends here
