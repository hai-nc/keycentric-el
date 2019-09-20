;;;; keycentric.el --- Centralizing keybindings into one place.  -*- lexical-binding: t; fill-column: 80 -*-
;;
;; Copyright (C) 2019  Hai NGUYEN
;;
;; Author: Hai NGUYEN
;; Created: 2019-08-19
;; Version: 0.3.0
;; Package-Requires: none
;; Keywords: dotemacs startup key
;; URL: <https://gitlab.com/haicnguyen/keycentric-el.git>, <https://github.com/haicnguyen/keycentric-el>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; Please refer to the attached file "README.md".
;;
;; Tested on Emacs 26.2


;;; Code:
(defconst keycentric-version "0.3.0")


(defun keycentric--list-item-maybe (item-or-list)
  (if (listp item-or-list)
      item-or-list
    (list item-or-list)))


(defun keycentric--convert-key-to-vector (key-arg)
  (cond ((vectorp key-arg) key-arg)
        ((stringp key-arg) (kbd key-arg))
        (t (user-error "Wrong argument's datatype: KEY (%s) is of type `%s' instead of type vector or string!" key-arg (type-of key-arg)))))


(defun keycentric--process-package-map (key value)
  (let* ((package key)
         map key command-or-lambda
         (defun-name (intern (format "keycentric-eval-after-load-%s" package)))
         (defun-form `(defun ,defun-name ()))
         (defun-body nil))
    (dolist (form% value)
      (setf map (first form%)
            key (second form%)
            command-or-lambda (third form%)
            keymapping-form
            (cond
             ((and (consp command-or-lambda) (eq (car command-or-lambda) :eval))
              `(let ((keycentric-key ,key)
                     (keycentric-map ,map))
                 ,(cadr command-or-lambda)))
             ((or (atom command-or-lambda)
                  (and (consp command-or-lambda)
                       (member (car command-or-lambda)
                               '(closure lambda))))
              `(define-key ,map ,key ',command-or-lambda))
             ((consp command-or-lambda)
              `(define-key ,map ,key ,command-or-lambda))
             (t (error "Unhandled case of function symbol %s (type %s)" command-or-lambda (type-of command-or-lambda)))))
      ;; (message "keymapping-form: %S" keymapping-form)
      (setf defun-body (cons keymapping-form defun-body)))
    ;; nreverse defun-body to restore the order of execution of the keymapping-form:
    (setf defun-form (append defun-form (nreverse defun-body)))
    ;; define the function now
    (eval defun-form)
    (or (fboundp defun-name) (error "Unable to define `%s' (keycentric--process-package-map)!" defun-name))
    (if (or (null package) (featurep package))
        (funcall defun-name)
      (eval-after-load package `(,defun-name)))))


(defun keycentric-define (forms)
  "FORMS a list of key-binding forms."
  (let (keys
        keymapping-form
        command-or-lambda
        package
        map
        ;;
        ;; the table below stores information for keybindings defined in the keycentric FORMS specific to each package
        ;;
        ;; each entry has the key a package symbol, and its value a list of key-bindings information: (<keymap> <key-vector> <function or lambda expression>)
        (package-map-table (make-hash-table :test #'eq)))
    ;; processing each form in forms
    (dolist (form% forms)
      ;; maintain the flow of execution by wrapping the operation in a condition-case form to handle any potential error.
      (condition-case the-error
          (progn
            ;; parse the key event vector
            (setf keys (mapcar #'keycentric--convert-key-to-vector
                               (keycentric--list-item-maybe (pop form%))))
            ;; the remaining of form% is a list of (command (<mode> . <mode-keymap>)'s)
            (dolist (key-mappings form%)
              ;; get the command symbol
              (setf command-or-lambda (pop key-mappings))
              ;; get the package and mode-specific-keymap information:
              (dolist (package-dot-map-pairs key-mappings)
                (setf package (car package-dot-map-pairs)
                      map (cdr package-dot-map-pairs))
                ;; for each keyvector parsed above, generate a list of (keymap keyvector command-or-lambda-expression) to add to the package-map-table:
                (dolist (key keys)
                  (puthash package
                           (cons (list map key command-or-lambda)
                                 (gethash package package-map-table))
                           package-map-table)))))
        (error (message "%s in `%S' (%s)" the-error keymapping-form (error-message-string the-error)))))
    ;; restore the order of appearance of the keymapping-form:
    (require 'subr-x)
    (dolist (key% (hash-table-keys package-map-table))
      (puthash key% (nreverse (gethash key% package-map-table)) package-map-table))
    (maphash #'keycentric--process-package-map package-map-table)
    ;; DEBUGGING: print the hash-table:
    package-map-table))



(provide 'keycentric)
;;; keycentric.el ends here
