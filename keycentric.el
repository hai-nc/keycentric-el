;;;; keycentric.el --- Centralizing keybindings into one place.  -*- lexical-binding: t; fill-column: 80 -*-
;;
;; Copyright (C) 2019  Hai NGUYEN
;;
;; Author: Hai NGUYEN
;; Created: 2019-08-19
;; Version: 0.2.1
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
(defconst keycentric-version "0.2.1")


(defun keycentric--list-item-maybe (item-or-list)
  (if (atom item-or-list)
      (list item-or-list)
    item-or-list))


(defun keycentric--convert-key-to-vector (key-arg)
  (cond ((vectorp key-arg) key-arg)
        ((stringp key-arg) (kbd key-arg))
        (t (user-error "Wrong argument's datatype: KEY (%s) is of type `%s' instead of type vector or string!" key-arg (type-of key-arg)))))


(defun keycentric (forms)
  "FORMS a list of key-binding forms."
  (let (keys
        form-remainings
        key-mappings
        keymapping-form
        fun
        package-dot-map-pairs
        package
        map)
    (while forms
      (condition-case the-error
          (progn
            (setf form-remainings (pop forms)
                  keys (mapcar #'keycentric--convert-key-to-vector
                               (keycentric--list-item-maybe (pop form-remainings))))
            (while form-remainings
              (setf key-mappings (pop form-remainings)
                    fun (pop key-mappings))
              (while key-mappings
                (setf package-dot-map-pairs (pop key-mappings)
                      package (car package-dot-map-pairs)
                      map (cdr package-dot-map-pairs))
                (dolist (key keys)
                  (setf
                   keymapping-form
                   (cond
                    ((and (consp fun) (eq (car fun) :eval))
                     `(let ((keycentric-key ,key)
                            (keycentric-map ,map))
                        ,(cadr fun)))
                    ((or (atom fun)
                         (and (consp fun)
                              (member (car fun)
                                      '(closure lambda))))
                     `(define-key ,map ,key ',fun))
                    ((consp fun)
                     `(define-key ,map ,key ,fun))
                    (t (error "Unhandled case of function symbol %s (type %s)" fun (type-of fun)))))
                  ;; adding extra validations for keymapping-form:
                  (setf keymapping-form
                        (append `(progn
                                   (unless (boundp ',map)
                                     (user-error "Keymap `%s' not found in package `%s'!" ',map ',package))
                                   (cl-assert (or (null ',package)
                                                  (featurep ',package)))                                 
                                   (cl-assert (keymapp ,map)))
                                (list keymapping-form)))
                  ;; if package is available now, eval-ing keymapping-form now,
                  ;; else eval-after-load it:
                  (if (or (null package) (featurep package))
                      (eval keymapping-form)
                    (eval-after-load package keymapping-form))))))
        (error (message "%s in `%S' (%s)" the-error keymapping-form (error-message-string the-error)))))))



(provide 'keycentric)
;;; keycentric.el ends here
