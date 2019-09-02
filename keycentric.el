;;;; keycentric.el --- Centralizing keybindings into one place.  -*- lexical-binding: t; fill-column: 80 -*-
;;
;; Copyright (C) 2019  Hai NGUYEN
;;
;; Author: Hai NGUYEN
;; Created: 2019-08-19
;; Version: 0.2.0
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
(defconst keycentric-version "0.2.0")


(defun keycentric--list-item-maybe (item-or-list)
  (if (atom item-or-list)
      (list item-or-list)
    item-or-list))


(defun keycentric--convert-key-to-vector (key-arg)
  (cond ((vectorp key-arg) key-arg)
        ((stringp key-arg) (kbd key-arg))
        (t (user-error "Wrong argument's datatype: KEY (%s) is of type `%s' instead of type vector or string!" key-arg (type-of key-arg)))))


(defun keycentric--eval-after-load-package (package-symbol keybinding-form)
  (declare (indent 1))
  (if (or (null package-symbol) (featurep package-symbol))
      (eval keybinding-form)
    (eval-after-load package-symbol keybinding-form)))


(defun keycentric--parse-keymap-dot-fun-forms (keymaps-dot-fun-form key package-symbol)
    ;; if after the package-form follows a form starting with the keyword :eval, just `eval' the remaining of the form, otherwise parse it and evaluate
  (if (eq (car keymaps-dot-fun-form) :eval)
      (progn
        (pop keymaps-dot-fun-form)
        (keycentric--eval-after-load-package package-symbol
          `(let ((keycentric-key ,key))
             ,@keymaps-dot-fun-form)))
    (progn
      (unless (consp keymaps-dot-fun-form)
        (error "this form is supposedly a dotted-pair"))
      (let ((maps (keycentric--list-item-maybe (car keymaps-dot-fun-form)))
            (fun (cdr keymaps-dot-fun-form))
            map)
        (while (car maps)
          (setf map (pop maps))
          (keycentric--eval-after-load-package package-symbol
            `(define-key ,map ,key ',fun)))))))


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
    (condition-case the-error
        (while forms
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
                       ((symbolp fun)                        
                        `(define-key ,map ,key ',fun))
                       ((consp fun)
                        `(define-key ,map ,key ,fun))
                       (t (error "Unhandled case of function symbol %s (type %s)" fun (type-of fun)))))
                 (if (or (null package) (featurep package))
                     (eval keymapping-form)
                   (eval-after-load package keymapping-form))))))
    (error (user-error "Error during processing keymapping form %S: %S" keymapping-form (error-message-string the-error))))))



(provide 'keycentric)
;;; keycentric.el ends here
