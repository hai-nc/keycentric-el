;;;; keycentric.el --- Centralizing keybindings into one place.  -*- lexical-binding: t; fill-column: 80 -*-
;;
;; Copyright (C) 2019  Hai NGUYEN
;;
;; Author: Hai NGUYEN
;; Created: 2019-08-19
;; Version: 0.1.0
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
(defconst keycentric-version "0.1.0")


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
        key-index
        key
        a-form
        package-form
        package-symbols
        package-symbol)
    (while (car forms)
      (setf a-form (pop forms)
            keys (mapcar #'keycentric--convert-key-to-vector
                         (keycentric--list-item-maybe (pop a-form))))
      (while (car a-form)
        (setf package-form (pop a-form)
              package-symbols (keycentric--list-item-maybe (car package-form))
              keymap-dot-fun-forms (cdr package-form))
        (unless (= (length package-symbols) 1)
          (warn "keycentric(): it is not advisable to have multiple package symbols `%S' in one form `%S', as it is likely that these packages define multiple keymaps in this form, and it will not be trivial to tell which keymap coming from exactly which package in order to properly do the key-binding."
                package-symbols
                package-form))
        (while (car keymap-dot-fun-forms)
          (setf keymap-dot-fun-form (pop keymap-dot-fun-forms))
          ;; cannot check empty list with (car package-symbols) nor use `pop' in (pop package-symbols) because package-symbols can be this special list: (nil).
          (while (null (zerop (length package-symbols)))
            (setf package-symbol (car package-symbols)
                  package-symbols (cdr package-symbols)
                  key-index (length keys))
            (while (>= (decf key-index) 0)
              (setf key (nth key-index keys))
              (keycentric--parse-keymap-dot-fun-forms keymap-dot-fun-form key package-symbol))))))))


(provide 'keycentric)
;;; keycentric.el ends here
