- [Keycentric &#x2013; Centralizing keybindings into one place](#org81994fb)
- [Features](#org4264ba9)
- [Limitations](#org5c56abd)
- [Installation](#orge0b9131)
- [Usage](#org9f1c453)
- [References](#orgee29b47)


<a id="org81994fb"></a>

# Keycentric &#x2013; Centralizing keybindings into one place

In my Emacs key-bindings, a single key-event (e.g. "<f5>") is bound in different minor-mode's keymaps, and usually the functions bound to each key perform similar tasks. To change the binding for a single key-event, I need to visit different places in my file to change it.

This library seek to centralize all key-bindings into one place, by combining all key-bindings to the same keyevents in different major-modes/minor-modes into one single place (please see the example above for illustration).

This library does not bind multiple commands to one key, nor does it compose keymaps into a single one as does the built-in function \`make-composed-keymap'.


<a id="org4264ba9"></a>

# Features

-   Easy to search and update key-bindings bound to the same key-event.
-   Delaying the keymap binding until the specified feature is loaded.
-   An :eval keyword to define a form to handle any extraordinary case.
-   Binding multiple keys for multiple (major/minor) modes for multiple keymaps, all in the same form (please see the example below).


<a id="org5c56abd"></a>

# Limitations

-   Single point of failure. This library seeks to centralize all key-bindings into one place. However, this library will ignore any error during execution so as to avoid choking in any one place.
-   Not straightforward to integrate with other tools on key-bindings and key-mappings (currently just using the :eval mechanism to run arbitrary code in the form).
-   Using \`eval-after-load' for currently unavailable keymap: any misconfiguration on the key-binding may remain hidden until the feature gets loaded and error gets raised.
-   Repetition of the keymap names for each key, due to the 1-many mapping of each key to multiple maps.


<a id="orge0b9131"></a>

# Installation

After downloading this library to somewhere on your computer (let's call it path-to-keycentric in the code below):

```emacs-lisp

(add-to-list 'load-path path-to-keycentric)
(require 'keycentric)
```


<a id="org9f1c453"></a>

# Usage

Below is an example:

```emacs-lisp
(keycentric `(("<f5>" (nil (global-map . backward-up-list)))))
```

which is equivalent to

```emacs-lisp
(define-key global-map (kbd "<f5>") 'backward-up-list)
```

In the example, "nil" in "(nil (global-map . backward-up-list))" means the subsequent keymaps (the global-map in this example) are supposed to be available at the time this form gets executed, so that key-binding can be execute now instead of doing \`eval-after-load' it.

Now for defining the same key event "<f5>" for multiple modes (2 modes in the example below: nil (ie. the always-present "global" mode) and lisp-mode):

```emacs-lisp
(keycentric `(("<f5>" (nil (global-map . backward-up-list)))))
                      (lisp-mode (lisp-mode-map . up-list))
```

Assume that \`lisp-mode' has not been loaded at the time of execution of this example, then it is equivalent to:

```emacs-lisp
(define-key global-map (kbd "<f5>") 'backward-up-list)
(eval-after-load "lisp-mode"
     `(define-key ,lisp-mode-map (kbd "<f5>") up-list))
```

If \`lisp-mode' has already been loaded, then it is equivalent to:

```emacs-lisp
(define-key global-map (kbd "<f5>") 'backward-up-list)
(define-key lisp-mode-map (kbd "<f5>") 'up-list)
```

To define for multiple key-events ("<f5>" and "<S-f5>" below):

```emacs-lisp
(keycentric `((("<f5>" "<S-f5>")    (nil (global-map . backward-up-list))
                                    (lisp-mode (lisp-mode-map . up-list)))))
```

This is equivalent to (assuming \`lisp-mode' has not been loaded):

```emacs-lisp
(define-key global-map (kbd "<f5>") 'backward-up-list)
(define-key global-map (kbd "<S-f5>") 'backward-up-list)
(eval-after-load "lisp-mode"
    `(define-key ,lisp-mode-map (kbd "<f5>") up-list))
(eval-after-load "lisp-mode"
    `(define-key ,lisp-mode-map (kbd "<S-f5>") up-list))
```

[remap <function>] and lambda form works just like using \`define-key':

```emacs-lisp
(keycentric `(([remap pop-to-mark-command]
                        (nil (global-map . (lambda ()
                                            (interactive)
                                            (and (revert-buffer nil t)
                                            (message "buffer reverted."))))))))
```

which is equivalent to

```emacs-lisp
(define-key global-map [remap pop-to-mark-command]
                        (lambda ()
                          (interactive)
                          (and (revert-buffer nil t)
                          (message "buffer reverted."))))
```

And for multiple keymaps:

```emacs-lisp
(keycentric `((("<f5>" "<S-f5>")
               (elisp-mode ((lisp-mode-map emacs-lisp-mode-map lisp-interaction-mode-map) . backward-up-list)))))
```

which is equivalent to:

```emacs-lisp
(define-key lisp-mode-map (kbd "<f5>") 'backward-up-list)
(define-key lisp-mode-map (kbd "<S-f5>") 'backward-up-list)
(define-key emacs-lisp-mode-map (kbd "<f5>") 'backward-up-list)
(define-key emacs-lisp-mode-map (kbd "<S-f5>") 'backward-up-list)
(define-key lisp-interaction-mode-map (kbd "<f5>") 'backward-up-list)
(define-key lisp-interaction-mode-map (kbd "<S-f5>") 'backward-up-list)
```

If there are multiple forms starting with the same key-event (eg. multiple forms starting with "<f5>" key-event), then the effect will be the sequential execution of all such forms, from the first to the last.

And to use :eval for flexibility:

```emacs-lisp
("<f8>" (eshell (:eval (add-hook 'eshell-mode-hook
                                 (lambda () (define-key eshell-mode-map
                                              keycentric-key
                                              #'view-echo-area-messages))))))
```

This is equivalent to

```emacs-lisp
(eval-after-load "eshell"
  `(add-hook ,eshell-mode-hook (lambda ()
                                (define-key ,eshell-mode-map
                                            (kbd "<f8>")
                                            view-echo-area-messages))))
```

In this :eval form, the key-event to be bound could be replaced with the variable \`keycentric-key', which is provided as a convenience (user can still re-type the key-event inside the form instead of using the variable \`keycentric-key'). Key-Binding for eshell is used in the example because eshell-mode-map is a local-buffer map that is only activated when eshell-mode is activated, thus to define a keymapping for eshell-mode-map one may need to add-hook as in the example.


<a id="orgee29b47"></a>

# References

-   [bind-key.el](https://github.com/jwiegley/use-package/blob/master/bind-key.el)