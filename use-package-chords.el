;;; use-package-chords.el --- key-chord keyword for use-package

;; Copyright (C) 2015 justin talbott

;; Author: justin talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/use-package-chords
;; Version: 0.0.1
;; Package-Requires: ((use-package "2.0") (bind-key "1.0") (key-chord "0.6"))
;; Filename: use-package-chords.el
;;

;;; Commentary:
;;

;;; Code:

(require 'use-package)
(require 'bind-key)
(require 'key-chord)

;;;###autoload
(defun bind-chord (chord command &optional keymap)
  "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed)."
  (let ((key1 (logand 255 (aref chord 0)))
        (key2 (logand 255 (aref chord 1))))
    (if (eq key1 key2)
        (bind-key (vector 'key-chord key1 key2) command keymap)
      (bind-key (vector 'key-chord key1 key2) command keymap)
      (bind-key (vector 'key-chord key2 key1) command keymap))))

;;;###autoload
(defmacro bind-chords (&rest args)
  "Bind multiple chords at once.

Accepts keyword argument:
:map - a keymap into which the keybindings should be added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (let* ((map (plist-get args :map))
         (maps (if (listp map) map (list map)))
         (key-bindings (progn
                         (while (keywordp (car args))
                           (pop args)
                           (pop args))
                         args)))
    (macroexp-progn
     (apply
      #'nconc
      (mapcar (lambda (form)
                (if maps
                    (mapcar
                     #'(lambda (m)
                         `(bind-chord ,(car form) ',(cdr form) ,m)) maps)
                  `((bind-chord ,(car form) ',(cdr form)))))
              key-bindings)))))

(add-to-list 'use-package-keywords :chords t)

(defalias 'use-package-normalize/:chords 'use-package-normalize-binder)

(defun use-package-handler/:chords (name keyword arg rest state)
  "Handler for `:chords' keyword in `use-package'."
  (use-package-handler/:bind name keyword arg rest state 'bind-chords))

(provide 'use-package-chords)
;;; use-package-chords.el ends here
