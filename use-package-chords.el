;;; use-package-chords.el --- key-chord keyword for use-package

;; Copyright (C) 2015 justin talbott

;; Author: justin talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/use-package-chords
;; Version: 0.1
;; Package-Requires: ((use-package "2.0") (bind-key "1.0") (key-chord "0.6") (bind-chord "0.1"))
;; Filename: use-package-chords.el
;;

;;; Commentary:
;;

;;; Code:

(require 'use-package)
(require 'bind-chord)

(add-to-list 'use-package-keywords :chords t)

(defalias 'use-package-normalize/:chords 'use-package-normalize-binder)

(defun use-package-handler/:chords (name keyword arg rest state)
  "Handler for `:chords' keyword in `use-package'."
  (use-package-handler/:bind name keyword arg rest state 'bind-chords))

(provide 'use-package-chords)
;;; use-package-chords.el ends here
