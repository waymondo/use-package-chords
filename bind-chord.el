;;; bind-chord.el --- key-chord binding helper for use-package-chords  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2022 Free Software Foundation, Inc.

;; Author: Justin Talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/jwiegley/use-package
;; Version: 0.2.2
;; Package-Requires: ((emacs "24.3") (bind-key "1.0") (key-chord "0.6"))
;; Filename: bind-chord.el

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

;;; Code:

(require 'bind-key)
(require 'key-chord nil t)
(declare-function key-chord-define "key-chord" (keymap keys command))

;;;###autoload
(defmacro bind-chord (chord command &optional keymap)
  "Bind CHORD to COMMAND in KEYMAP (`global-map' if not passed).
CHORD must be a string (e.g., \"jk\") or a vector (e.g., [?j ?k])."
  `(key-chord-define ,(if keymap keymap 'global-map)
                     ,chord
                     ,command))

(defun bind-chords-form (args keymap)
  "Bind multiple chords at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (let (map pkg)
    (let ((cont t))
      (while (and cont args)
        (if (cond ((eq :map (car args))
                   (setq map (cadr args)))
                  ((eq :package (car args))
                   (setq pkg (cadr args))))
            (setq args (cddr args))
          (setq cont nil))))

    (unless map (setq map keymap))

    (let (first next)
      (while args
        (if (keywordp (car args))
            (progn
              (setq next args)
              (setq args nil))
          (if first
              (nconc first (list (car args)))
            (setq first (list (car args))))
          (setq args (cdr args))))

      (cl-flet
          ((wrap (map bindings)
                 (if (and map pkg (not (memq map '(global-map
                                                   override-global-map))))
                     `((if (boundp ',map)
                           ,(macroexp-progn bindings)
                         (eval-after-load
                             ,(if (symbolp pkg) `',pkg pkg)
                           ',(macroexp-progn bindings))))
                   bindings)))

        (append
         (wrap map
               (cl-mapcan
                (lambda (form)
                  (let ((fun (and (cdr form) (list 'function (cdr form)))))
                    (if (and map (not (eq map 'global-map)))
                        `((bind-chord ,(car form) ,fun ,map))
                      `((bind-chord ,(car form) ,fun nil)))))
                first))
         (when next
           (bind-chords-form (if pkg
                            (cons :package (cons pkg next))
                          next) map)))))))

;;;###autoload
(defmacro bind-chords (&rest args)
  "Bind multiple chords at once.

Accepts keyword argument:
:map - a keymap into which the keybindings should be added

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (macroexp-progn (bind-chords-form args nil)))

(provide 'bind-chord)

;;; bind-chord.el ends here
