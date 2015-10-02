[`key-chord`](http://www.emacswiki.org/emacs/key-chord.el) keyword
support for [`use-package`](https://github.com/jwiegley/use-package).

## Installation and Usage

Recommended install from [MELPA](melpa.milkbox.net) with `M-x
package-install use-package-chords`. Then require it after `use-package`, i.e.:

``` elisp
(eval-when-compile
  (require 'use-package))
(use-package use-package-chords
  :config (key-chord-mode 1))
```

With this extension, you can define chords using the `:chords` keyword in the same manner
as `:bind` and related keywords, using a cons or a list of conses:

``` elisp
(use-package ace-jump-mode
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode)))
```
