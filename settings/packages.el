(require 'package)

(setq package-archives '(("elpy" . "https://jorgenschaefer.github.io/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

(setq package-check-signature nil)

(package-initialize)

(defvar my-packages '(autopair
                      cider
                      clojure-mode
                      dockerfile-mode
                      elisp-slime-nav
                      elpy
                      enh-ruby-mode
                      ess
                      haml-mode
                      idle-highlight-mode
                      ido-ubiquitous
                      jinja2-mode
                      markdown-mode
                      neotree
                      paredit
                      rainbow-delimiters
                      robe
                      sass-mode
                      smex
                      yaml-mode
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; manually installed packages
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "ws-trim-1.4")

(require 'ess-site)

(provide 'packages)
