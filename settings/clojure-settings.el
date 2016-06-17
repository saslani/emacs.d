;; Clojure IDE setup

(defun clojure ()
  (interactive)
  (four-windows)
  (shell)
  (cider-jack-in)
  (shrink-window 10))

(global-set-key (kbd "<f9>") 'clojure)

(defun make-or-find-repl ()
  (if (not (buffer-exists? "*cider-repl"))
      (cider-jack-in)
    (progn
      (select-window (get-lru-window))
      (switch-to-buffer (cider-current-repl-buffer)))))

(defun repl-right ()
  (interactive)
  (split-window-right -50)
  (make-or-find-repl))

(global-set-key (kbd "M-h M-r") 'repl-right)

(defun repl-bottom ()
  (interactive)
  (split-window-below -15)
  (make-or-find-repl))

(global-set-key (kbd "M-h M-b") 'repl-bottom)

;; customize indentation for midje facts
(require 'clojure-mode)
(define-clojure-indent
  (fact 'defun)
  (facts 'defun)
  (fact-group 'defun)
  (silent-fact 'defun)
  (future-fact 'defun)
  (tabular 'defun)
  (against-background 'defun)
  (error-let 'defun)
  (provided 0))

;; hide the *nrepl-connection* and *nrepl-server* buffers from
;; appearing in switch-to-buffer (C-x b)
(setq nrepl-hide-special-buffers t)

;; show nrepl port in cider-repl buffer name
(setq nrepl-buffer-name-show-port t)

(defun pretty-lambdas (mode)
  (font-lock-add-keywords
      mode `(("(\\(fn\\>\\)"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1)
                                                 (make-char 'greek-iso8859-7 107))
                                 'decompose-region))))))

(eval-after-load 'clojure-mode (lambda () (pretty-lambdas 'clojure-mode)))
(eval-after-load 'clojurescript-mode (lambda () (pretty-lambdas 'clojurescript-mode)))


(add-hook 'clojure-mode-hook 'linum-mode)

(add-hook 'cider-interaction-mode-hook 'eldoc-mode)

(add-hook 'cider-mode-hook (lambda ()
                             (eldoc-mode)
                             (paredit-mode +1)
                             (autopair-mode 0)))

(require 'no-bold-fonts)
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (eldoc-mode)
            (disable-bold-fonts)
            (paredit-mode 1)
            (autopair-mode 0)
            (rainbow-delimiters-mode 1)))

(setq cider-repl-display-help-banner nil)
(setq cider-auto-select-error-buffer nil)
(setq cider-show-error-buffer nil)
(setq cider-repl-use-clojure-font-lock t)

;; eval in repl
(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(global-set-key (kbd "M-h e") 'cider-eval-expression-at-point-in-repl)

(defun set-ns-with-repl-requires ()
  "Include repl-requires in non-user namespaces."
  (interactive)
  (cider-repl-set-ns (cider-current-ns))
  (cider-interactive-eval "(apply require clojure.main/repl-requires)"))

(add-hook 'nrepl-connected-hook
          (lambda ()
            ;; Override the default cider-repl-set-ns key binding
            (define-key cider-mode-map (kbd "C-c M-n") 'set-ns-with-repl-requires)))

(defun clj-scratch ()
  "Create/retrieve a Clojure scratch buffer and switch to it.."
  (interactive)
  (switch-to-buffer (get-buffer-create "*clj-scratch*"))
  (clojure-mode)
  (if (not (buffer-exists? "*cider-repl"))
      (cider-jack-in)))

(global-set-key (kbd "M-h c") 'clj-scratch)


;; starter-kit-lisp functions
;; Copyright (c) 2008-2010 Phil Hagelberg and contributors
;;; These belong in prog-mode-hook:
;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a byte-compiled lambda doesn't already exist in the list.

(defun esk-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-column-number-mode)
(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

(defun esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
(add-hook 'emacs-lisp-mode-hook 'esk-prog-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

;;; Enhance Lisp Modes

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; TODO: look into parenface package
(defface esk-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'starter-kit-faces)

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                            '(("(\\|)" . 'esk-paren-face))))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'paredit-mode))

(provide 'clojure-settings)
