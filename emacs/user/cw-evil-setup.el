(provide 'cw-evil-setup)

;; Fix search. Must be set before loading evil.
(setq evil-search-module 'evil-search)

(require 'evil)

;; turn on evil-mode
(evil-mode 1)

; fix cursor color
(setq evil-default-cursor t)
(set-cursor-color "white")

;; Go into emacs state after starting these modes.
(dolist (mode '(term eshell ielm))
  (eval `(defadvice ,mode (after advice-term-line-mode activate)
           (evil-normal-state)
           (evil-emacs-state))))

(add-hook 'cider-docview-mode-hook #'evil-emacs-state)
(add-hook 'ielm-mode-hook #'evil-emacs-state)
(add-hook 'cider-repl-mode-hook #'evil-emacs-state)
(add-hook 'cider-stacktrace-mode-hook #'evil-emacs-state)

;; Key bindings
(macroexpand '(evil-define-key 'normal global-map "C-u" 'evil-scroll-up))

;; (evil-define-key 'normal global-map "M-." nil)
