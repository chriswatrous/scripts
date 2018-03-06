
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/user/")

(require 'cw-utils)
(require 'cw-package-setup)
(require 'cw-evil-setup)
(require 'cw-general-options)
(require 'cw-modes-setup)
(require 'cw-key-bindings)
(require 'cw-symbols)

;; Theses will fail if the package is not installed. Then install the package.
(require 'clojure-mode)
(require 'column-marker)
(require 'cython-mode)
(require 'dired+)
(require 'dockerfile-mode)
(require 'feature-mode)
(require 'go-mode)
(require 'haskell-mode)
(require 'highlight-symbol)
(require 'json-mode)
(require 'linum-relative)  ; need melpa version
(require 'markdown-mode)
(require 'popup)
(require 'projectile)
(require 'python-mode)
(require 'rainbow-delimiters)
(require 'uniquify)
(require 'server)
(require 'smooth-scrolling)
(require 'yaml-mode)
(require 'zoom-frm)

; must load after installed packages are loaded
(require 'cw-colors)

;; Initialization
(split-window-right)
;; (other-window 1)
;; (term "bash")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (web-mode zoom-frm yaml-mode smooth-scrolling s rjsx-mode restclient rainbow-delimiters python-mode projectile powerline-evil popup npm-mode multi-term markdown-preview-mode linum-relative json-mode highlight-symbol haskell-mode groovy-mode go-mode feature-mode evil-magit elpy dockerfile-mode dired+ cython-mode column-marker cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
