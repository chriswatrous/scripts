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
