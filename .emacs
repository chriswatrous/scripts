(let ((default-directory "~/.emacs.d/packages"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;;; Install packages
(require 'package)
(add-to-list 'package-archives '("melpa stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
;; Installed:
;;   clojure-mode
;;   evil
;;   powerline
;;   powerline-evil

(require 'evil)
(require 'rainbow-delimiters)
(require 'uniquify)
(require 'powerline)

;;;; Macros
(defmacro comment (&rest args) nil)


(comment
  (print-list load-path)  ; Print the current load path
  (list-colors-display)   ; list color names
  (list-faces-display)    ; list current faces
  )


;;;; Options
(tool-bar-mode -1)  ; Turn off toolbar
(menu-bar-mode -1)  ; Turn off menu
(xterm-mouse-mode 1)  ; Turn on mouse support in terminal
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
(setq inhibit-startup-message t)  ; Bypass emacs start screen
(setq uniquify-buffer-name-style 'forward)
(evil-mode 1)
(powerline-default-theme)

;; Suppress "Symbolic link to Git-controlled source file; follow link?"
(setq vc-follow-symlinks nil)

;; Add space after line numbers when used from terminal.
(unless window-system (setq linum-format "%d "))


;;;; Hooks
;; Rainbow delimiters hooks
(setq rainbow-hooks
      '(emacs-lisp-mode-hook
	python-mode-hook
	clojure-mode-hook))
(dolist (h rainbow-hooks)
  (add-hook h #'rainbow-delimiters-mode))


;;;; Colors
;; Default colors for new frames
(setq default-frame-alist
      '((foreground-color . "#E0E0E0")
	(background-color . "#000000")))

(set-face-attribute 'linum nil :foreground "#00cc00")
(set-face-attribute 'fringe nil :background "#111133")

;; Rainbow delimiters colors
(let ((i 1))
  (dolist (c '("#ff0000" "#00ff00" "#0066ff" "#ffff00" "#ff00ff"
	       "#00ffff" "#880000" "#008800" "#0000ff"))
    (set-face-attribute (intern (format "rainbow-delimiters-depth-%d-face" i))
			nil :foreground c)
    (setq i (1+ i))))
(dolist (s '(rainbow-delimiters-mismatched-face rainbow-delimiters-unmatched-face))
  (set-face-attribute s nil :foreground "white" :background "red"))


;;;; Key bindings
;;; Tweak some existing commands
;; Open buffer list in same window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Switch to new window after split below.
(defun my-split-window-below () (interactive) (progn
  (split-window-below)
  (other-window 1)))
(global-set-key (kbd "C-x 2") 'my-split-window-below)

;; Switch to new window after split right.
(defun my-split-window-right () (interactive) (progn
  (split-window-right)
  (other-window 1)))
(global-set-key (kbd "C-x 3") 'my-split-window-right)

(global-set-key (kbd "C-.") 'repeat)

;;; Evil-mode key bindings
(evil-define-key 'insert global-map (kbd "C-c") 'evil-esc)

;;; Clear some keys
(global-unset-key (kbd "C-z"))


;;;; Useful Functions
(defun print-list (list)
  (while list
    (princ-list (car list) "\n")
    (setq list (cdr list))))
