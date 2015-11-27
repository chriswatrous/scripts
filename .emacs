(let ((default-directory "~/.emacs.d/packages"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(require 'rainbow-delimiters)
(require 'package)

;;;; Install packages
(add-to-list 'package-archives '("melpa stable" . "http://melpa-stable.milkbox.net/packages/") t)

(setq my-packages '("clojure-mode"))
(dolist (p my-packages)
  (unless (package-installed-p p) (package-install p)))


;;;; Macros
(defmacro comment (&rest args) nil)


;;;; Useful info
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
(add-to-list 'default-frame-alist '(foreground-color . "#E0E0E0"))
(add-to-list 'default-frame-alist '(background-color . "#000000"))
(set-face-attribute 'linum nil :foreground "#00cc00")

;; Rainbow delimiters colors
(set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#ff0000")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#00ff00")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#0066ff")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#ffff00")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#ff00ff")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#00ffff")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#880000")
(set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#008800")
(set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#0000ff")
(set-face-attribute 'rainbow-delimiters-mismatched-face nil
		    :foreground "white" :background "red")
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
		    :foreground "white" :background "red")


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

;;; Custom key bindings
(global-set-key (kbd "C-.") 'repeat)

;;; Clear some keys
(global-unset-key (kbd "C-z"))


;;;; Useful Functions
(defun print-list (list)
  (while list
    (princ (car list))
    (princ "\n")
    (setq list (cdr list))))
