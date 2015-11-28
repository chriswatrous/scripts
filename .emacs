(let ((default-directory "~/.emacs.d/packages"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))


;;;; Macros
(defmacro comment (&rest args) nil)
(defmacro cmd (&rest args) `(lambda () (interactive) (progn ,@args)))


;;;; Prepare package manager
(require 'package)
(let ((repos '(("melpa stable" . "http://melpa-stable.milkbox.net/packages/")
	      ("melpa" . "http://melpa.milkbox.net/packages/")
	      ("marmalade" . "http://marmalade-repo.org/packages/"))))
  (dolist (repo repos)
    (add-to-list 'package-archives repo)))
(add-to-list 'package-archives '("melpa stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(comment
 (package-list-packages)  ; list / install / uninstall packages
 )
;; Installed:
;;   clojure-mode
;;   evil
;;   powerline
;;   powerline-evil
;;   cider
;;   projectile
;;   linum-relative


(require 'evil)
(require 'rainbow-delimiters)
(require 'uniquify)
(require 'powerline)
(require 'powerline-evil)
(require 'dired+)
(require 'linum-relative)


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
(setq uniquify-buffer-name-style 'reverse)
(diredp-toggle-find-file-reuse-dir t)  ; Dired+ reuse buffer for changing dirs.
(linum-relative-on)

;; Evil mode
(evil-mode 1)
(setq evil-default-cursor t)  ; fix cursor color
(set-cursor-color "white")    ; fix cursor color

(powerline-evil-vim-color-theme)

;; Suppress "Symbolic link to Git-controlled source file; follow link?"
(setq vc-follow-symlinks nil)

;; Add space after line numbers when used from terminal.
(unless window-system (setq linum-format "%d "))


;;;; Hooks
;; Activate rainbow delimiters.
(setq rainbow-hooks
      '(emacs-lisp-mode-hook
	python-mode-hook
	clojure-mode-hook))
(dolist (h rainbow-hooks)
  (add-hook h #'rainbow-delimiters-mode))

;; Don't hide details in dired.
;; (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 0)))


;;;; Colors
;; Default colors for new frames
(setq default-frame-alist
      '((foreground-color . "#E0E0E0")
	(background-color . "#000000")))

(set-face-attribute 'linum nil :foreground "#00cc00")
(set-face-attribute 'fringe nil :background "#111133")

;; Rainbow delimiters colors
(defun face (i)
  (intern (format "rainbow-delimiters-depth-%d-face" i)))
(let ((i 1))
  (dolist (c '("#ff0000" "#00ff00" "#0066ff" "#ffff00" "#ff00ff"
	       "#00ffff" "#880000" "#008800" "#0000ff"))
    (set-face-attribute (face i) nil :foreground c)
    (setq i (1+ i))))
(dolist (s '(rainbow-delimiters-mismatched-face rainbow-delimiters-unmatched-face))
  (set-face-attribute s nil :foreground "white" :background "red"))


;;;; Key bindings
;;; Tweak some existing commands
;; Open buffer list in same window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)


;; Switch to new window after split.
(global-set-key (kbd "C-x 2") (cmd (split-window-below) (other-window 1)))
(global-set-key (kbd "C-x 3") (cmd (split-window-right) (other-window 1)))

;; Map C-c to ESC in all evil states but normal and emacs.
(defun my-esc (prompt)
  (cond ((or (evil-insert-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
	(t (kbd "C-c"))))
(define-key key-translation-map (kbd "C-c") 'my-esc)
;; Evil operator state doesn't use the key-translation-map.
(define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)

;;; Custom key bindings
;; Keys I could use (with current binding)
;; C-a - move to beginning of line
;; C-j - newline
;; C-k - kill line
;; C-\ - toggle input method
;; C-l - current line to center / top / bottom
;; C-;
;; C-=
;; C-'
;; C-Return
;; C-,
;; C-`
;; C-Tab
;; C-S-Tab
;; F5
;; F6
;; F7
;; F8
;; F9
;; F12

;; C-q comibination
(define-key global-map (kbd "C-q") nil)
(define-key global-map (kbd "C-q s")
  (cmd (find-file "/ssh:chris@192.168.1.50:/home/chris/stuff")))
(define-key global-map (kbd "C-q q") 'linum-relative-toggle)

;; Dired / Dired+
(define-key dired-mode-map (kbd "<backspace>") 'diredp-kill-this-tree)


;;;; Useful Functions
(defun print-list (list)
  (while list
    (princ-list (car list) "\n")
    (setq list (cdr list))))
(put 'dired-find-alternate-file 'disabled nil)
