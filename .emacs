;; (let ((default-directory "~/.emacs.d/packages"))
;;   (add-to-list 'load-path default-directory)
;;   (normal-top-level-add-subdirs-to-load-path))


;;;; Macros
(defmacro comment (&rest args) nil)
(defmacro cmd (&rest args) `(lambda () (interactive) (progn ,@args)))


;;;; Prepare package manager
(require 'package)
(setq package-archives
      '(("melpa stable" . "http://melpa-stable.milkbox.net/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(comment
 (package-list-packages)  ; list / install / uninstall packages
 )

;; Theses will fail if the package is not installed. Then install the package.
(require 'clojure-mode)
(require 'column-marker)
(require 'dired+)
(require 'dockerfile-mode)
(require 'evil)
(require 'feature-mode)
(require 'highlight-symbol)
(require 'linum-relative)  ; need melpa version
(require 'powerline)
(require 'powerline-evil)
(require 'projectile)
(require 'rainbow-delimiters)
(require 'uniquify)
(require 'smooth-scrolling)
(require 'zoom-frm)


;;;; Useful info
(comment
  (print-list load-path)  ; Print the current load path
  (list-colors-display)   ; list color names
  (list-faces-display)    ; list current faces
  )


;;;; Predicates for telling which system I am on
(defun windowsp () (eq system-type 'windows-nt))
(defun work-windowsp () (file-exists-p "C:/Users/IBM_ADMIN"))


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
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Liberation Mono-10")))
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq smooth-scroll-margin 5)

;; Put backup files and autosave files in temp dir.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Column marker stuff
(dolist (s '(emacs-lisp-mode-hook python-mode-hook))
  (add-hook s (lambda () (column-marker-1 79))))
(set-face-attribute 'column-marker-1 nil :background "red" :foreground "white")

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
(dolist (s '(rainbow-delimiters-mismatched-face
	     rainbow-delimiters-unmatched-face))
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
  (cond ((or (evil-insert-state-p) (evil-replace-state-p)
	     (evil-visual-state-p)) [escape])
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

;; Allow C-q to be used as a prefix key.
(define-key global-map (kbd "C-q") nil)

(define-key global-map (kbd "C-q q") 'linum-relative-toggle)

;; Scroll by 5
(dolist (k '("C-S-e" "<C-down>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-down 5))))
(dolist (k '("C-S-y" "<C-up>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-up 5))))

;; Better zoom in/out
;; (dolist (k '("C-x C-=" "C-x C--" "C-x C-0" "C-x C-+"))
;;   (define-key global-map (kbd k) 'zoom-in/out))

;; Open main work location in dired.
(define-key global-map (kbd "C-q a")
  (cmd (if (work-windowsp)
	   (find-file "C:/Users/IBM_ADMIN/VirtualBox Share/gitrepos")
	   (find-file "/ssh:chris@192.168.1.50:/home/chris/stuff"))))

;; Bindings for highlight-symbol
(define-key global-map (kbd "C-q C-w") 'highlight-symbol)
(define-key global-map (kbd "C-q w") 'highlight-symbol-remove-all)
(define-key global-map (kbd "C->") 'highlight-symbol-next)
(define-key global-map (kbd "C-<") 'highlight-symbol-prev)


;; Dired / Dired+
(define-key dired-mode-map (kbd "<backspace>") 'diredp-kill-this-tree)


;;;; Useful Functions
(defun print-list (list)
  (while list
    (princ-list (car list) "\n")
    (setq list (cdr list))))
