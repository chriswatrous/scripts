(let ((default-directory "~/.emacs.d/packages"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(require 'rainbow-delimiters)

(defmacro comment (&rest args) nil)

(comment
  (print-list load-path)  ; Print the current load path
  )

(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
(setq inhibit-startup-message t)

;; Add space after line numbers when used from terminal.
(unless window-system
  (setq linum-format "%d "))

(comment
  (list-colors-display)  ; list color names
  (list-faces-display)   ; list current faces
  )
(add-to-list 'default-frame-alist '(foreground-color . "#E0E0E0"))
(add-to-list 'default-frame-alist '(background-color . "#000000"))
(set-face-attribute 'linum nil :foreground "forest green")

;; Open buffer list in same window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Switch to new window after split below.
(defun my-split-window-below () (interactive) (progn
  (split-window-below)
  (other-window 1)))
(global-set-key (kbd "C-x 2") 'my-split-window-below)

;; Switch to new window after split below.
(defun my-split-window-right () (interactive) (progn
  (split-window-right)
  (other-window 1)))
(global-set-key (kbd "C-x 3") 'my-split-window-right)

;; Suppress "Symbolic link to Git-controlled source file; follow link?"
(setq vc-follow-symlinks nil)

(defun print-list (list)
  (while list
    (princ (concat (car list) "\n"))
    (setq list (cdr list))))
