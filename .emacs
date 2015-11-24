(tool-bar-mode -1)
(menu-bar-mode -1)
(xterm-mouse-mode 1)

(global-linum-mode t)
(setq inhibit-startup-message t)

;; Add space after line numbers when used from terminal.
(unless window-system
  (setq linum-format "%d "))

(add-to-list 'default-frame-alist '(foreground-color . "#E0E0E0"))
(add-to-list 'default-frame-alist '(backround-color . "#000000"))

;; Open buffer list in same window.
(global-set-key "\C-x\C-b" 'buffer-menu)

;; Normally emacs prints this
;; "Symbolic link to Git-controlled source file; follow link?"
;; when opening a symlnk.
;; Suppress this message.
(setq vc-follow-symlinks nil)
