(provide 'cw-general-options)

(require 'server)
(require 's)
(require 'dash)

(setq default-tab-width 4)
(tool-bar-mode -1)  ; Turn off toolbar
(menu-bar-mode -1)  ; Turn off menu
(xterm-mouse-mode 1)  ; Turn on mouse support in terminal
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-linum-mode t)
(setq inhibit-startup-message t)  ; Bypass emacs start screen
(setq uniquify-buffer-name-style 'reverse)
(diredp-toggle-find-file-reuse-dir t)  ; Dired+ reuse buffer for changing dirs.
(linum-relative-toggle)
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Liberation Mono-10"))
      ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "Liberation Mono-11"))
      ((eq system-type 'darwin)
       (set-face-attribute 'default nil :font "Monaco-13")))
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq smooth-scroll-margin 5)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq-default line-spacing 4)
(setq term-buffer-maximum-size 10000)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(blink-cursor-mode 0)
(set-face-attribute 'cursor nil :background "#00ff00")
(setq woman-fill-frame t)
(setq mouse-autoselect-window t)
(unless (server-running-p) (server-start))

;; Workaround for PATH on mac
(when (eq system-type 'darwin)
  (let (emacs-path-dir?
        not-emacs-path-dir?
        dirs
        emacs-dirs
        non-emacs-dirs
        new-dirs)
    (defun emacs-path-dir? (x) (s-starts-with? "/Applications/Emacs.app" x))
    (defun not-emacs-path-dir? (x) (not (emacs-path-dir? x)))
    (setq dirs (s-split ":" (getenv "PATH")))
    (setq emacs-dirs (-filter 'emacs-path-dir? dirs))
    (setq non-emacs-dirs (-filter 'not-emacs-path-dir? dirs))
    (setq new-dirs (-concat emacs-dirs '("/usr/local/bin") non-emacs-dirs))
    (setenv "PATH" (s-join ":" new-dirs))))
(setq exec-path (-cons* exec-path "/usr/local/bin"))

;; auto saving and loading
(add-hook 'focus-out-hook (cmd (save-some-buffers t)))
(setq auto-revert-interval 1)
(global-auto-revert-mode t)

;; Put backup files and autosave files in temp dir.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Suppress "Symbolic link to Git-controlled source file; follow link?"
(setq vc-follow-symlinks nil)

;; Add space after line numbers when used from terminal.
(unless window-system (setq linum-format "%d "))
