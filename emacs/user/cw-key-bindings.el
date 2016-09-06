(provide 'cw-key-bindings)

(require 'cw-utils)

(define-key global-map (kbd "<f2>") 'evil-ex-nohighlight)
(define-key global-map (kbd "C-\\") nil)

;;; Disable some built in keys
(global-unset-key (kbd "C-j"))

;;; Tweak some existing commands
;; Open buffer list in same window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;;; Custom key bindings
;; Keys I could use (with current binding)
;; C-j - newline
;; C-k - kill line
;; C-\ - toggle input method
;; C-l - current line to center / top / bottom
;; C-q - quoted-insert

;; C-=
;; C-`
;; C-Tab
;; C-S-Tab
;; F6
;; F7
;; F8
;; F9
;; F12

(define-key global-map (leader+ "C-q") 'linum-relative-toggle)

;; Better zoom in/out
(dolist (k '("C-x C-=" "C-x C--" "C-x C-0" "C-x C-+"))
  (define-key global-map (kbd k) 'zoom-in/out))

(define-key global-map (kbd "C-S-t") (cmd (term "/bin/bash")))
(define-key global-map (kbd "C-;") 'buffer-menu)
(define-key global-map (leader+ "C-e") 'eshell)
(define-key global-map (leader+ "C-g") 'rgrep)

;; Opening files
(defun find-file-either-window (other-window)
  (if other-window
      (command-execute 'find-file-other-window)
    (command-execute 'find-file)))
(define-key global-map (kbd "C-'")
  (cmd (find-file-either-window (equal major-mode 'term-mode))))
(define-key global-map (kbd "C-\"")
  (cmd (find-file-either-window (not (equal major-mode 'term-mode)))))
(define-key global-map (kbd "C-,") 'async-shell-command)

;; Scroll by 5
(dolist (k '("C-S-e" "<C-down>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-down 5))))
(dolist (k '("C-S-y" "<C-up>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-up 5))))

;; Window switching
(define-key global-map (kbd "C-S-h") 'evil-window-left)
(define-key global-map (kbd "C-S-l") 'evil-window-right)
(define-key global-map (kbd "C-S-k") 'evil-window-up)
(define-key global-map (kbd "C-S-j") 'evil-window-down)
(define-key global-map (kbd "S-<left>") 'evil-window-left)
(define-key global-map (kbd "S-<right>") 'evil-window-right)
(define-key global-map (kbd "S-<up>") 'evil-window-up)
(define-key global-map (kbd "S-<down>") 'evil-window-down)

;; Window management
(global-set-key (kbd "C-S-q") (cmd (split-window-below) (other-window 1)))
(global-set-key (kbd "C-S-w") (cmd (split-window-right) (other-window 1)))
(global-set-key (kbd "C-~") 'delete-window)

(global-set-key
 (leader+ "C-r")
 (cmd (if (buffer-modified-p) (revert-buffer) (revert-buffer t t t))))

(global-set-key (leader+ "r") 'rename-buffer)

(global-set-key (leader+ "C-c")
                (cmd (save-some-buffers t)
                     (if server-buffer-clients
                         (server-edit)
                       (kill-this-buffer))
                     (buffer-menu)))

(global-set-key (leader+ "C-f") 'make-frame)

;; Bindings for highlight-symbol
(define-key global-map (leader+ "C-w") 'highlight-symbol)
(define-key global-map (leader+ "w") 'highlight-symbol-remove-all)
(define-key global-map (kbd "C->") 'highlight-symbol-next)
(define-key global-map (kbd "C-<") 'highlight-symbol-prev)
(define-key global-map (kbd "C-S-s")
  (cmd (save-some-buffers t)
       (unless (evil-emacs-state-p) (evil-normal-state))
       (message "all files saved")))
;; (define-key global-map (kbd "C-c")
;;   (cmd (unless (evil-emacs-state-p) (evil-normal-state))))

;; Dired / Dired+
(define-key dired-mode-map (kbd "<backspace>") 'diredp-kill-this-tree)

;; Make escape quit from the minibuffer.
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
