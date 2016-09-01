(add-to-list 'load-path "~/.emacs.d/user/")

(require 'cw-utils)
(require 'cw-package-setup)
(require 'cw-evil-setup)
(require 'cw-general-options)

;; Theses will fail if the package is not installed. Then install the package.
(require 'clojure-mode)
(require 'cider)
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

;;;; Useful info
;; (print-list load-path)  ; Print the current load path
;; (list-colors-display)   ; list color names
;; (list-faces-display)    ; list current faces

;; Elpy setup
(elpy-enable)
(define-key elpy-mode-map (kbd "<C-return>") nil)
;; (when (file-exists-p "/Users/chris/venv")
  ;; (pyvenv-activate "/Users/chris/venv/"))
;; (setq flymake-no-changes-timeout 3)
;; (setq python-check-command "flake8")
;; (setq elpy-rpc-backed "jedi")


;;;; Key bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun leader+ (key) (kbd (concat "<C-return> " key)))
(define-key global-map (kbd "<f2>") 'evil-ex-nohighlight)
(define-key global-map (kbd "C-\\") nil)

;;; Disable some built in keys
(global-unset-key (kbd "C-j"))

;;; Tweak some existing commands
;; Open buffer list in same window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Map C-c to ESC in all evil states but normal and emacs.
;; (defun my-esc (prompt)
;;   (cond ((or (evil-insert-state-p) (evil-replace-state-p)
;;              (evil-visual-state-p)) [escape])
;;         (t (kbd "C-c"))))
;; (define-key key-translation-map (kbd "C-c") 'my-esc)
;; ;; Evil operator state doesn't use the key-translation-map.
;; (define-key evil-operator-state-map (kbd "C-c") 'keyboard-quit)

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

;; Change cider-repl C-return to C-S-return so it doesn't interfere with my
;; leader key.
(define-key cider-repl-mode-map (kbd "<C-return>") nil)
(define-key cider-repl-mode-map (kbd "<C-S-return>")
  'cider-repl-closing-return)

(define-key global-map (leader+ "C-q") 'linum-relative-toggle)
(evil-define-key 'normal global-map "C-u" 'evil-scroll-up)
;; (evil-define-key 'normal global-map "M-." nil)


;; Scroll by 5
(dolist (k '("C-S-e" "<C-down>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-down 5))))
(dolist (k '("C-S-y" "<C-up>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-up 5))))

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
                       (kill-this-buffer))))

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

;; python-mode insert breakpoint
(define-key-cmd python-mode-map (leader+ "C-b")
  (let ((indent (current-indent)))
    (beginning-of-line)
    (insert (concat (str* " " indent)
                    "import ipdb; ipdb.set_trace()\n"))
    (forward-line -1)
    (forward-char indent)))

;; Go to first pep8 error
(define-key-cmd python-mode-map (kbd "<f5>")
  (let ((result (call-process-buffer-str "pep8" "-")))
    (if (/= (length (strip result)) 0)
        (let ((parts (parse-pep8 result)))
          (beginning-of-buffer)
          (forward-line (1- (nth 0 parts)))
          (forward-char (1- (nth 1 parts)))
          (popup-tip (nth 2 parts)))
      (princ "No pep8 errors."))))


;; Emacs pager support
(defun ep-revert-buffer ()
  (when (and (get-buffer ep-current-buffer)
             (equal (buffer-name) ep-current-buffer))
    (revert-buffer t t t))
  (run-at-time "1 sec" ep-revert-buffer))
(setq ep-current-buffer nil)


;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(split-window-right)
(other-window 1)
(term "bash")
