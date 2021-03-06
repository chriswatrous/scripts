(provide 'cw-modes-setup)

(require 'cw-utils)

;; powerline
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-vim-color-theme)

;; json-mode
(require 'json-mode)
(add-hook 'json-mode-hook (cmd (make-local-variable 'js-indent-level)
                               (setq js-indent-level 2)))

;; javascript-mode
(setq js-indent-level 2)
(add-hook 'js-mode-hook
          (lambda ()
            (modify-syntax-entry ?` "\"" js-mode-syntax-table)
            (modify-syntax-entry ?_ "w")))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (if (equal web-mode-content-type "javascript")
                (web-mode-set-content-type "jsx")
              (message "now set to: %s" web-mode-content-type))))

;; term
(require 'term)
; Fix terminal window height.
; This function needed to use (floor (window-screen-lines)) instead of
; (1- (window-height))
; This works for 24.4.1
(defun term-check-size (process)
  (when (or (/= term-height (floor (window-screen-lines)))
	    (/= term-width (term-window-width)))
    (term-reset-size (floor (window-screen-lines)) (term-window-width))
    (set-process-window-size process term-height term-width)))
; fix paste on mac
(when osx?
  (add-hook 'term-mode-hook
            (lambda () (define-key term-raw-map (kbd "s-v") 'term-paste))))

;; c-mode
(setf (cdr (assoc 'other c-default-style)) "python")
;; (defun my-c-mode-hook ()
;;   (setq c-basic-offset 4
;;         c-indent-level 4
;;         c-default-style "python"))
;; (add-hook 'c-mode-common-hook 'my-c-mode-hook)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4
                  c-indent-level 4
                  c-default-style "python"
                  indent-tabs-mode nil
                  comment-start "//"
                  comment-end "")))

;; python-mode / elpy
(require 'python-mode)
(require 'elpy)
(add-hook 'python-mode-hook
          (cmd (highlight-regexp "import i?pdb; i?pdb\.set_trace()"
                                 'highlight)
               (call-interactively 'highlight-indentation-mode)))
(elpy-enable)
(elpy-use-ipython)
(define-key elpy-mode-map (kbd "<C-return>") nil)
(define-key elpy-mode-map (kbd "C-S-d") 'elpy-goto-definition-other-window)
(when (file-exists-p "~/venv")
  (pyvenv-activate "~/venv/"))
;; (setq flymake-no-changes-timeout 3)
;; (setq python-check-command "flake8")
;; (setq elpy-rpc-backed "jedi")
; python-mode insert breakpoint
(define-key-cmd python-mode-map (leader+ "C-b")
  (let ((indent (current-indent)))
    (beginning-of-line)
    (insert (concat (s-repeat indent " ")
                    "import ipdb; ipdb.set_trace()\n"))
    (forward-line -1)
    (forward-char indent)))
; Go to first pep8 error
(define-key-cmd python-mode-map (kbd "<f5>")
  (let ((result (call-process-buffer-str "pep8" "-")))
    (if (/= (length (s-trim result)) 0)
        (let ((parts (parse-pep8 result)))
          (beginning-of-buffer)
          (forward-line (1- (nth 0 parts)))
          (forward-char (1- (nth 1 parts)))
          (popup-tip (nth 2 parts)))
      (princ "No pep8 errors."))))


;; dired
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; cider
(require 'cider)
; Change cider-repl C-return to C-S-return so it doesn't interfere with my
; leader key.
(define-key cider-repl-mode-map (kbd "<C-return>") nil)
(define-key cider-repl-mode-map (kbd "<C-S-return>")
  'cider-repl-closing-return)
(add-hook 'cider-mode-hook (cmd (modify-syntax-entry ?- "w")))

;; emacs-lisp-mode
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(define-key emacs-lisp-mode-map (kbd "<tab>") 'completion-at-point)

;; nxml-mode
(setq nxml-child-indent 4
      nxml-attribute-indent 4)


;; restclient-mode
;; (add-function :after (symbol-function 'restclient-mode)
              ;; (lambda () (define-key restclient-mode-map (kbd "C-M-x")
                           ;; 'restclient-http-send-current-stay-in-window)))
