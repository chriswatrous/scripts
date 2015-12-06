;;;; Macros
(defmacro comment (&rest args) nil)
(defmacro cmd (&rest args) `(lambda () (interactive) (progn ,@args)))
(defmacro define-key-cmd (keymap key &rest body)
  `(define-key ,keymap ,key (lambda () (interactive) (progn ,@body))))

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (let ((lform (list-wrap form)))
        (setq result (append lform (list result)))))))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (let ((lform (list-wrap form)))
        (setq result (append (list (car lform) result)
                             (cdr lform)))))))

;; Used in the above macros
(defun list-wrap (x) (if (listp x) x (list x)))


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
(require 'popup)
(require 'powerline)
(require 'powerline-evil)
(require 'projectile)
(require 'python-mode)
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
(defun work-windows? () (file-exists-p "C:/Users/IBM_ADMIN"))
(defun work-linux? () (file-exists-p "/media/sf_VirtualBox_Share"))
(defun home-linux? () (file-exists-p "/home/chris/stuff"))


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
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

;; Put backup files and autosave files in temp dir.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Column marker stuff
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

;; Start term mode in line mode and evil emacs mode.
(defadvice term (after advice-term-line-mode activate)
  (evil-normal-state)
  (evil-emacs-state))

;; Activate rainbow delimiters and column marker for programming language
;; modes.
(dolist (h '(emacs-lisp-mode-hook python-mode-hook clojure-mode-hook))
  (add-hook h #'rainbow-delimiters-mode)
  (add-hook h (lambda () (column-marker-1 79))))

(add-hook 'dired-mode-hook #'dired-omit-mode)

(add-hook 'python-mode-hook
          (cmd (highlight-regexp "import ipdb; ipdb\.set_trace()"
                                 'highlight)))

;;;; Colors
;; Default colors for new frames
(setq default-frame-alist
      '((foreground-color . "#E0E0E0")
        (background-color . "#000000")))

(set-face-attribute 'linum nil :foreground "#00cc00")
(set-face-attribute 'fringe nil :background "#111133")
(set-face-attribute 'highlight nil :background "#005500")

;; Rainbow delimiters colors
(defconst rainbow-colors
  '("#ff0000"
    "#00ff00"
    "#0066ff"
    "#ffff00"
    "#ff00ff"
    "#00ffff"
    "#880000"
    "#008800"
    "#0000ff"))
(let ((i 1))
  (dolist (c rainbow-colors)
    (set-face-attribute (intern (format "rainbow-delimiters-depth-%d-face" i))
                        nil :foreground c)
    (setq i (1+ i))))
(dolist (s '(rainbow-delimiters-mismatched-face
             rainbow-delimiters-unmatched-face))
  (set-face-attribute s nil :foreground "white" :background "red"))

;; Term mode colors
(add-hook 'term-mode-hook
          (cmd (set-face-attribute 'term-color-blue nil
                                   :foreground "#55aaff"
                                   :background "#55aaff")))

;;;; Key bindings
(defun leader+ (key) (kbd (concat "<C-return> " key)))

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
;; C-q - quoted-insert
;; C-=
;; C-`
;; C-Tab
;; C-S-Tab
;; F5
;; F6
;; F7
;; F8
;; F9
;; F12

(define-key global-map (leader+ "q") 'linum-relative-toggle)

;; Scroll by 5
(dolist (k '("C-S-e" "<C-down>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-down 5))))
(dolist (k '("C-S-y" "<C-up>"))
  (define-key global-map (kbd k) (cmd (evil-scroll-line-up 5))))

;; Better zoom in/out
(dolist (k '("C-x C-=" "C-x C--" "C-x C-0" "C-x C-+"))
  (define-key global-map (kbd k) 'zoom-in/out))

;; Open main work location in dired.
(define-key-cmd global-map (leader+ "a")
  (cond ((work-windows?)
         (find-file "C:/Users/IBM_ADMIN/VirtualBox Share/gitrepos"))
        ((work-linux?)
         (find-file "/media/sf_VirtualBox_Share/gitrepos"))
        ((home-linux?)
         (find-file "/ssh:chris@192.168.1.50:/home/chris/stuff"))))

(define-key global-map (leader+ "C-t") (cmd (term "/bin/bash")))
(define-key global-map (kbd "C-;") 'buffer-menu)
(define-key global-map (kbd "C-'") 'find-file)
(define-key global-map (kbd "C-,") 'async-shell-command)
(define-key global-map (leader+ "C-h") 'evil-window-left)
(define-key global-map (leader+ "C-l") 'evil-window-right)
(define-key global-map (leader+ "C-k") 'evil-window-up)
(define-key global-map (leader+ "C-j") 'evil-window-down)

;; Bindings for highlight-symbol
(define-key global-map (leader+ "C-w") 'highlight-symbol)
(define-key global-map (leader+ "w") 'highlight-symbol-remove-all)
(define-key global-map (kbd "C->") 'highlight-symbol-next)
(define-key global-map (kbd "C-<") 'highlight-symbol-prev)

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
      (popup-tip "No pep8 errors."))))


;;;; Useful Functions
(defun print-list (list)
  (dolist (item list)
    (princ item)
    (princ "\n")))

(defun lstrip (s)
  (let ((l (length s))
        (i 0))
    (while (and (< i l)
                (whitespace-char? (aref s i)))
      (setq i (1+ i)))
    (substring s i l)))

(defun rstrip (s)
  (let ((i (length s)))
    (while (and (> i 0) (whitespace-char? (aref s (1- i))))
      (setq i (1- i)))
    (substring s 0 i)))

(defun strip (s) (rstrip (lstrip s)))

(defun whitespace-char? (c) (or (= c ? ) (= c ?\t) (= c ?\n)))

(defun move-down-to-nonempty-line ()
  (while (and (not (at-last-line?)) (current-line-whitespace?))
    (forward-line 1)))

(defun move-up-to-nonempty-line ()
  (while (and (not (at-first-line?)) (current-line-whitespace?))
    (forward-line -1)))

(defun current-line-whitespace? ()
  (= (length (strip (current-line))) 0))

(defun at-first-line? () (= (line-beginning-position) 1))
(defun at-last-line? () (= (line-end-position) (buffer-end 1)))

(defun current-line ()
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))

(defun current-indent ()
  "Get the indent of the first non-blank line at or below point."
  (let ((old-pos (point)))
    (move-down-to-nonempty-line)
    (let ((s (current-line)))
      (goto-char old-pos)
      (- (length s) (length (lstrip s))))))

(defun str* (str n)
  "Repeat the string str n times."
  (let ((retval ""))
    (dotimes (i n) (setq retval (concat retval str)))
    retval))

(defun call-process-buffer-str (cmd &rest args)
  "Call an external program, sending the current buffer as input and returning
   the output as a string."
  (let ((b-name (format "temp-%d" (random 1000000000)))
        linum column message)
    (eval (append '(call-process-region 1 (buffer-end 1) cmd
                                        nil b-name nil)
                  args))
    (with-current-buffer b-name
      (let ((result (buffer-substring-no-properties 1 (buffer-end 1))))
        (kill-buffer b-name)
        result))))

(defun str-find-char (str char)
  "Return a list of the indices in str matching char."
  (let ((i 0) (idxs nil) (l (length str)))
    (while (< i l)
      (when (= (aref str i) char) (setq idxs (cons i idxs)))
      (setq i (1+ i)))
    (reverse idxs)))

(defun parse-pep8-line (line)
  "Return a list containing the line number, column, and message out of a pep8
   error line."
  (let ((idxs (str-find-char line ?:)))
    (list (-> line (substring (1+ (nth 0 idxs)) (nth 1 idxs)) string-to-number)
          (-> line (substring (1+ (nth 1 idxs)) (nth 2 idxs)) string-to-number)
          (substring line (1+ (nth 2 idxs))))))

(defun parse-pep8 (str) (-> str (split-string "\n") car parse-pep8-line))
