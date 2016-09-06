(provide 'cw-utils)

;; Macros
(defmacro comment (&rest args) nil)
(defmacro cmd (&rest args) `(lambda () (interactive) (progn ,@args)))
(defmacro define-key-cmd (keymap key &rest body)
  `(define-key ,keymap ,key (lambda () (interactive) (progn ,@body))))

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (let ((lform (list-wrap form)))
        (setq result (append lform (list result))))))) ;

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (let ((lform (list-wrap form)))
        (setq result (append (list (car lform) result)
                             (cdr lform)))))))

(defconst osx? (eq system-type 'darwin))

(defun list-wrap (x) (if (listp x) x (list x)))

(defun print-list (list)
  (dolist (item list)
    (princ item)
    (princ "\n")))

(defun whitespace-char? (c) (or (= c ? ) (= c ?\t) (= c ?\n)))

(defun move-down-to-nonempty-line ()
  (while (and (not (at-last-line?)) (current-line-whitespace?))
    (forward-line 1)))

(defun move-up-to-nonempty-line ()
  (while (and (not (at-first-line?)) (current-line-whitespace?))
    (forward-line -1)))

(defun current-line-whitespace? ()
  (= (length (s-trim (current-line))) 0))

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
      (- (length s) (length (s-trim-left s))))))

(defun call-process-buffer-str (cmd &rest args)
  "Call an external program, sending the current buffer as input and returning
   the output as a string."
  (--call-process cmd args '(call-process-region 1 (buffer-end 1))))

(defun call-process-str (cmd &rest args)
  "Call an external program and return the output as a string"
  (--call-process cmd args '(call-process)))

(defun --call-process (cmd args call-expr)
  (let ((b-name (format "temp-%d" (random 1000000000)))
        linum column message result)
    (eval (append call-expr '(cmd nil b-name nil) args))
    (setq result (buffer-to-str b-name))
    (kill-buffer b-name)
    result))

(defun buffer-to-str (b-name)
  "Return the text in a buffer as a string"
    (with-current-buffer b-name
      (buffer-substring-no-properties 1 (buffer-end 1))))

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

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Emacs pager support
;; (defun ep-revert-buffer ()
;;   (when (and (get-buffer ep-current-buffer)
;;              (equal (buffer-name) ep-current-buffer))
;;     (revert-buffer t t t))
;;   (run-at-time "1 sec" ep-revert-buffer))
;; (setq ep-current-buffer nil)

(defun leader+ (key) (kbd (concat "<C-return> " key)))
