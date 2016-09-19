(provide 'cw-symbols)

(require 'cw-utils)

(defun show-symbols (&optional pred)
  (buffer-print-list
   "*symbols*"
   (-sort 'string< (if pred (-filter pred (get-symbols)) (get-symbols)))))

(defun buffer-print-list (buffername items)
  (with-current-buffer (get-buffer-create buffername)
    (erase-buffer)
    (dolist (x items)
      (insert (if (stringp x) x (prin1-to-string x)))
      (insert ?\n))
    (goto-char 1))
  (display-buffer buffername))

(defun get-symbols ()
  (let ((out nil))
    (mapatoms (lambda (x) (push x out)))
    out))

(defun symbol-search (pattern)
  (show-symbols (lambda (x) (s-match pattern (symbol-name x)))))

(symbol-search ".-map$" (symbol-name x))
(show-symbols (lambda (x) (and (boundp x) (keymapp (symbol-value x)))))
(symbol-search "^evil-.*-map$")
