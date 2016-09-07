
(let ((a nil))
  (mapatoms (lambda (x) (push x a)))
  (print-list a))
(setq c 1)

(setq a (mapatoms (lambda (x) (symbol-name x))))
(print-list a)

(defun show-symbols (pred)
  (let ((buf (get-buffer-create "*symbols*")))
    (with-current-buffer buf (erase-buffer))
    (mapatoms (lambda (x)
                (when (funcall pred x)
                  (with-current-buffer buf
                    (insert (symbol-name x))
                    (insert "\n")))))
    (with-current-buffer buf (sort-lines nil 1 (point-max)))
    (display-buffer buf)
    (goto-char 1)))

(show-symbols (lambda (x) (and (boundp x) (keymapp (symbol-value x)))))
