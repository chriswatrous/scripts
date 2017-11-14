(provide 'cw-colors)

;;;; Info
;; (list-colors-display)   ; list color names
;; (list-faces-display)    ; list current faces

(set-face-attribute 'cursor nil :background "#00ff00")

; search highlight color
(set-face-attribute 'evil-ex-lazy-highlight nil
                    :background "#008800"
                    :foreground "#ffffff")

;; default colors for new frames
(add-to-list 'default-frame-alist '(foreground-color . "#E0E0E0"))
(add-to-list 'default-frame-alist '(background-color . "#000000"))

(set-face-attribute 'linum nil :foreground "#00dd00")
(set-face-attribute 'fringe nil :background "#111133")
(set-face-attribute 'highlight nil :background "#005500")

;; cider
(setq cider-stacktrace-frames-background-color "black")

;; term
(add-hook 'term-mode-hook
          (cmd (set-face-attribute 'term-color-blue nil
                                   :foreground "#55aaff"
                                   :background "#55aaff")))


;; colunm-marker
(set-face-attribute 'column-marker-1 nil :background "red" :foreground "white")


;;;; rainbow-delimiters-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
  (defconst rainbow-colors
    '("#ff0000"
      "#00ff00"
      "#0066ff"
      "#eeee00"
      "#ff00ff"
      "#00ff99"
      "#ff6600"
      "#666666"
      "#7744ff"))
  (let ((i 1))
    (dolist (c rainbow-colors)
      (set-face-attribute
       (intern (format "rainbow-delimiters-depth-%d-face" i))
       nil :foreground c)
      (setq i (1+ i)))))
(dolist (s '(rainbow-delimiters-mismatched-face
             rainbow-delimiters-unmatched-face))
  (set-face-attribute s nil :foreground "white" :background "red"))

(dolist (hook '(emacs-lisp-mode-hook
                python-mode-hook
                clojure-mode-hook
                go-mode-hook
                haskell-mode-hook
                yaml-mode-hook
                json-mode-hook
                js-mode-hook
                ielm-mode-hook))
  (add-hook hook #'rainbow-delimiters-mode))

;; Turn on column marker.
(dolist (hook '(emacs-lisp-mode-hook
                clojure-mode-hook
                go-mode-hook
                haskell-mode-hook
                yaml-mode-hook
                json-mode-hook
                ielm-mode-hook
                markdown-mode-hook))
  (add-hook hook (lambda () (column-marker-1 80))))
(add-hook 'js-mode-hook (lambda () (column-marker-1 150)))
(add-hook 'python-mode-hook (lambda () (column-marker-1 79)))
