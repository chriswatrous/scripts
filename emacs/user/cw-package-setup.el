(provide 'cw-package-setup)

(require 'package)

(setq package-archives
      '(("melpa stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("elpy" . "https://jorgenschaefer.github.io/packages/")))
(package-initialize)
