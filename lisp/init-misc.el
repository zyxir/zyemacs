;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; Miscellaneous major modes/

;;; Code:

(use-package matlab
  :straight matlab-mode
  :mode "\\.m\\'"
  :config
  (add-hook 'matlab-mode-hook
	    (lambda ()
	      (auto-fill-mode t)
	      (display-line-numbers-mode t))))

(use-package plantuml-mode
  :straight t
  :config
  (setq plantuml-jar-path (concat zy/3rd-party-path "plantuml/plantuml.jar")
	plantuml-default-exec-mode 'jar))

;; End of config.

(provide 'init-misc)