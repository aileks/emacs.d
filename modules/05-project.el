;;; -*- lexical-binding: t -*-

;; Project management (built-in)
(setq project-mode-line t)

(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package project
  :ensure nil
  :bind-keymap ("C-x p" . project-prefix-map))

(use-package consult-project-extra
  :after consult)

;; Dired + extensions
(require 'dired-x)
(setq dired-listing-switches "-al")
(setq dired-omit-files "^\\.?#\\|^\\..*$")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)

(defun aileks/dired-omit-mode-toggle ()
  (interactive)
  (if dired-omit-mode
      (dired-omit-mode -1)
    (dired-omit-mode 1))
  (revert-buffer))

(global-set-key (kbd "C-x C-b") #'ibuffer)
