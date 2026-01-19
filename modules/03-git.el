;;; -*- lexical-binding: t -*-

;; Magit: minimal git client
(use-package transient)
(use-package magit
  :after transient
  :commands (magit-status magit-dispatch)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*magit" (one-or-more anything) "*")
                 (display-buffer-below-selected)
                 (window-height . 0.4))))
(use-package magit-todos
  :after magit
  :custom (magit-todos-exclude-globs '("node_modules" "dist" "build"))
  :config (magit-todos-mode 1))

;; diff-hl: better git diff highlighting
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :custom
  (diff-hl-margin-symbols-alist
   '((insert . "")
     (delete . "")
     (change . "")
     (unknown . "")
     (ignored . "")
     (reference . " ")))
  :custom-face
  (diff-hl-margin-change ((t (:foreground "#e5c07b"))))
  (diff-hl-margin-ignored ((t (:foreground "#5c6370"))))
  :config
  (diff-hl-flydiff-mode 1)
  (diff-hl-margin-mode 1)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Forge: interaction with sites like GitHub, GitLab, etc.
(use-package forge
  :after magit
  :init
  (setq forge-owned-accounts nil))

(use-package git-modes)
