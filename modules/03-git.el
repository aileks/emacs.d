;;; -*- lexical-binding: t -*-

(use-package transient)
(use-package magit
  :after transient
  :bind (("C-x g" . magit-status)
         ("C-c g b" . magit-blame-addition))
  :commands (magit-status magit-dispatch magit-push))

(use-package magit-todos
  :after magit
  :bind (("C-c g t" . magit-todos-list))
  :commands (magit-todos-list))

(use-package browse-at-remote
  :after magit
  :bind (("C-c g y" . browse-at-remote))
  :commands (browse-at-remote))

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

(use-package forge
  :after magit
  :init
  (setq forge-owned-accounts nil))

(use-package git-modes)
