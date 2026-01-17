;;; -*- lexical-binding: t -*-

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.6)
  :config (which-key-mode))

;; mood-line: status bar
(use-package mood-line
  :config (mood-line-mode))

;; undo-fu: better undo/redo
(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

;; yasnippet: code snippets
(use-package yasnippet
  :config (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

;; ws-butler: whitespace management
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; dape: dubugging client
(use-package dape
  :bind ("C-c d" . dape))

;; eat: terminal in case I need one
(use-package eat
  :bind ("C-c t" . eat))

(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-c s l") #'consult-line)
(global-set-key (kbd "C-c s r") #'consult-ripgrep)
(global-set-key (kbd "C-c s f") #'consult-find)
(global-set-key (kbd "C-c s m") #'consult-imenu)
(global-set-key (kbd "C-c z") #'olivetti-mode)
(global-set-key (kbd "C-c o") #'aileks/dired-toggle-ignored)
(global-set-key (kbd "C-c p f") #'consult-project-extra-find)
(global-set-key (kbd "C-c p r") #'consult-project-extra-ripgrep)
(global-set-key (kbd "C-c g t") #'magit-todos-list)
(global-set-key (kbd "C-c g b") #'magit-blame-addition)
(global-set-key (kbd "C-c g y") #'browse-at-remote)
(global-set-key (kbd "C-c o") #'aileks/dired-toggle-ignored)
