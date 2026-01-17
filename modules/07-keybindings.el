;;; -*- lexical-binding: t -*-

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

;; mood-line: status bar
(use-package mood-line
  :config (mood-line-mode))

;; undo-tree: undo/redo + visualizer
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo))
  :hook (after-init . (lambda () (setq undo-tree-visualizer-diff t))))

;; yasnippet: code snippets
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :bind (:map yas-minor-mode-map
              ("TAB" . yas-next-field-or-maybe-expand)
              ("<tab>" . yas-next-field-or-maybe-expand)
              ("<backtab>" . yas-prev-field)
              ("S-TAB" . yas-prev-field))
  :config
  (yas-reload-all))
(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yasnippet-snippets-initialize)
  (yas-reload-all))

;; ws-butler: whitespace management
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; multiple-cursors: multi-edit
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; dape: debugging client
(use-package dape
  :bind (("C-c D" . dape)
         ("<f5>" . dape-continue)
         ("<f10>" . dape-next)
         ("<f11>" . dape-step-in)
         ("<f12>" . dape-step-out)))

;; Remaining keybinds
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c /") #'consult-ripgrep)
(global-set-key (kbd "C-c b") #'consult-buffer)
(global-set-key (kbd "C-c d") #'dired-jump)
(global-set-key (kbd "C-c f") #'consult-project-extra-find)
(global-set-key (kbd "C-c h") #'windmove-left)
(global-set-key (kbd "C-c j") #'windmove-down)
(global-set-key (kbd "C-c k") #'windmove-up)
(global-set-key (kbd "C-c l") #'windmove-right)
(global-set-key (kbd "C-c L") #'elpaca-manager)
(global-set-key (kbd "C-c n") #'view-echo-area-messages)
(global-set-key (kbd "C-c o") #'dired-toggle-ignored)
(global-set-key (kbd "C-c q") #'quit-window)
(global-set-key (kbd "C-c r") #'apheleia-format-buffer)
(global-set-key (kbd "C-c t") #'eat)
(global-set-key (kbd "C-c u") #'undo-tree-visualize)
(global-set-key (kbd "C-c x") #'flymake-show-diagnostics-buffer)
(global-set-key (kbd "C-c z") #'olivetti-mode)
(global-set-key (kbd "C-c s l") #'consult-line)
(global-set-key (kbd "C-c s f") #'consult-find)
(global-set-key (kbd "C-c s m") #'consult-imenu)
(global-set-key (kbd "C-c p f") #'consult-project-extra-find)
(global-set-key (kbd "C-c p r") #'consult-project-extra-ripgrep)
(global-set-key (kbd "C-c g t") #'magit-todos-list)
(global-set-key (kbd "C-c g b") #'magit-blame-addition)
(global-set-key (kbd "C-c g y") #'browse-at-remote)
