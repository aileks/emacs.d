;;; -*- lexical-binding: t -*-

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.3)
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
  :config
  (yas-reload-all))
(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yasnippet-snippets-initialize)
  (yas-reload-all))


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
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-d") #'consult-dir)

(global-set-key (kbd "C-c /") #'consult-ripgrep)
(global-set-key (kbd "C-c s l") #'consult-line)
(global-set-key (kbd "C-c s f") #'consult-find)
(global-set-key (kbd "C-c s m") #'consult-imenu)

(global-set-key (kbd "C-c p f") #'consult-project-extra-find)
(global-set-key (kbd "C-c p r") #'consult-project-extra-ripgrep)

(global-set-key (kbd "C-c g t") #'magit-todos-list)
(global-set-key (kbd "C-c g b") #'magit-blame-addition)
(global-set-key (kbd "C-c g y") #'browse-at-remote)

(global-set-key (kbd "C-c e r") #'eglot-rename)
(global-set-key (kbd "C-c e a") #'eglot-code-actions)
(global-set-key (kbd "C-c e o") #'eglot-code-action-organize-imports)
(global-set-key (kbd "C-c e h") #'eglot-show-call-hierarchy)
(global-set-key (kbd "C-c e t") #'eglot-show-type-hierarchy)
(global-set-key (kbd "C-c e d") #'eldoc-doc-buffer)
(global-set-key (kbd "C-c e f") #'eglot-format-buffer)
(global-set-key (kbd "C-c e D") #'flymake-show-project-diagnostics)

(global-set-key (kbd "C-c D") #'dape)
(global-set-key (kbd "C-c z") #'olivetti-mode)
(global-set-key (kbd "C-;") #'comment-line)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-j") #'avy-goto-char-timer)
(global-set-key (kbd "C-c a j") #'avy-goto-char-timer)
(global-set-key (kbd "C-?") #'undo-tree-redo)

(global-set-key (kbd "C-c h") #'windmove-left)
(global-set-key (kbd "C-c j") #'windmove-down)
(global-set-key (kbd "C-c k") #'windmove-up)
(global-set-key (kbd "C-c l") #'windmove-right)

(global-set-key (kbd "C-c d") #'dired-jump)
(global-set-key (kbd "C-c o") #'dired-toggle-ignored)

(global-set-key (kbd "C-c b") #'consult-buffer)
(global-set-key (kbd "C-c f") #'consult-project-extra-find)
(global-set-key (kbd "C-c L") #'elpaca-manager)
(global-set-key (kbd "C-c n") #'view-echo-area-messages)
(global-set-key (kbd "C-c q") #'quit-window)
(global-set-key (kbd "C-c r") #'apheleia-format-buffer)
(global-set-key (kbd "C-c u") #'undo-tree-visualize)
(global-set-key (kbd "C-c x") #'flymake-show-buffer-diagnostics)

(global-set-key (kbd "<f5>") #'dape-continue)
(global-set-key (kbd "<f10>") #'dape-next)
(global-set-key (kbd "<f11>") #'dape-step-in)
(global-set-key (kbd "<f12>") #'dape-step-out)

(global-set-key (kbd "C-c m") #'marginalia-cycle)
(global-set-key (kbd "C-c c") #'compile)
