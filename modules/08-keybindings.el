;;; -*- lexical-binding: t -*-

(use-package which-key
  :demand t
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode 1))

(use-package mood-line
  :config (mood-line-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-c u" . undo-tree-visualize))
  :hook (after-init . (lambda () (setq undo-tree-visualizer-diff t))))

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

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package dape
  :defer t
  :bind (("C-c D" . dape)
         ("<f5>" . dape-continue)
         ("<f10>" . dape-next)
         ("<f11>" . dape-step-in)
         ("<f12>" . dape-step-out)))

;; Keep deferred package commands callable from global keymaps.
(autoload 'consult-buffer "consult" nil t)
(autoload 'consult-ripgrep "consult" nil t)
(autoload 'consult-line "consult" nil t)
(autoload 'consult-find "consult" nil t)
(autoload 'consult-imenu "consult" nil t)
(autoload 'consult-dir "consult-dir" nil t)
(autoload 'consult-project-extra-find "consult-project-extra" nil t)
(autoload 'consult-project-extra-ripgrep "consult-project-extra" nil t)
(autoload 'magit-status "magit" nil t)
(autoload 'magit-todos-list "magit-todos" nil t)
(autoload 'browse-at-remote "browse-at-remote" nil t)
(autoload 'dape "dape" nil t)
(autoload 'dape-continue "dape" nil t)
(autoload 'dape-next "dape" nil t)
(autoload 'dape-step-in "dape" nil t)
(autoload 'dape-step-out "dape" nil t)
(autoload 'consult-notes "consult-notes" nil t)
(autoload 'apheleia-format-buffer "apheleia" nil t)
(autoload 'jupyter-run-repl "jupyter" nil t)
(autoload 'jupyter-connect-repl "jupyter" nil t)
(autoload 'jupyter-repl-associate-buffer "jupyter" nil t)
(autoload 'embark-act "embark" nil t)
(autoload 'avy-goto-char-timer "avy" nil t)
(autoload 'marginalia-cycle "marginalia" nil t)

;; Misc keybinds
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
(global-set-key (kbd "<f5>") #'dape-continue)
(global-set-key (kbd "<f10>") #'dape-next)
(global-set-key (kbd "<f11>") #'dape-step-in)
(global-set-key (kbd "<f12>") #'dape-step-out)
(global-set-key (kbd "C-c z") #'olivetti-mode)
(global-set-key (kbd "C-;") #'comment-line)
(global-set-key (kbd "C-.") #'embark-act)
(global-set-key (kbd "M-j") #'avy-goto-char-timer)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(global-set-key (kbd "C-c h") #'windmove-left)
(global-set-key (kbd "C-c j") #'windmove-down)
(global-set-key (kbd "C-c k") #'windmove-up)
(global-set-key (kbd "C-c l") #'windmove-right)

(global-set-key (kbd "C-c d") #'dired-jump)
(global-set-key (kbd "C-c o") #'dired-toggle-gitignored)

(global-set-key (kbd "C-c b") #'consult-buffer)
(global-set-key (kbd "C-c f") #'consult-project-extra-find)
(global-set-key (kbd "C-c L") #'elpaca-manager)
(global-set-key (kbd "C-c N") #'consult-notes)
(global-set-key (kbd "C-c n") #'view-echo-area-messages)
(global-set-key (kbd "C-c q") #'quit-window)
(global-set-key (kbd "C-c r") #'apheleia-format-buffer)
(global-set-key (kbd "C-c x") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c y r") #'jupyter-run-repl)
(global-set-key (kbd "C-c y c") #'jupyter-connect-repl)
(global-set-key (kbd "C-c y a") #'jupyter-repl-associate-buffer)
(global-set-key (kbd "C-c m") #'marginalia-cycle)
(global-set-key (kbd "C-c p c") #'compile)
