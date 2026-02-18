;;; -*- lexical-binding: t -*-

(use-package which-key
  :defer 5
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode))

(use-package mood-line
  :config (mood-line-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo))
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

(defun cfg/command-safe-call (command &optional feature)
  "Call COMMAND interactively; require FEATURE first when needed."
  (unless (fboundp command)
    (when feature
      (require feature nil t)))
  (if (fboundp command)
      (call-interactively command)
    (user-error "Command `%s' unavailable%s"
                command
                (if feature (format " (package `%s')" feature) ""))))

(defmacro cfg/define-safe-command (name command &optional feature)
  "Define NAME as safe COMMAND wrapper."
  `(defun ,name ()
     (interactive)
     (cfg/command-safe-call ',command ',feature)))

(cfg/define-safe-command cfg/consult-buffer consult-buffer consult)
(cfg/define-safe-command cfg/consult-dir consult-dir consult-dir)
(cfg/define-safe-command cfg/consult-ripgrep consult-ripgrep consult)
(cfg/define-safe-command cfg/consult-line consult-line consult)
(cfg/define-safe-command cfg/consult-find consult-find consult)
(cfg/define-safe-command cfg/consult-imenu consult-imenu consult)
(cfg/define-safe-command cfg/consult-project-find consult-project-extra-find consult-project-extra)
(cfg/define-safe-command cfg/consult-project-ripgrep consult-project-extra-ripgrep consult-project-extra)
(cfg/define-safe-command cfg/magit-status magit-status magit)
(cfg/define-safe-command cfg/magit-todos magit-todos-list magit-todos)
(cfg/define-safe-command cfg/magit-blame magit-blame-addition magit)
(cfg/define-safe-command cfg/browse-at-remote browse-at-remote browse-at-remote)
(cfg/define-safe-command cfg/eglot-rename eglot-rename eglot)
(cfg/define-safe-command cfg/eglot-actions eglot-code-actions eglot)
(cfg/define-safe-command cfg/eglot-organize eglot-code-action-organize-imports eglot)
(cfg/define-safe-command cfg/eglot-call-hierarchy eglot-show-call-hierarchy eglot)
(cfg/define-safe-command cfg/eglot-type-hierarchy eglot-show-type-hierarchy eglot)
(cfg/define-safe-command cfg/eglot-format eglot-format-buffer eglot)
(cfg/define-safe-command cfg/dape dape dape)
(cfg/define-safe-command cfg/dape-continue dape-continue dape)
(cfg/define-safe-command cfg/dape-next dape-next dape)
(cfg/define-safe-command cfg/dape-step-in dape-step-in dape)
(cfg/define-safe-command cfg/dape-step-out dape-step-out dape)
(cfg/define-safe-command cfg/olivetti olivetti-mode olivetti)
(cfg/define-safe-command cfg/embark-act embark-act embark)
(cfg/define-safe-command cfg/avy avy-goto-char-timer avy)
(cfg/define-safe-command cfg/undo-redo undo-tree-redo undo-tree)
(cfg/define-safe-command cfg/undo-visualize undo-tree-visualize undo-tree)
(cfg/define-safe-command cfg/consult-notes consult-notes consult-notes)
(cfg/define-safe-command cfg/apheleia-format apheleia-format-buffer apheleia)
(cfg/define-safe-command cfg/jupyter-run-repl jupyter-run-repl jupyter)
(cfg/define-safe-command cfg/jupyter-connect-repl jupyter-connect-repl jupyter)
(cfg/define-safe-command cfg/jupyter-associate-buffer jupyter-repl-associate-buffer jupyter)
(cfg/define-safe-command cfg/marginalia-cycle marginalia-cycle marginalia)
(cfg/define-safe-command cfg/move-text-up move-text-up move-text)
(cfg/define-safe-command cfg/move-text-down move-text-down move-text)

(use-package dape
  :defer t)

;; Misc keybinds
(global-set-key (kbd "C-x b") #'cfg/consult-buffer)
(global-set-key (kbd "C-x g") #'cfg/magit-status)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-d") #'cfg/consult-dir)

(global-set-key (kbd "C-c /") #'cfg/consult-ripgrep)
(global-set-key (kbd "C-c s l") #'cfg/consult-line)
(global-set-key (kbd "C-c s f") #'cfg/consult-find)
(global-set-key (kbd "C-c s m") #'cfg/consult-imenu)

(global-set-key (kbd "C-c p f") #'cfg/consult-project-find)
(global-set-key (kbd "C-c p r") #'cfg/consult-project-ripgrep)

(global-set-key (kbd "C-c g t") #'cfg/magit-todos)
(global-set-key (kbd "C-c g b") #'cfg/magit-blame)
(global-set-key (kbd "C-c g y") #'cfg/browse-at-remote)

(global-set-key (kbd "C-c e r") #'cfg/eglot-rename)
(global-set-key (kbd "C-c e a") #'cfg/eglot-actions)
(global-set-key (kbd "C-c e o") #'cfg/eglot-organize)
(global-set-key (kbd "C-c e h") #'cfg/eglot-call-hierarchy)
(global-set-key (kbd "C-c e t") #'cfg/eglot-type-hierarchy)
(global-set-key (kbd "C-c e d") #'eldoc-doc-buffer)
(global-set-key (kbd "C-c e f") #'cfg/eglot-format)
(global-set-key (kbd "C-c e D") #'flymake-show-project-diagnostics)

(global-set-key (kbd "C-c D") #'cfg/dape)
(global-set-key (kbd "C-c z") #'cfg/olivetti)
(global-set-key (kbd "C-;") #'comment-line)
(global-set-key (kbd "C-.") #'cfg/embark-act)
(global-set-key (kbd "M-j") #'cfg/avy)
(global-set-key (kbd "C-?") #'cfg/undo-redo)

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(global-set-key (kbd "C-c h") #'windmove-left)
(global-set-key (kbd "C-c j") #'windmove-down)
(global-set-key (kbd "C-c k") #'windmove-up)
(global-set-key (kbd "C-c l") #'windmove-right)

(global-set-key (kbd "C-c d") #'dired-jump)
(global-set-key (kbd "C-c o") #'dired-toggle-gitignored)

(global-set-key (kbd "C-c b") #'cfg/consult-buffer)
(global-set-key (kbd "C-c f") #'cfg/consult-project-find)
(global-set-key (kbd "C-c L") #'elpaca-manager)
(global-set-key (kbd "C-c N") #'cfg/consult-notes)
(global-set-key (kbd "C-c n") #'view-echo-area-messages)
(global-set-key (kbd "C-c q") #'quit-window)
(global-set-key (kbd "C-c r") #'cfg/apheleia-format)
(global-set-key (kbd "C-c u") #'cfg/undo-visualize)
(global-set-key (kbd "C-c x") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c y r") #'cfg/jupyter-run-repl)
(global-set-key (kbd "C-c y c") #'cfg/jupyter-connect-repl)
(global-set-key (kbd "C-c y a") #'cfg/jupyter-associate-buffer)

(global-set-key (kbd "<f5>") #'cfg/dape-continue)
(global-set-key (kbd "<f10>") #'cfg/dape-next)
(global-set-key (kbd "<f11>") #'cfg/dape-step-in)
(global-set-key (kbd "<f12>") #'cfg/dape-step-out)

(global-set-key (kbd "C-c m") #'cfg/marginalia-cycle)
(global-set-key (kbd "C-c p c") #'compile)

(global-set-key (kbd "M-p") #'cfg/move-text-up)
(global-set-key (kbd "M-n") #'cfg/move-text-down)
