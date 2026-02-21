;;; -*- lexical-binding: t -*-

(use-package eglot
  :ensure nil
  :commands (eglot-rename
             eglot-code-actions
             eglot-code-action-organize-imports
             eglot-show-call-hierarchy
             eglot-show-type-hierarchy
             eglot-format-buffer)
  :bind (("C-c e r" . eglot-rename)
         ("C-c e a" . eglot-code-actions)
         ("C-c e o" . eglot-code-action-organize-imports)
         ("C-c e h" . eglot-show-call-hierarchy)
         ("C-c e t" . eglot-show-type-hierarchy)
         ("C-c e f" . eglot-format-buffer))
  :hook ((python-mode python-ts-mode python-base-mode
          julia-mode
          ess-r-mode
          c++-mode c++-ts-mode)
         . eglot-ensure)
  :config
  (setq eglot-connect-timeout 30
        eglot-autoshutdown nil
        eglot-ignored-server-capabilities nil
        eglot-code-action-indications '(eldoc-hint)))

(use-package eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  ;; Keep Julia on `julia-mode` until julia-ts-mode/grammar ABI settles.
  (setq treesit-auto-langs (remove 'julia treesit-auto-langs))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-view-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode)
(use-package json-mode)
(use-package toml-mode)

(use-package ess
  :mode (("\\.[rR]\\'" . ess-r-mode)
         ("\\.Rprofile\\'" . ess-r-mode)))

(use-package julia-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
  :mode "\\.jl\\'")

(use-package eglot-jl
  :after eglot
  :config
  (eglot-jl-init))

(use-package quarto-mode
  :mode (("\\.qmd\\'" . poly-quarto-mode)
         ("\\.Rmd\\'" . poly-quarto-mode)))

(use-package apheleia
  :bind (("C-c r" . apheleia-format-buffer))
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--assume-filename" filepath))
  (setf (alist-get 'clang-format apheleia-mode-alist)
        '(c++-mode c++-ts-mode))
  (setf (alist-get 'ruff-format apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-format apheleia-mode-alist)
        '(python-mode python-ts-mode python-base-mode))
  (add-hook 'prog-mode-hook #'apheleia-mode))

(use-package flymake
  :ensure nil
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicator-position 'left-margin)
  (flymake-margin-indicators-string
   '((error "" compilation-error)
     (warning "" compilation-warning)
     (note "" compilation-info))))

(with-eval-after-load 'eglot
  (dolist (entry
           '(((c++-mode c++-ts-mode) . ("clangd"))
              ((python-mode python-ts-mode python-base-mode) . ("basedpyright-langserver" "--stdio"))
             ((ess-r-mode :language-id "r")
              . ("R" "--no-echo" "-e" "languageserver::run()"))))
    (add-to-list 'eglot-server-programs entry)))

(dolist (entry
         '((c++-mode    . c++-ts-mode)
           (python-mode . python-ts-mode)))
  (add-to-list 'major-mode-remap-alist entry))
