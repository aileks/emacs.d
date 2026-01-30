;;; -*- lexical-binding: t -*-

(use-package eglot
  :ensure nil
  :hook ((c-mode c-ts-mode
                 c++-mode c++-ts-mode
                 zig-mode zig-ts-mode
                 python-mode python-ts-mode
                 sh-mode bash-ts-mode
                 js-mode js-ts-mode
                 typescript-mode typescript-ts-mode
                 tsx-mode tsx-ts-mode
                 css-mode css-ts-mode
                 web-mode html-ts-mode)
         . eglot-ensure)
  :config
  (setq eglot-connect-timeout 30
        eglot-autoshutdown nil
        eglot-ignored-server-capabilities nil))

(use-package eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode"))
(with-eval-after-load 'zig-ts-mode
  (define-key zig-ts-mode-map (kbd "M-[") #'beginning-of-defun)
  (define-key zig-ts-mode-map (kbd "M-]") #'end-of-defun))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-view-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode)
(use-package json-mode)
(use-package toml-mode)

;; lean4 mode specific setup
(use-package dash :ensure t)
(use-package lsp-mode :ensure t)
(use-package magit-section :ensure t)
(use-package lean4-mode
  :commands (lean4-mode)
  :ensure (lean4-mode :host github
                      :repo "leanprover-community/lean4-mode"
                      :files ("*.el" "data"))
  :hook (lean4-mode . lsp-deferred))

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (setf (alist-get 'prettier apheleia-mode-alist)
        '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-mode tsx-ts-mode))
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--assume-filename" filepath))
  (setf (alist-get 'clang-format apheleia-mode-alist)
        '(c-mode c-ts-mode c++-mode c++-ts-mode))
  (setf (alist-get 'black apheleia-formatters)
        '("black" "-"))
  (setf (alist-get 'black apheleia-mode-alist)
        '(python-mode python-ts-mode))
  (setf (alist-get 'shfmt apheleia-mode-alist)
        '(sh-mode bash-ts-mode))
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
(use-package flymake-shellcheck
  :hook (sh-mode . flymake-shellcheck-load)
  :custom (flymake-shellcheck-checker 'sh-shellcheck))

(with-eval-after-load 'eglot
  (dolist (entry
           '(((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd"))
             ((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))
             ((sh-mode bash-ts-mode) . ("bash-language-server" "start"))

             ((js-mode js-ts-mode typescript-mode typescript-ts-mode)
              . ("typescript-language-server" "--stdio"))
             (((tsx-mode :language-id "typescriptreact")
               (tsx-ts-mode :language-id "typescriptreact"))
              . ("typescript-language-server" "--stdio"))

             ((web-mode html-ts-mode) . ("vscode-html-language-server" "--stdio"))
             ((css-mode css-ts-mode)  . ("vscode-css-language-server" "--stdio"))

             ((zig-ts-mode :language-id "zig") . ("zls"))))
    (add-to-list 'eglot-server-programs entry)))

(setopt major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (python-mode     . python-ts-mode)
          (js-mode         . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (sh-mode         . bash-ts-mode)))
