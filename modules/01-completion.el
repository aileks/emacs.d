;;; -*- lexical-binding: t -*-

;; Vertico: vertical minibuffer completion
(use-package vertico
  :demand t
  :config (vertico-mode 1))

;; Corfu: in-buffer completion popup
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-min-width 30)
  (corfu-max-width 160)
  (corfu-popupinfo-delay '(0.2 . 0.5))
  :config
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil)
  (define-key corfu-map (kbd "<backtab>") nil)
  (define-key corfu-map (kbd "S-TAB") nil)
  (define-key corfu-map (kbd "<return>") #'corfu-insert)
  (set-face-attribute 'corfu-default nil :height 1.0)
  (set-face-attribute 'corfu-current nil :height 1.0)
  (set-face-attribute 'corfu-popupinfo nil :height 1.0))

;; Nerd icons: icons in completion + gutter
(use-package nerd-icons)
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (nerd-icons-completion-marginalia-setup))
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons t)
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.25 :scale 0.5 :background nil))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Orderless: fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; Marginalia: annotations in minibuffer
(use-package marginalia
  :init (marginalia-mode))

;; Consult: search + navigation
(use-package consult)
(use-package consult-dir
  :after consult)


;; Embark: contextual actions
(use-package embark
  :custom (prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult
  :after (embark consult))

;; Cape: completion at point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; Avy: jump to visible text
(use-package avy)

;; Helpful: better help buffers
(use-package helpful)


(use-package savehist
  :ensure nil
  :init (savehist-mode))

