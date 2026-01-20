;;; -*- lexical-binding: t -*-

;; Vertico: vertical minibuffer completion
(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

;; Corfu: in-buffer completion popup
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.2 . 0.5))
  (corfu-popupinfo-direction '(left right vertical))
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

;; Orderless: general matching everywhere
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))
(setq orderless-matching-styles
      '(orderless-literal orderless-regexp orderless-flex))

;; Hotfuzz: scored fuzzy for files/buffers
(use-package hotfuzz
  :after vertico
  :config
  (setq completion-category-overrides
        '((file   (styles hotfuzz orderless partial-completion))
          (buffer (styles hotfuzz orderless)))))

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

