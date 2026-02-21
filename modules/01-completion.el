;;; -*- lexical-binding: t -*-

(use-package vertico
  :demand t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

(defun cfg/corfu-auto-enable ()
  (setq-local corfu-auto t))

(add-hook 'prog-mode-hook #'cfg/corfu-auto-enable)
(add-hook 'org-mode-hook #'cfg/corfu-auto-enable)

(use-package corfu
  :demand t
  :custom
  (corfu-auto nil)
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

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))
(setq orderless-matching-styles
      '(orderless-literal orderless-regexp orderless-flex))
;; Never auto-open *Completions*.
(setq completion-auto-help nil)

(use-package hotfuzz
  :after vertico
  :config
  (setq completion-category-overrides
        '((file   (styles hotfuzz orderless partial-completion))
          (buffer (styles hotfuzz orderless)))))

(use-package marginalia
  :bind (("C-c m" . marginalia-cycle))
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-c b" . consult-buffer)
         ("C-c /" . consult-ripgrep)
         ("C-c s l" . consult-line)
         ("C-c s f" . consult-find)
         ("C-c s m" . consult-imenu)))
(setq completion-in-region-function #'consult-completion-in-region)
(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)))

(use-package embark
  :bind (("C-." . embark-act))
  :custom (prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult
  :after (embark consult))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(global-set-key (kbd "M-/") #'dabbrev-expand)
(global-set-key (kbd "C-M-/") #'dabbrev-completion)

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)))
(use-package helpful)

(use-package savehist
  :ensure nil
  :init (savehist-mode))
