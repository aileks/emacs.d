;;; -*- lexical-binding: t -*-

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      ring-bell-function 'ignore
      visible-bell nil
      require-final-newline t)

(setq confirm-kill-emacs 'y-or-n-p)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; Better defaults
(electric-pair-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)
(recentf-mode 1)

;; Line numbers in code + writing
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)

(use-package olivetti
  :hook ((text-mode . olivetti-mode)
         (org-mode . olivetti-mode))
  :custom
  (olivetti-body-width 100)
  (olivetti-style 'fringe))

;; Prefer y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Scrolling
(setq scroll-margin 5
      scroll-conservatively 10000
      scroll-step 1
      scroll-preserve-screen-position t)

;; Indentation + print width
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100)

;; No dialog prompts
(setq use-dialog-box nil)

;; Keep scratch clean
(setq initial-scratch-message nil)

;; Avoid cursor blink
(blink-cursor-mode 0)

;; Save place in files
(save-place-mode 1)

(use-package dashboard
  :demand t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook)
  (defun aileks/dashboard-open (&optional frame)
    (when (frame-live-p frame)
      (select-frame frame))
    (dashboard-open))
  (add-hook 'elpaca-after-init-hook #'aileks/dashboard-open)
  (add-hook 'server-after-make-frame-hook #'aileks/dashboard-open))

