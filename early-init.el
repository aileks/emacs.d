;;; -*- lexical-binding: t -*-

(setq default-frame-alist '((undecorated . t)))
(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))
(add-to-list 'default-frame-alist '(font . "CommitMono Nerd Font Mono-16"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setenv "LC_COLLATE" "C")

;; Startup speed: boost GC threshold
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable slow file handlers during startup
(defvar my-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my-file-name-handler-alist)))

;; Misc
(setq native-comp-async-report-warnings-errors 'silent)
(setq package-enable-at-startup nil)

(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)
