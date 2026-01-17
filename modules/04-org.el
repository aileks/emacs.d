;;; -*- lexical-binding: t -*-

(use-package org
  :ensure nil
  :config
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "inbox.org"))
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

(require 'org-tempo)
