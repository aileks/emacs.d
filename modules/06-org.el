;;; -*- lexical-binding: t -*-

;; read_when: update this file when changing note layout, capture flow, or Org export defaults.

(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/Notes/org")
        org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-agenda-files
        (list (expand-file-name "inbox.org" org-directory)
              (expand-file-name "projects.org" org-directory)
              (expand-file-name "calendar.org" org-directory)
              (expand-file-name "notes.org" org-directory))
        org-log-done 'time
        org-hide-emphasis-markers t
        org-startup-indented t
        org-ellipsis "..."
        org-pretty-entities t
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-preview-latex-default-process 'dvisvgm
        org-latex-pdf-process
        '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
        org-babel-python-command "python3")
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (setq org-capture-templates
        `(("t" "Task -> inbox" entry
           (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
          ("n" "Note -> notes" entry
           (file ,(expand-file-name "notes.org" org-directory))
           "* %^{Title}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
          ("p" "Project note" entry
           (file ,(expand-file-name "projects.org" org-directory))
           "* %^{Project}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")
          ("r" "Reading note" entry
           (file ,(expand-file-name "reading.org" org-directory))
           "* %^{Source}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")))
  (setq org-refile-targets
        `((,(expand-file-name "projects.org" org-directory) :maxlevel . 3)
          (,(expand-file-name "notes.org" org-directory) :maxlevel . 3)))
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (julia . t))))

(use-package tex
  :ensure auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-PDF-mode t))

(use-package cdlatex
  :hook ((org-mode . org-cdlatex-mode)
         (LaTeX-mode . turn-on-cdlatex)))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package denote
  :defer t
  :commands (denote)
  :init
  (setq denote-directory org-directory
        denote-known-keywords '("math" "statistics" "ml" "research" "project")
        denote-infer-keywords t
        denote-sort-keywords t)
  :config
  (denote-rename-buffer-mode 1))

(use-package consult-notes
  :ensure (:host github :repo "mclear-tools/consult-notes")
  :after consult
  :commands (consult-notes consult-notes-search-in-all-notes)
  :init
  (setq consult-notes-file-dir-sources
        `(("Org" ?o ,org-directory)))
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode 1)))
