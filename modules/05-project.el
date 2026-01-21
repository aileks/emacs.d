;;; -*- lexical-binding: t -*-

(setq project-mode-line t)

(use-package ibuffer-vc
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))
(use-package project
  :ensure nil)
(use-package consult-project-extra
   :after consult)
 
 ;; Auto-register projects on file open
 (defun auto-register-project ()
   "Auto-register current buffer's project if not already known."
   (when-let ((proj (project-current nil))
                 (root (project-root proj)))
     (unless (member root (project-known-project-roots))
       (project-remember-project proj)
       (message "Registered project: %s" root))))

 (add-hook 'after-change-major-mode-hook #'auto-register-project)
 
 ;; Dired + extensions
(require 'dired-x)
(setq dired-listing-switches "-alh --group-directories-first")
(setq dired-omit-mode nil)

(defun my-dired-gitignored-files ()
  (let* ((dir (dired-current-directory))
         (root (locate-dominating-file dir ".git")))
    (when root
      (let* ((default-directory root)
             (output (shell-command-to-string
                      "git ls-files -o -i --exclude-standard --directory"))
             (ignored (split-string output "\n" t))
             (dir-truename (file-truename dir)))
        (delq nil
              (mapcar (lambda (entry)
                        (let* ((abs (expand-file-name entry root))
                               (abs (file-truename (directory-file-name abs))))
                          (when (string-prefix-p dir-truename abs)
                            (let ((rel (file-relative-name abs dir)))
                              (unless (string-match-p "/" rel)
                                rel)))))
                      ignored))))))

(defun dired-toggle-ignored ()
  (interactive)
  (if dired-omit-mode
      (progn
        (setq-local dired-omit-files nil)
        (dired-omit-mode -1))
    (let ((ignored (my-dired-gitignored-files)))
      (setq-local dired-omit-files
                  (when ignored
                    (concat "\\`" (regexp-opt ignored) "\\'")))
      (dired-omit-mode 1)))
  (dired-revert))
