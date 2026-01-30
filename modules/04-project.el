;;; -*- lexical-binding: t; -*-

(setq-default project-mode-line t)

(use-package ibuffer-vc
  :hook (ibuffer-hook . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package project)

(use-package consult-project-extra
  :after consult
  :custom (consult-project-function #'consult-project-extra-project-fn)
  :bind (("C-c p f" . consult-project-extra-find)
         ("C-c p o" . consult-project-extra-find-other-window)))

(defun project-auto-remember ()
  (when-let* ((proj (project-current nil))
              (root (file-name-as-directory (file-truename (project-root proj)))))
    (unless (or (file-remote-p root)
                (seq-some (lambda (p) (file-equal-p root p))
                          (project-known-project-roots)))
      (project-remember-project proj))))

(add-hook 'find-file-hook #'project-auto-remember)
(add-hook 'dired-mode-hook #'project-auto-remember)

(require 'dired-x)

(setq dired-listing-switches "-alh")

(defvar-local dired--saved-omit-files nil
  "Saved value of `dired-omit-files' before toggling gitignored entries.")

(defun dired-gitignored-names ()
  (let* ((dir (file-truename (dired-current-directory)))
         (root (locate-dominating-file dir ".git")))
    (when root
      (let* ((default-directory root)
             (rel (file-relative-name dir root))
             ;; Restrict to this directory for speed:
             (args (append '("ls-files" "-o" "-i" "--exclude-standard" "--directory")
                           (unless (member rel '("./" "")) (list "--" rel))))
             (entries (apply #'process-lines "git" args)))
        (delq nil
              (mapcar (lambda (entry)
                        (let* ((abs (file-truename (directory-file-name
                                                    (expand-file-name entry root)))))
                          (when (file-in-directory-p abs dir)
                            (let ((name (file-relative-name abs dir)))
                              (unless (string-match-p "/" name) name)))))
                      entries))))))

(defun dired-toggle-gitignored ()
  (interactive)
  (if dired-omit-mode
      (progn
        (setq-local dired-omit-files dired--saved-omit-files)
        (setq-local dired--saved-omit-files nil)
        (dired-omit-mode -1))
    (setq-local dired--saved-omit-files dired-omit-files)
    (let ((ignored (dired-gitignored-names)))
      (setq-local dired-omit-files
                  (and ignored (concat "\\`" (regexp-opt ignored) "\\'")))
      (dired-omit-mode 1)))
  (revert-buffer))
