;;; -*- lexical-binding: t -*-

(defconst cfg/config-required-executables
  '("julia"
    "R"
    "python3"
    "basedpyright-langserver"
    "ruff"
    "clangd"
    "clang-format"
    "jupyter"
    "jupytext"
    "latexmk"
    "pdflatex")
  "Executables required by this config.")

(defconst cfg/config-org-bootstrap-files
  '("inbox.org" "projects.org" "calendar.org" "notes.org" "reading.org")
  "Files that must exist under `org-directory'.")

(defvar cfg/config-last-health nil
  "Most recent result from `cfg/config-health-snapshot'.")

(defun cfg/config--org-directory ()
  "Resolve Org directory robustly."
  (expand-file-name (if (and (boundp 'org-directory) org-directory)
                        org-directory
                      "~/Notes/org")))

(defun cfg/config-check-executables ()
  "Return status of required executables."
  (let (missing present)
    (dolist (exe cfg/config-required-executables)
      (if (executable-find exe)
          (push exe present)
        (push exe missing)))
    `((present . ,(nreverse present))
      (missing . ,(nreverse missing)))))

(defun cfg/config-bootstrap-org-files ()
  "Create Org directory and default files when missing."
  (let* ((dir (cfg/config--org-directory))
         (created nil)
         (errors nil))
    (condition-case err
        (unless (file-directory-p dir)
          (make-directory dir t)
          (push dir created))
      (error
       (push (format "mkdir %s: %s" dir (error-message-string err)) errors)))
    (dolist (name cfg/config-org-bootstrap-files)
      (let ((path (expand-file-name name dir)))
        (condition-case err
            (unless (file-exists-p path)
              (with-temp-file path
                (insert (format "#+title: %s\n\n"
                                (file-name-base name))))
              (push path created))
          (error
           (push (format "touch %s: %s" path (error-message-string err))
                 errors)))))
    `((directory . ,dir)
      (created . ,(nreverse created))
      (errors . ,(nreverse errors)))))

(defun cfg/config-check-module-order ()
  "Check that init loader explicitly defers keybindings to last."
  (let* ((init-path (expand-file-name "init.el" user-emacs-directory))
         (ok nil)
         (msg nil))
    (if (not (file-exists-p init-path))
        (setq ok nil
              msg "init.el missing")
      (with-temp-buffer
        (insert-file-contents init-path)
        (setq ok (and (re-search-forward "08-keybindings\\.el" nil t)
                      (re-search-forward "(unless (string= file keybindings)" nil t)
                      (re-search-forward "(load keybindings)" nil t))))
      (setq msg (if ok
                    "keybindings loaded last by init loader"
                  "init loader does not guarantee keybindings-last")))
    `((ok . ,ok)
      (message . ,msg))))

(defconst cfg/config-critical-keys
  '("C-x b" "C-x g" "C-x C-d"
    "C-c /" "C-c p f" "C-c p r"
    "C-c g t" "C-c g y"
    "C-c e r" "C-c e a" "C-c e f"
    "C-c D" "C-c N" "C-c r"
    "C-c y r" "C-c y c" "C-c y a"
    "<f5>" "<f10>" "<f11>" "<f12>")
  "Keybindings that should always resolve to commands.")

(defun cfg/config-check-keybindings ()
  "Ensure critical keys resolve to interactive commands."
  (let (bad)
    (dolist (k cfg/config-critical-keys)
      (let ((cmd (key-binding (kbd k))))
        (unless (and cmd (commandp cmd))
          (push (format "%s -> %s" k (or cmd "nil")) bad))))
    `((ok . ,(null bad))
      (bad . ,(nreverse bad)))))

(defun cfg/config-health-snapshot ()
  "Collect health status."
  (let* ((bootstrap (cfg/config-bootstrap-org-files))
         (executables (cfg/config-check-executables))
         (module-order (cfg/config-check-module-order))
         (keybindings (cfg/config-check-keybindings)))
    `((timestamp . ,(current-time-string))
      (executables . ,executables)
      (bootstrap . ,bootstrap)
      (module-order . ,module-order)
      (keybindings . ,keybindings))))

(defun cfg/config--format-lines (xs)
  "Format list XS as newline-delimited lines."
  (mapconcat (lambda (x) (format "- %s" x)) xs "\n"))

(defun cfg/config-health-report (health)
  "Render HEALTH alist into report string."
  (let* ((exec (alist-get 'executables health))
         (bootstrap (alist-get 'bootstrap health))
         (order (alist-get 'module-order health))
         (keys (alist-get 'keybindings health))
         (missing (alist-get 'missing exec))
         (created (alist-get 'created bootstrap))
         (bootstrap-errors (alist-get 'errors bootstrap))
         (key-errors (alist-get 'bad keys)))
    (concat
     (format "Config health @ %s\n\n" (alist-get 'timestamp health))
     (format "Module order: %s\n"
             (if (alist-get 'ok order) "ok" "fail"))
     (format "Keybindings: %s\n"
             (if (alist-get 'ok keys) "ok" "fail"))
     (format "Missing executables: %d\n\n" (length missing))
     (when missing
       (concat "Missing executables:\n"
               (cfg/config--format-lines missing)
               "\n\n"))
     (when created
       (concat "Bootstrapped Org paths:\n"
               (cfg/config--format-lines created)
               "\n\n"))
     (when bootstrap-errors
       (concat "Org bootstrap errors:\n"
               (cfg/config--format-lines bootstrap-errors)
               "\n\n"))
     (when key-errors
       (concat "Keybinding resolution errors:\n"
               (cfg/config--format-lines key-errors)
               "\n")))))

(defun cfg/config-health-check ()
  "Run full config health checks and show report."
  (interactive)
  (setq cfg/config-last-health (cfg/config-health-snapshot))
  (let ((report (cfg/config-health-report cfg/config-last-health)))
    (if noninteractive
        (princ report)
      (with-current-buffer (get-buffer-create "*aileks-config-health*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report)
          (goto-char (point-min))
          (special-mode))
        (pop-to-buffer (current-buffer))))
    cfg/config-last-health))

(defun cfg/config-startup-health ()
  "Run startup health checks with graceful warnings."
  (setq cfg/config-last-health (cfg/config-health-snapshot))
  (let* ((exec (alist-get 'executables cfg/config-last-health))
         (bootstrap (alist-get 'bootstrap cfg/config-last-health))
         (keys (alist-get 'keybindings cfg/config-last-health))
         (missing (alist-get 'missing exec))
         (bootstrap-errors (alist-get 'errors bootstrap))
         (key-errors (alist-get 'bad keys))
         (warn-lines nil))
    (when missing
      (push (format "missing executables: %s"
                    (mapconcat #'identity missing ", "))
            warn-lines))
    (when bootstrap-errors
      (push "org bootstrap had errors (see *aileks-config-health*)" warn-lines))
    (when key-errors
      (push "some critical keybindings unresolved (see *aileks-config-health*)"
            warn-lines))
    (when warn-lines
      (display-warning
       'aileks-config
       (concat
        (mapconcat #'identity (nreverse warn-lines) "\n")
        "\nRun M-x cfg/config-health-check for full report.")
       :warning))))

(add-hook 'emacs-startup-hook #'cfg/config-startup-health)
