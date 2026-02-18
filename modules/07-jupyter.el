;;; -*- lexical-binding: t -*-

(defun cfg/pyvenv--project-root ()
  (or (when-let* ((proj (project-current nil)))
        (project-root proj))
      (locate-dominating-file default-directory "pyproject.toml")
      (locate-dominating-file default-directory "requirements.txt")
      (locate-dominating-file default-directory ".git")))

(defun cfg/pyvenv--env-dir-p (dir)
  (and (file-directory-p dir)
       (or (file-exists-p (expand-file-name "pyvenv.cfg" dir))
           (file-exists-p (expand-file-name "bin/python" dir))
           (file-exists-p (expand-file-name "Scripts/python.exe" dir)))))

(defun cfg/pyvenv--find-project-venv (root)
  (catch 'match
    (dolist (name '(".venv" "venv" ".env" "env"))
      (let ((candidate (expand-file-name name root)))
        (when (cfg/pyvenv--env-dir-p candidate)
          (throw 'match candidate))))
    nil))

(defun cfg/pyvenv-auto-activate ()
  "Activate local project venv when visiting Python buffers."
  (when (require 'pyvenv nil t)
    (when-let* ((root (cfg/pyvenv--project-root))
                (venv (cfg/pyvenv--find-project-venv root))
                (target (file-name-as-directory (expand-file-name venv))))
      (unless (equal target pyvenv-virtual-env)
        (pyvenv-activate target)))))

(add-hook 'python-base-mode-hook #'cfg/pyvenv-auto-activate)

(use-package pyvenv
  :defer t
  :init
  (setopt pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[" pyvenv-virtual-env-name "] ")))
  :config
  (pyvenv-mode 1))

(use-package jupyter
  :defer t
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-repl-associate-buffer))

(use-package code-cells
  :defer t
  :hook ((python-base-mode . code-cells-mode-maybe)
         (julia-mode . code-cells-mode-maybe)
         (julia-ts-mode . code-cells-mode-maybe)
         (ess-r-mode . code-cells-mode-maybe))
  :config
  ;; Keep .ipynb as text-first buffers with language-appropriate highlighting.
  (setq code-cells-convert-ipynb-style
        '(("jupytext" "--to" "ipynb")
          ("jupytext" "--to" "auto:percent")
          code-cells--guess-mode
          code-cells-mode))
  (with-eval-after-load 'code-cells
    (define-key code-cells-mode-map (kbd "C-c C-c") #'code-cells-eval)
    (define-key code-cells-mode-map (kbd "C-c C-n") #'code-cells-forward-cell)
    (define-key code-cells-mode-map (kbd "C-c C-p") #'code-cells-backward-cell)))

(defun cfg/enable-org-babel-jupyter ()
  (when (and (featurep 'org)
             (locate-library "ob-jupyter"))
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages '((jupyter . t))))))

(with-eval-after-load 'org
  (cfg/enable-org-babel-jupyter))

(with-eval-after-load 'jupyter
  (cfg/enable-org-babel-jupyter))
