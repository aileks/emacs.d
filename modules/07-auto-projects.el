;;; -*- lexical-binding: t -*-

(require 'project)

(defun auto-register-project ()
  "Register current file's git repo as a project if it's not already registered."
  (when-let ((filename (buffer-file-name))
              (project-root (project-try-vc (file-name-directory filename))))
    (project-remember-project (make-project project-root))))

;; Register hook only after project.el AND dashboard are loaded
(with-eval-after-load 'project
  (with-eval-after-load 'dashboard
    (add-hook 'find-file-hook #'auto-register-project)))
