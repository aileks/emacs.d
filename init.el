;;; -*- lexical-binding: t -*-

;; Package setup (elpaca + use-package)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq use-package-always-ensure t)

;; Load my custom theme
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(condition-case err
    (load-theme 'ashen t)
  (error
   (display-warning 'init
                    (format "Failed to load theme `ashen': %s"
                            (error-message-string err))
                    :warning)))

(add-hook 'elpaca-after-init-hook #'elpaca-update-all)

;; Load modules
(let ((module-dir (expand-file-name "modules" user-emacs-directory)))
  (let* ((modules (sort (directory-files module-dir t "^[0-9].*\\.el$") #'string<))
         (keybindings (expand-file-name "08-keybindings.el" module-dir)))
    (dolist (file modules)
      (unless (string= file keybindings)
        (load file)))
    (when (file-exists-p keybindings)
      (load keybindings))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dc30d260b8880bee7ec11c9d2b69ac8fb1f7f5e63dddf262f399e87b31349823"
     "5542e5a1c0fbc0f23631b47366c6de6f6baa9ca45abf09c5f8149223f0154b2c"
     "3530e61adf707798249f7cf7858968e515214e20b7a360094f01ab3239ea7d9a"
     "67ff5f14c4e803825ac0f59732df882e56bf7610efd5328c2df359d5b5942c0b"
     "33f7727677d9891a1f9b1520de22fd0f5f25d918cd5a056f226b8fb5ace0da3d" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
