;; init.el --- Main Emacs configuration entry point

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load main configuration modules
(require 'emacs-writing-studio)
(require 'ews)
(require 'shell)



;; Startup echo
(message "Emacs initialized successfully.")

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))
