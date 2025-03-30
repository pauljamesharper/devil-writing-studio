;; init.el --- Main Emacs configuration entry point

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load main configuration modules
(require 'emacs-writing-studio)
(require 'ews)
(require 'shell)


;; Startup echo
(message "Emacs initialized successfully.")
