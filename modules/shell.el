;; Vterm

    (use-package vterm
      :ensure t
      :config
    (setq shell-file-name "/bin/bash"
          vterm-max-scrollback 5000))


    ;; Vterm-Toggle

    ;; vterm-toggle toggles between the vterm buffer and whatever buffer you are editing.
    (use-package vterm-toggle
      :after vterm
      :config
      (setq vterm-toggle-fullscreen-p nil)
      (setq vterm-toggle-scope 'project)
      (add-to-list 'display-buffer-alist
                   '((lambda (buffer-or-name _)
                         (let ((buffer (get-buffer buffer-or-name)))
                           (with-current-buffer buffer
                             (or (equal major-mode 'vterm-mode)
                                 (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                      (display-buffer-reuse-window display-buffer-at-bottom)
                      ;;(display-buffer-reuse-window display-buffer-in-direction)
                      ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                      ;;(direction . bottom)
                      ;;(dedicated . t) ;dedicated is supported in emacs27
                      (reusable-frames . visible)
                      (window-height . 0.3)))
      :bind
      ("C-c w t v" . vterm-toggle))

    ;; Sudo Edit
  ;;sudo-edit gives us the ability to open files with sudo privileges or switch over to editing with sudo privileges if we initially opened the file without such privileges.
  ;; Install and configure sudo-edit package
  (use-package sudo-edit
    :ensure t
    :bind
    (("C-c f u" . sudo-edit-find-file)
     ("C-c f U" . sudo-edit)))

  ;; Add which-key support for these bindings
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c f u" "Sudo find file"
      "C-c f U" "Sudo edit file"))

  ;; Add god-mode support if needed
  (with-eval-after-load 'god-mode
    (which-key-add-key-based-replacements
      "c f u" "Sudo find file"
      "c f U" "Sudo edit file"))

  ;;; ESHELL
(use-package eshell
  :ensure nil
  :defer t
  :config
  (defun emacs-solo/eshell-pick-history ()
    "Show Eshell history in a completing-read picker and insert the selected command."
    (interactive)
    (let* ((history-file (expand-file-name "eshell/history" user-emacs-directory))
           (history-entries (when (file-exists-p history-file)
                              (with-temp-buffer
                                (insert-file-contents history-file)
                                (split-string (buffer-string) "\n" t))))
           (selection (completing-read "Eshell History: " history-entries)))
      (when selection
        (insert selection))))


  (defun eshell/cat-with-syntax-highlighting (filename)
    "Like cat(1) but with syntax highlighting.
  Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))
  (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)


  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l") #'emacs-solo/eshell-pick-history)
              (local-set-key (kbd "C-l")
                             (lambda ()
                               (interactive)
                               (eshell/clear 1)
                               (eshell-send-input)))))

  (require 'vc)
  (require 'vc-git)
  (setopt eshell-prompt-function
        (lambda ()
          (concat
           "┌─("
           (if (> eshell-last-command-status 0)
               "❌"
             "🐂")
           " " (number-to-string eshell-last-command-status)
           ")──("
           "🧘 " (or (file-remote-p default-directory 'user) (user-login-name))
           ")──("
           "💻 " (or (file-remote-p default-directory 'host) (system-name))
           ")──("
           "🕝 " (format-time-string "%H:%M:%S" (current-time))
           ")──("
           "📁 "
           (concat (if (>= (length (eshell/pwd)) 40)
                       (concat "..." (car (last (butlast (split-string (eshell/pwd) "/") 0))))
                     (abbreviate-file-name (eshell/pwd))))
           ")\n"

           (when (and (fboundp 'vc-git-root) (vc-git-root default-directory))
             (concat
              "├─(🌿 " (car (vc-git-branches))
              (let* ((branch (car (vc-git-branches)))
                     (behind (string-to-number
                              (shell-command-to-string
                               (concat "git rev-list --count HEAD..origin/" branch)))))
                (if (> behind 0)
                    (concat "  ⬇️ " (number-to-string behind))))

              (let ((modified (length (split-string
                                       (shell-command-to-string
                                        "git ls-files --modified") "\n" t)))
                    (untracked (length (split-string
                                        (shell-command-to-string
                                         "git ls-files --others --exclude-standard") "\n" t))))
                (concat
                 (if (> modified 0)
                     (concat "  ✏️ " (number-to-string modified)))
                 (if (> untracked 0)
                     (concat "  📄 " ))))
              ")\n"))
           "└─➜ ")))

  (setq eshell-prompt-regexp "└─➜ ")

  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

  (setq eshell-visual-commands
        '("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
          "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m"
          "ncmpcpp" "newsbeuter" "nethack" "mutt")))



    (provide 'shell)
