;;; emacs-writing-studio.el --- Main config for the Devil's Emacs Writing Studio -*- lexical-binding: t; -*-

;; Original Author: Peter Prevos <peter@prevos.net>
;; URL: https://github.com/pprevos/emacs-writing-studio

                                     ;;; Commentary:
;; This file contains the core configuration for the Devil's Emacs Writing Studio.

                         ;;; Code:

                         ;;; emacs-writing-studio.el --- Devil's Emacs Writing Studio init -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Peter Prevos

;; Original Author: Peter Prevos <peter@prevos.net>
;; Original URL: https://github.com/pprevos/emacs-writing-studio/

;; Maintainer: Paul James Harper <pauljamesharper@hardhatcriticalrisk.com>
;; Forked and adapted by Paul James Harper, 2025.

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;; Emacs Writing Studio init file: https://lucidmanager.org/tags/emacs
;; This init file is tangled from: documents/ews-book/99-appendix.org

;; This is a fork of the Emacs Writing Studio configuration by Peter Prevos,
;; adapted and maintained by Paul James Harper for personal use and development.

;;
                                     ;;; Code:

;; Emacs 29 or higher?

(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires version 29 or later"))

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name
                           "custom.el"
                           user-emacs-directory))

(load custom-file :no-error-if-file-is-missing)

;; Bind key for customising variables

(keymap-global-set "C-c w v" 'customize-variable)

;; Create a keymap for find-related commands
(define-prefix-command 'my-find-map)

;; Bind it to a prefix key
(global-set-key (kbd "C-c f") my-find-map)

;; Define just the configuration file binding
(define-key my-find-map (kbd "c") 
            (lambda () (interactive) (find-file "~/.config/emacs/Emacs.org")))

;; Add which-key support for these bindings
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c f" "Find"
    "C-c f c" "Edit emacs config"))


;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  (warning-minimum-level :emergency))

;; Load EWS functions

(load-file (concat (file-name-as-directory user-emacs-directory)
                   "ews.el"))

;; Check for missing external software

(ews-missing-executables
 '(("gs" "mutool")
   "pdftotext"
   "soffice"
   "zip"
   "ddjvu"
   "curl"
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc") 
   ("grep" "ripgrep")
   ("convert" "gm")
   "dvipng"
   "latex"
   "hunspell"
   "git"))

                                 ;;; Dashboard

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-set-heading-icons t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '((dhammapada)
                     (ledger . 5)
                     (recents . 5)
                     (bookmarks . 5)))
  (dashboard-item-generators '((dhammapada . pjh/dashboard-insert-dhammapada)
                               (ledger . pjh/dashboard-ledger-monthly-balances)
                               (recents . dashboard-insert-recents)
                               (bookmarks . dashboard-insert-bookmarks)))
  :init
  (defun pjh/dashboard-insert-dhammapada (_list-size)
    (dashboard-insert-heading "Dhammapada Verse:"
                              nil
                              (nerd-icons-faicon "nf-fa-leaf"
                                                 :height 1.2
                                                 :v-adjust 0.0
                                                 :face 'dashboard-heading))
    (insert "\n")
    (let ((verse (shell-command-to-string "display-dhammapada")))
      (insert "    " (replace-regexp-in-string "\n" "\n    " verse))))

  (defun pjh/dashboard-ledger-monthly-balances (_list-size)
    (dashboard-insert-heading "Monthly Balance:"
                              nil
                              (nerd-icons-faicon "nf-fa-money"
                                                 :height 1.2
                                                 :v-adjust 0.0
                                                 :face 'dashboard-heading))
    (insert "\n")
    (let* ((categories '("Expenses:Food:Restaurants"
                         "Expenses:Food:Groceries"
                         "Expenses:Misc"
                         "Expenses:Subscriptions"))
           (current-month (format-time-string "%Y/%m"))
           (journal-file (expand-file-name "~/Dropbox/projects/personal/finances/main.dat"))
           (cmd (format "ledger bal --flat --monthly --period %s %s -f %s"
                        current-month
                        (mapconcat #'identity categories " ")
                        journal-file))
           (output (shell-command-to-string cmd)))
      (insert "    " (replace-regexp-in-string "\n" "\n    " output))))
  :config
  (dashboard-setup-startup-hook))






                                     ;;; LOOK AND FEEL

(tool-bar-mode -1)                  
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Short answers only please

(setq-default use-short-answers t)

;; Scratch buffer settings

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+title: Scratch Buffer\n#+subtitle: Scratch Buffer\nThe text in this buffer is not saved when exiting Emacs.\n\n")

;; Spacious padding (I don't like it but maybe you do?

;; (use-package spacious-padding
;;   :custom
;;   (line-spacing 3)
;;   (spacious-padding-mode 1))

;; Nerd Icons
;; This is an icon set that can be used with dashboard, dired, ibuffer and other Emacs programs.
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))


;; Modus and EF Themes

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(modus-operandi-tinted
                            modus-vivendi-tinted))
  :init
  ;; Load the dark theme (modus-vivendi-tinted) by default
  (load-theme 'modus-vivendi-tinted t)
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t m" . modus-themes-select)
   ("C-c w t s" . consult-theme)))

(use-package ef-themes)

;; Mixed-pich mode

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; Window management
;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; MINIBUFFER COMPLETION

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

(use-package savehist
  :init
  (savehist-mode))

;; Search for partial matches in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability
(use-package which-key
  :config
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-max-description-length 40
        which-key-min-display-lines 3)

  (which-key-mode)
  ;; Add descriptive labels for writing prefixes
  (which-key-add-key-based-replacements
    "C-c w" "writing"
    ",w" "writing"
    "C-c w t" "toggle"
    ",w t" "toggle"
    "C-c w s" "spell"
    ",w s" "spell"
    "C-c w b" "bibliography"
    ",w b" "bibliography"
    "C-c w m" "multimedia"
    ",w m" "multimedia"
    "C-c w d" "denote"
    ",w d" "denote"
    "C-c w x" "explore"
    ",w x" "explore")
  
  
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

(use-package which-key-posframe
  :after which-key
  :ensure t
  :config
  (setq which-key-posframe-border-width 2)
  (set-face-attribute 'which-key-posframe-border nil :background "lime green")
  (which-key-posframe-mode 1))

;; Contextual menu with right mouse button

(when (display-graphic-p)
  (context-menu-mode))

;; Improved help buffers

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

                                     ;;; Text mode settings

(use-package text-mode
  :ensure
  nil
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

;; Check spelling with flyspell and hunspell

(use-package flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary ews-hunspell-dictionaries)
  (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

                                     ;;; Ricing Org mode

(use-package org
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'error)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-id-link-to-org-use-id t)
  (org-fold-catch-invisible-edits 'show))

;; Show hidden emphasis markers

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Org modern: Most features are disabled for beginning users

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-table nil)
  (org-modern-keyword nil)
  (org-modern-timestamp nil)
  (org-modern-priority nil)
  (org-modern-checkbox nil)
  (org-modern-tag nil)
  (org-modern-block-name nil)
  (org-modern-keyword nil)
  (org-modern-footnote nil)
  (org-modern-internal-target nil)
  (org-modern-radio-target nil)
  (org-modern-statistics nil)
  (org-modern-progress nil))

;; INSPIRATION

;; Doc-View

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Managing Bibliographies

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Citar to access bibliographies

(use-package citar
  :defer t
  :custom
  (citar-bibliography ews-bibtex-files)
  :bind
  (("C-c w b o" . citar-open)))


;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))



        ;;; Bongo - Flexible Media Player
(use-package bongo
  :ensure t
  :config
  ;; Set default directory for music files
  (setq bongo-default-directory "~/Music/")
  
  ;; Configure external players (mpv is recommended if available)
  (when (executable-find "mpv")
    (setq bongo-enabled-backends '(mpv))
    ;; Don't use socket connection to avoid "Connection refused" errors
    (setq bongo-mpv-use-socket-p nil)
    
    ;; Configure MPV to handle audio and video differently
    (defun my/bongo-mpv-args-for-file (file)
      "Return appropriate MPV arguments based on file type."
      (let ((extension (downcase (file-name-extension file ""))))
        (cond
         ;; Video extensions - use fullscreen
         ((member extension '("mp4" "mkv" "avi" "mov" "webm" "flv" "wmv"))
          '("--fs" "--video=auto"))
         ;; Audio extensions - hide video
         (t
          '("--no-video" "--no-terminal")))))
    
    ;; Override the default arguments function
    (defun bongo-mpv-args-for-file (file &optional backend-name)
      (my/bongo-mpv-args-for-file file)))
  
  ;; Adjust volume changes
  (setq bongo-volume-increment 5)
  
  ;; Optionally customize appearance
  (setq bongo-header-line-mode nil)  ; disable header line
  (setq bongo-mode-line-indicator-mode t)  ; enable mode line indicator
  
  ;; Create a keymap for Bongo commands
  (define-prefix-command 'my-bongo-map)
  (global-set-key (kbd "C-c m") 'my-bongo-map)
  
  ;; Basic playback controls
  (define-key my-bongo-map (kbd "b") 'bongo)  ; main buffer
  (define-key my-bongo-map (kbd "l") 'bongo-library)  ; library view
  (define-key my-bongo-map (kbd "p") 'bongo-pause/resume)  ; toggle pause
  (define-key my-bongo-map (kbd "s") 'bongo-stop)  ; stop playback
  (define-key my-bongo-map (kbd "n") 'bongo-next)  ; next track
  (define-key my-bongo-map (kbd "r") 'bongo-previous)  ; previous track
  (define-key my-bongo-map (kbd "f") 'bongo-seek-forward)  ; seek forward
  (define-key my-bongo-map (kbd "d") 'bongo-seek-backward)  ; seek backward
  
  ;; Volume controls
  (define-key my-bongo-map (kbd "+") 'bongo-volume-up)  ; increase volume
  (define-key my-bongo-map (kbd "-") 'bongo-volume-down)  ; decrease volume
  
  ;; Playlist management
  (define-key my-bongo-map (kbd "i") 'bongo-insert-file)  ; insert file
  (define-key my-bongo-map (kbd "I") 'bongo-insert-directory)  ; insert directory
  (define-key my-bongo-map (kbd "c") 'bongo-clear-playlist)  ; clear playlist
  (define-key my-bongo-map (kbd "a") 'bongo-append-file)  ; append file
  (define-key my-bongo-map (kbd "A") 'bongo-append-directory)  ; append directory
  
  ;; Playback speed control
  (define-key my-bongo-map (kbd "<") 
              (lambda () (interactive) 
        	(bongo-custom-backend-action 'mpv "speed-set" "0.9")))
  (define-key my-bongo-map (kbd ">") 
              (lambda () (interactive) 
        	(bongo-custom-backend-action 'mpv "speed-set" "1.1")))
  
  ;; Add which-key labels
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c m" "Multimedia"
      ",m" "Multimedia"
      "C-c m b" "Bongo buffer"
      ",m b" "Bongo buffer"
      "C-c m l" "Library view"
      ",m l" "Library view"
      "C-c m p" "Play/pause"
      ",m p" "Play/pause"
      "C-c m s" "Stop"
      ",m s" "Stop"
      "C-c m n" "Next track"
      ",m n" "Next track"
      "C-c m r" "Previous track"
      ",m r" "Previous track"
      "C-c m f" "Seek forward"
      ",m f" "Seek forward"
      "C-c m d" "Seek backward"
      ",m d" "Seek backward"
      "C-c m +" "Volume up"
      ",m +" "Volume up"
      "C-c m -" "Volume down"
      ",m -" "Volume down"
      "C-c m i" "Insert file"
      ",m i" "Insert file"
      "C-c m I" "Insert directory"
      ",m I" "Insert directory"
      "C-c m a" "Append file"
      ",m a" "Append file"
      "C-c m A" "Append directory"
      ",m A" "Append directory"
      "C-c m c" "Clear playlist"
      ",m c" "Clear playlist"
      "C-c m <" "Slower"
      ",m <" "Slower"
      "C-c m >" "Faster"
      ",m >" "Faster")))

(use-package openwith
  :config
  (openwith-mode t)
  :custom
  (openwith-associations nil))

;; Fleeting notes

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-goto-interface 'outline-path-completion)
  (org-capture-templates
   '(("f" "Fleeting note"
      item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Permanent note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?"))))

;; Denote

(use-package denote
  :defer t
  :custom
  (denote-sort-keywords t)
  (denote-link-description-function #'ews-denote-link-description-title-case)
  (denote-directory "~/Dropbox/Documents/notes")
  (denote-rename-buffer-format "Denote: %t (%k)")
  (denote-infer-keywords nil)
  (denote-known-keywords
   '("pra" "prb" "prc"
     "areas" "resource" "archive"))
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :init
  (require 'denote-org-extras)
  :bind
  (("C-c w d b" . denote-find-backlink)
   ("C-c w d d" . denote-date)
   ("C-c w d l" . denote-find-link)
   ("C-c w d h" . denote-org-extras-link-to-heading)
   ("C-c w d i" . denote-link-or-create)
   ("C-c w d k" . denote-rename-file-keywords)
   ("C-c w d n" . denote)
   ("C-c w d r" . denote-rename-file)
   ("C-c w d R" . denote-rename-file-using-front-matter)))

;; Consult convenience functions

(use-package consult
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-c w g" . consult-grep))
  :config
  (add-to-list 'consult-preview-allowed-hooks 'visual-line-mode))

;; Consult-Notes for easy access to notes

(use-package consult-notes
  :custom
  (consult-notes-denote-display-keywords-indicator "_")
  :bind
  (("C-c w d f" . consult-notes)
   ("C-c w d g" . consult-notes-search-in-all-notes))
  :init
  (consult-notes-denote-mode))

;; Citar-Denote to manage literature notes

(use-package citar-denote
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))

;; Explore and manage your Denote collection

(use-package denote-explore
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-barchart-keywords)
   ("C-c w x e" . denote-explore-barchart-filetypes)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ("C-c w x x" . denote-explore-random-regex)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-barchart-degree)))

;; Set some Org mode shortcuts

(use-package org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

;; Distraction-free writing

(use-package olivetti
  :demand t
  :bind
  (("C-c w o" . ews-olivetti)))

;; Undo Tree

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  :bind
  (("C-c w u" . undo-tree-visualise)))

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-global-bibliography ews-bibtex-files
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

;; Lookup words in the online dictionary

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  (("C-c w s d" . dictionary-lookup-definition)))

(use-package powerthesaurus
  :bind
  (("C-c w s p" . powerthesaurus-transient)))

;; Writegood-Mode for weasel words, passive writing and repeated word detection

(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  :hook
  (text-mode . writegood-mode))

;; Titlecasing

(use-package titlecase
  :bind
  (("C-c w s t" . titlecase-dwim)
   ("C-c w s c" . ews-org-headings-titlecase)))

;; Abbreviations

(add-hook 'text-mode-hook 'abbrev-mode)

;; Lorem Ipsum generator

(use-package lorem-ipsum
  :custom
  (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
  :init
  (setq lorem-ipsum-sentence-separator
        (if sentence-end-double-space "  " " "))
  :bind
  (("C-c w s i" . lorem-ipsum-insert-paragraphs)))

;; ediff

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;; Enable Other text modes

;; Fontain mode for writing scrits

(use-package fountain-mode)

;; Markdown mode

(use-package markdown-mode)

;; PUBLICATION

;; Generic Org Export Settings

(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

;; epub export

(use-package ox-epub
  :demand t
  :init
  (require 'ox-org))

;; LaTeX PDF Export settings

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; EWS paperback configuration

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("ews"
     "\\documentclass[11pt, twoside, hidelinks]{memoir}
                                           \\setstocksize{9.25in}{7.5in}
                                           \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
                                           \\setlrmarginsandblock{1.5in}{1in}{*} 
                                           \\setulmarginsandblock{1in}{1.5in}{*}
                                           \\checkandfixthelayout
                                           \\layout
                                           \\setcounter{tocdepth}{0}
                                           \\setsecnumdepth{subsection}
                                           \\renewcommand{\\baselinestretch}{1.2}
                                           \\setheadfoot{0.5in}{0.75in}
                                           \\setlength{\\footskip}{0.8in}
                                           \\chapterstyle{bianchi}
                                           \\renewcommand{\\beforechapskip}{-30pt}
                                           \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
                                           \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
                                           \\setsubsubsecheadstyle{\\normalfont\\centering}
                                           \\pagestyle{myheadings}
                                           \\usepackage[font={small, it}]{caption}
                                           \\usepackage{ccicons}
                                           \\usepackage{ebgaramond}
                                           \\usepackage[authoryear]{natbib}
                                           \\bibliographystyle{apalike}
                                           \\usepackage{svg}
                                           \\hyphenation{mini-buffer}
                                           \\renewcommand{\\LaTeX}{LaTeX}
                                           \\renewcommand{\\TeX}{TeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

                                     ;;; ADMINISTRATION

;; Bind org agenda command and custom agenda

(use-package org
  :custom
  (org-agenda-custom-commands
   '(("e" "Agenda, next actions and waiting"
      ((agenda "" ((org-agenda-overriding-header "Next seven days:")
                   (org-agenda-span 7)
                   (org-agenda-start-on-weekday nil)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Actions:")))
       (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))))))
  :bind
  (("C-c a" . org-agenda)))

;; Khalel

(use-package khalel
  :ensure t
  :after org
  :config
  (khalel-add-capture-template)
  (require 'khalel-icalendar))
(setq khalel-khal-command "~/.local/bin/khal")
(setq khalel-vdirsyncer-command "~/.local/bin/vdirsyncer")
(setq khalel-capture-key "e")
(setq org-directory "~/Dropbox/Documents/notes")
(setq khalel-import-org-file (concat org-directory "/" "calendar.org"))
(setq khalel-import-org-file-confirm-overwrite nil)
(setq khalel-import-end-date "+90d")


;; FILE MANAGEMENT

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil))

;; Hide or display hidden files

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ( "."     . dired-omit-mode))
  :custom (dired-omit-files "^\\.[a-zA-Z0-9]+"))

;; Backup files

(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              version-control t
              delete-old-versions t
              create-lockfiles nil)

;; Recent files

(use-package recentf
  :config
  (recentf-mode t)
  :custom
  (recentf-max-saved-items 50)
  :bind
  (("C-c w r" . recentf-open)))

;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r d" . bookmark-delete))

;; Image viewer

(use-package emacs
  :custom
  (image-dired-external-viewer "gimp")
  :bind
  ((:map image-mode-map
         ("k" . image-kill-buffer)
         ("<right>" . image-next-file)
         ("<left>"  . image-previous-file))
   (:map dired-mode-map
         ("C-<return>" . image-dired-dired-display-external))))

(use-package image-dired
  :bind
  (("C-c w I" . image-dired))
  (:map image-dired-thumbnail-mode-map
        ("C-<right>" . image-dired-display-next)
        ("C-<left>"  . image-dired-display-previous)))

;; ADVANCED UNDOCUMENTED EXPORT SETTINGS FOR EWS

;; Use GraphViz for flow diagrams
;; requires GraphViz software
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates GraophViz dot

                                    ;;; Devil Mode 

(use-package devil
  :ensure t
  :vc (:url "https://github.com/fbrosda/devil"
            :branch "dev"
            :rev :newest)
  :custom
  (devil-exit-key ".")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  (devil-which-key-support t)
  :config
  ;; New advice fix for the proper function
  (defun devil--enable-which-key-support ()
    "Enable which-key support for devil-mode."
    (when devil-which-key-support
      (require 'which-key)
      (which-key-add-key-based-replacements "," "Devil")))
  
  ;; Replace the problematic function
  (advice-remove 'devil--which-key-describe-keymap 'devil--which-key-describe-keymap)
  
  ;; Use a timer to ensure everything is loaded properly
  (run-with-idle-timer 5 nil (lambda ()
                               (message "Activating devil-mode...")
                               (global-devil-mode -1)
                               (global-devil-mode 1)))
  
  ;; Ensure which-key replacements are set up correctly
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c w" "writing"
      ",w" "writing"
      "C-c w t" "toggle"
      ",w t" "toggle"
      "C-c w s" "spell"
      ",w s" "spell"
      "C-c w b" "bibliography"
      ",w b" "bibliography"
      "C-c m" "multimedia"
      ",m" "multimedia"
      "C-c w d" "denote"
      ",w d" "denote"
      "C-c w x" "explore"
      ",w x" "explore"
      "C-x w" "windows"
      ",x w" "windows"
      "C-c n" "Notes"
      ",n" "Notes")))

;; For blocks
(setq org-structure-template-alist
      '(("s" . "src")
        ("e" . "src emacs-lisp")
        ("E" . "src emacs-lisp :results value code :lexical t")
        ("t" . "src emacs-lisp :tangle FILENAME")
        ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
        ("x" . "example")
        ("X" . "export")
        ("q" . "quote")))

                    ;;; ----- TODO Configuration -----

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)")))

(setq org-todo-keyword-faces
      '(("GOAL" . (:foreground "orange red" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("WAIT" . (:foreground "HotPink2" :weight bold))
        ("BACK" . (:foreground "MediumPurple3" :weight bold))))

                      ;;; ----- Context Tags -----

(setq-default org-tag-alist
              '((:startgroup)
                ("Areas")
                (:grouptags)
                ("@home" . ?H)
                ("@work" . ?W)
                (:endgroup)

                (:startgrouptag . nil)
                ("Contexts")
                (:grouptags)
                ("@computer" . ?C)
                ("@mobile" . ?M)
                ("@calls" . ?A)
                ("@errands" . ?E)
                (:endgrouptag)

                ;; Task Types
                (:startgrouptag . nil)
                ("Types")
                (:grouptags)
                ("@easy" . ?e)
                ("@hacking" . ?h)
                ("@writing" . ?w)
                ("@creative" . ?v)
                ("@accounting" . ?a)
                ("@email" . ?m)
                ("@system" . ?s)
                (:endgrouptag)

                ;; Workflow states
                (:startgroup . nil)
                ("States")
                (:grouptags)
                ("@plan" . ?p)
                ("@review" . ?r)
                ("@followup" . ?f)
                (:endgroup)))


;; Only make context tags inheritable (what about noexport?)
(setq org-use-tag-inheritance "^@")

                      ;;; ----- Time Tracking -----

;; Clock in on the current task when setting a timer
(add-hook 'org-timer-set-hook #'org-clock-in)

;; Clock out of the current task when the timer is complete
(add-hook 'org-timer-done-hook #'org-clock-out)

                      ;;; ----- Agenda Configuration -----

(defvar dw/base-agenda-files '("inbox.org" "calendar.org")
  "The base agenda files that will always be included.")

(setq org-agenda-span 'day
      org-agenda-start-with-log-mode t
      org-agenda-files dw/base-agenda-files
      org-agenda-window-setup 'current-window)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time
      org-log-into-drawer t)


                      ;;; ----- Denote Integration -----

(defun dw/refresh-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (append (denote-directory-files "_pra")
                dw/base-agenda-files)))

(defun dw/goto-weekly-note ()
  (interactive)
  (let* ((note-title (format-time-string "%Y-W%V"))
         (existing-notes
          (denote-directory-files (format "-%s" note-title) nil t)))
    (if existing-notes
        (find-file (car existing-notes))
      (denote note-title '("plw")))))

(with-eval-after-load 'denote
  ;; Quick access commands
  (keymap-set global-map "C-c n w" #'dw/goto-weekly-note)

  ;; Refresh agenda files the first time
  (dw/refresh-agenda-files)

  ;; Update agenda files after notes are created or renamed
  (add-hook 'denote-after-rename-file-hook #'dw/refresh-agenda-files)
  (add-hook 'denote-after-new-note-hook #'dw/refresh-agenda-files))

;; Add which-key label for the notes prefix
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c n" "Notes"
    "C-c n w" "Weekly note"))

;; Also add the devil-mode version
(with-eval-after-load 'devil
  (which-key-add-key-based-replacements
    ",n" "Notes"
    ",n w" "Weekly note"))

;; Ledger
(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode))
(setq ledger-clear-whole-transactions 1
      ledger-reconcile-default-commodity "€"
      ledger-default-date-format "%Y-%m-%d")
(setq ledger-file "~/Dropbox/projects/personal/finances/main.dat")

(use-package flycheck-ledger
  :ensure t
  :after ledger-mode
  :config
  (add-hook 'ledger-mode-hook #'flycheck-mode))


;; Ensure multimedia keybindings are properly set
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c m" "Multimedia"
    ",m" "Multimedia"))

(with-eval-after-load 'devil
  (which-key-add-key-based-replacements
    ",m" "Multimedia"))

;; Read RSS feeds with Elfeed

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory (getenv "HOME"))
                 "Dropbox/Documents/elfeed.org"))))

      ;;; Enhanced Elfeed YouTube Configuration with Fixed Multimedia Keybindings

;; This extension to your elfeed setup adds better YouTube feed management
;; and integrates with your existing Bongo configuration
;; WITH FIXED MULTIMEDIA KEYBINDINGS (C-c m and ,m in devil mode)

;; 1. Core Elfeed enhancements for YouTube
(with-eval-after-load 'elfeed
  ;; Add a specific tag for YouTube entries to make filtering easier
  (add-hook 'elfeed-new-entry-hook
            (lambda (entry)
              (when (string-match-p "\\(youtube\\.com\\|youtu\\.be\\)" (elfeed-entry-link entry))
                (elfeed-tag entry 'youtube))))

  ;; Face for YouTube entries - makes them stand out in the feed list
  (defface elfeed-youtube
    '((t :foreground "#FF0000"))
    "Face for YouTube entries in Elfeed"
    :group 'elfeed)

  ;; Add face to the entry list format
  (push '(youtube elfeed-youtube)
        elfeed-search-face-alist)

  ;; Enhanced keybindings for YouTube workflows - using different keys
  ;; to avoid conflicts with your multimedia setup
  (define-key elfeed-search-mode-map (kbd "v") 'pjh/elfeed-play-with-bongo)
  (define-key elfeed-search-mode-map (kbd "V") 'pjh/elfeed-add-to-bongo)
  
  ;; Filter management keys
  (define-key elfeed-search-mode-map (kbd "Y") 
              (lambda () (interactive) (elfeed-search-set-filter "+youtube +unread")))
  (define-key elfeed-search-mode-map (kbd "A") 
              (lambda () (interactive) (elfeed-search-set-filter "+unread")))
  
  ;; Default filter to show unread entries
  (setq elfeed-search-filter "+unread"))

;; 2. YouTube playback functions (previously defined)
(defun pjh/elfeed-play-with-bongo ()
  "Play the YouTube video of the current Elfeed entry with Bongo."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (if entry
        (let ((url (elfeed-entry-link entry)))
          (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
              (progn
                ;; Ensure Bongo buffer exists
                (unless (get-buffer "*Bongo Playlist*")
                  (bongo))
                (with-current-buffer (or (get-buffer "*Bongo Playlist*")
                                         (current-buffer))
                  ;; Add to Bongo playlist
                  (bongo-insert-uri url)
                  ;; Play if not already playing
                  (unless (bongo-playing-p)
                    (bongo-play-line)))
                ;; Mark as read in Elfeed
                (elfeed-search-untag-all-unread)
                (message "Added YouTube video to Bongo: %s" url))
            (message "The URL is not a YouTube link: %s" url)))
      (message "No entry selected in Elfeed."))))

(defun pjh/elfeed-add-to-bongo ()
  "Add the YouTube video of the current Elfeed entry to Bongo playlist without playing."
  (interactive)
  (let ((entry (elfeed-search-selected :single)))
    (if entry
        (let ((url (elfeed-entry-link entry)))
          (if (and url (string-match-p "https?://\\(www\\.\\)?youtube\\.com\\|youtu\\.be" url))
              (progn
                ;; Ensure Bongo buffer exists
                (unless (get-buffer "*Bongo Playlist*")
                  (bongo))
                (with-current-buffer (or (get-buffer "*Bongo Playlist*")
                                         (current-buffer))
                  ;; Add to Bongo playlist
                  (bongo-insert-uri url))
                ;; Mark as read in Elfeed
                (elfeed-search-untag-all-unread)
                (message "Added YouTube video to Bongo playlist: %s" url))
            (message "The URL is not a YouTube link: %s" url)))
      (message "No entry selected in Elfeed."))))

;; 3. Helper function to quickly add YouTube channel feeds
(defun pjh/elfeed-add-youtube-channel (channel-id)
  "Add a YouTube channel to elfeed-org config by channel ID or username.
      CHANNEL-ID can be either a channel ID starting with UC or a username."
  (interactive "sEnter YouTube channel ID or username: ")
  (let* ((is-id (string-prefix-p "UC" channel-id))
         (feed-url (if is-id
                       (format "https://www.youtube.com/feeds/videos.xml?channel_id=%s" channel-id)
                     (format "https://www.youtube.com/feeds/videos.xml?user=%s" channel-id)))
         (elfeed-file (concat (file-name-as-directory (getenv "HOME"))
                              "Dropbox/Documents/elfeed.org"))
         (channel-info (if is-id
                           (format "Channel ID: %s" channel-id)
                         (format "Username: %s" channel-id))))
    
    ;; Request channel information to get the title
    (url-retrieve feed-url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Error fetching channel info: %s" (plist-get status :error))
                      ;; Parse the response to get the channel title
                      (goto-char (point-min))
                      (re-search-forward "<title>\\(.*?\\)</title>" nil t)
                      (let ((channel-title (match-string 1)))
                        ;; Add to elfeed.org file
                        (with-current-buffer (find-file-noselect elfeed-file)
                          (goto-char (point-max))
                          ;; Find the YouTube heading or create it
                          (unless (re-search-backward "^\\*+ YouTube" nil t)
                            (goto-char (point-max))
                            (insert "\n* YouTube\n"))
                          (end-of-line)
                          (insert (format "\n** %s\n   :PROPERTIES:\n   :ID: %s\n   :END:\n   %s" 
                                          channel-title
                                          channel-id
                                          feed-url))
                          (save-buffer)
                          (kill-buffer))
                        (message "Added %s to elfeed.org" channel-title)
                        ;; Refresh elfeed-org
                        (when (featurep 'elfeed-org)
                          (elfeed-org-export-opml)
                          (elfeed-org))))))
    (message "Processing channel %s..." channel-id)))

;; 4. YouTube feed management transient menu
(use-package transient
  :ensure t
  :config
  (transient-define-prefix pjh/youtube-feeds-menu ()
    "Menu for YouTube feed management."
    ["YouTube Feed Actions"
     ("a" "Add YouTube channel" pjh/elfeed-add-youtube-channel)
     ("y" "Show only YouTube" (lambda () (interactive) 
                                (elfeed-search-set-filter "+youtube +unread")))
     ("f" "Filter YouTube..." (lambda () (interactive)
                                (elfeed-search-set-filter 
                                 (concat "+youtube +unread " 
                                         (read-string "Additional filter terms: ")))))
     ("r" "Reset filter" (lambda () (interactive) 
                           (elfeed-search-set-filter "+unread")))
     ("o" "Open elfeed-org file" (lambda () (interactive)
                                   (find-file (concat (file-name-as-directory (getenv "HOME"))
                                                      "Dropbox/Documents/elfeed.org"))))
     ("u" "Update feeds" elfeed-update)
     ]
    ["Bongo Integration"
     ("p" "Play in Bongo" pjh/elfeed-play-with-bongo)
     ("q" "Queue in Bongo" pjh/elfeed-add-to-bongo)
     ("b" "Open Bongo" bongo)
     ])
  
  ;; Add key binding for the YouTube menu - using different key
  (with-eval-after-load 'elfeed
    (define-key elfeed-search-mode-map (kbd "C-c y") 'pjh/youtube-feeds-menu)))

;; 5. Customize Bongo for better YouTube experience
(with-eval-after-load 'bongo
  ;; Ensure MPV uses yt-dlp with good default quality settings
  (when (executable-find "mpv")
    (setq bongo-mpv-program-arguments 
          (append bongo-mpv-program-arguments 
                  '("--ytdl-format=bestvideo[height<=720]+bestaudio/best[height<=720]"
                    "--force-window=yes"
                    "--keep-open=yes"))))
  
  ;; Add function to get video title when playing
  (defun pjh/bongo-get-youtube-title ()
    "Get the title of the currently playing YouTube video."
    (interactive)
    (when (bongo-playing-p)
      (let ((filename (bongo-playlist-line-file bongo-playing-line)))
        (when (and filename (string-match-p "\\(youtube\\.com\\|youtu\\.be\\)" filename))
          (message "Playing: %s" 
                   (shell-command-to-string 
                    (format "yt-dlp --get-title %s 2>/dev/null" 
                            (shell-quote-argument filename)))))))))

;; 6. Proper keybinding setup that doesn't conflict with multimedia keys
;; Preserve your C-c m (multimedia) map and add our YouTube bindings elsewhere
(with-eval-after-load 'which-key
  ;; Add our YouTube menu hint
  (which-key-add-key-based-replacements "C-c y" "YouTube menu")
  
  ;; Make sure we don't override the multimedia map
  (which-key-add-key-based-replacements "C-c m" "Multimedia")
  (which-key-add-key-based-replacements ",m" "Multimedia"))

;; 7. Proper devil-mode integration that preserves your multimedia keys
(with-eval-after-load 'devil
  ;; Add our YouTube menu hint
  (which-key-add-key-based-replacements ",y" "YouTube menu")
  
  ;; Make sure multimedia map is preserved
  (which-key-add-key-based-replacements ",m" "Multimedia"))

;; 8. Additional elfeed-org integration
(with-eval-after-load 'elfeed-org
  ;; Helper to convert YouTube URLs to feed URLs
  (defun pjh/youtube-url-to-feed-url (url)
    "Convert a standard YouTube URL to its RSS feed URL."
    (interactive "sYouTube URL: ")
    (cond
     ;; Channel URL format
     ((string-match "youtube\\.com/channel/\\(UC[^/?\n]+\\)" url)
      (let ((channel-id (match-string 1 url)))
        (message "Feed URL: https://www.youtube.com/feeds/videos.xml?channel_id=%s" channel-id)
        (kill-new (format "https://www.youtube.com/feeds/videos.xml?channel_id=%s" channel-id))))
     
     ;; User URL format
     ((string-match "youtube\\.com/\\(?:user\\|c\\)/\\([^/?\n]+\\)" url)
      (let ((username (match-string 1 url)))
        (message "Feed URL: https://www.youtube.com/feeds/videos.xml?user=%s" username)
        (kill-new (format "https://www.youtube.com/feeds/videos.xml?user=%s" username))))
     
     ;; Couldn't parse
     (t (message "Could not parse YouTube URL format")))))

;; 9. Global keybindings for YouTube functionality
;; Create a dedicated keymap that doesn't interfere with multimedia
(define-prefix-command 'my-youtube-map)
(global-set-key (kbd "C-c y") 'my-youtube-map)

;; Add YouTube commands to this map
(define-key my-youtube-map (kbd "a") 'pjh/elfeed-add-youtube-channel)
(define-key my-youtube-map (kbd "u") 'pjh/youtube-url-to-feed-url)
(define-key my-youtube-map (kbd "e") 'elfeed)
(define-key my-youtube-map (kbd "m") 'pjh/youtube-feeds-menu)

;; Add which-key descriptions for these commands
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c y a" "Add YouTube channel"
    "C-c y u" "Convert URL to feed"
    "C-c y e" "Open Elfeed"
    "C-c y m" "YouTube menu"))

;; Make sure devil-mode knows about these bindings too
(with-eval-after-load 'devil
  (which-key-add-key-based-replacements
    ",y a" "Add YouTube channel"
    ",y u" "Convert URL to feed"
    ",y e" "Open Elfeed"
    ",y m" "YouTube menu"))


(provide 'emacs-writing-studio)
                                     ;;; emacs-writing-studio.el ends here
