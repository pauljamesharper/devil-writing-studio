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
                       (recents . 5)
                       (bookmarks . 5)))
    (dashboard-item-generators '((dhammapada . pjh/dashboard-insert-dhammapada)
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
      (interactive)
      (dashboard-insert-heading "Monthly Balance:"
                                nil
                                (nerd-icons-faicon "nf-fa-money"
                                                   :height 1.2
                                                   :v-adjust 0.0
                                                   :face 'dashboard-heading))
      (insert "\n")
      (let* ((categories '("Expenses:Food:Restaurants"
                           "Expenses:Food:Groceries"
                           "Expenses:Misc"))
             (current-month (format-time-string "%Y/%m"))
             (journal-file (expand-file-name "~/Dropbox/projects/personal/finances/main.dat"))
             (cmd (format "ledger bal --flat --monthly --period %s %s -f %s"
                          current-month
                          (mapconcat #'identity categories " ")
                          journal-file)))
        (insert (shell-command-to-string cmd))))
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

  ;; Easy insertion of weblinks

  (use-package org-web-tools
    :bind
    (("C-c w w" . org-web-tools-insert-link-for-url)))


                 ;;; EMMS - Emacs Multimedia System
  (use-package emms
    :ensure t
    :init
    (require 'emms-setup)
    (emms-all)
    (require 'emms-player-mplayer)
    :config
    ;; Use mkv/mplayer as the default player
    (setq emms-player-list '(emms-player-mplayer)
          emms-player-mplayer-command-name "mkv" ;; change to "mplayer" if mkv fails
          emms-source-file-default-directory "~/Music/")

    ;; Optional: recursively add all audio/video in ~/Music
    (setq emms-source-file-directory-tree-function
          'emms-source-file-directory-tree-find)

    ;; Create a keymap for EMMS commands
    (define-prefix-command 'my-emms-map)
    (global-set-key (kbd "C-c m") 'my-emms-map)

    (define-key my-emms-map (kbd "f") 'emms-play-find)
    (define-key my-emms-map (kbd "d") 'emms-play-directory-tree)
    (define-key my-emms-map (kbd "s") 'emms-stop)
    (define-key my-emms-map (kbd "p") 'emms-pause)
    (define-key my-emms-map (kbd "n") 'emms-next)
    (define-key my-emms-map (kbd "b") 'emms-previous)
    (define-key my-emms-map (kbd "+") (lambda () (interactive) (emms-seek +10))) ;; seek forward
    (define-key my-emms-map (kbd "-") (lambda () (interactive) (emms-seek -10))) ;; seek backward

    ;; Playback speed control
    (define-key my-emms-map (kbd "<") (lambda () (interactive)
                                        (emms-player-mplayer-command "speed_mult 0.9")))
    (define-key my-emms-map (kbd ">") (lambda () (interactive)
                                        (emms-player-mplayer-command "speed_mult 1.1")))

    ;; Add which-key labels
    (with-eval-after-load 'which-key
      (which-key-add-key-based-replacements
        "C-c m" "Multimedia"
        "C-c m f" "Find file"
        "C-c m d" "Play dir"
        "C-c m s" "Stop"
        "C-c m p" "Pause"
        "C-c m n" "Next"
        "C-c m b" "Back"
        "C-c m +" "Seek +10s"
        "C-c m -" "Seek -10s"
        "C-c m <" "Slower"
        "C-c m >" "Faster")))


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
  ;; Fixed Devil Mode setup
  ;; Define the face first with proper inheritance
  (defface devil-repeat-highlighting
    '((t (:inherit highlight)))
    "Face for repeatable keys in devil-mode."
    :group 'devil)


  (run-with-idle-timer 1 nil (lambda ()
                               (when (fboundp 'global-devil-mode)
                                 (global-devil-mode -1)
                                 (global-devil-mode 1))))

  ;; Now load devil mode
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
    ;; Correct the advice function issue
    (advice-add 'devil--which-key-describe-keymap :around
                (lambda (orig-fun &rest args)
                  (if (= (length args) 2)
                      (apply orig-fun args)
                    (message "Wrong number of arguments for which-key function"))))
    ;; Use a timer to ensure everything is loaded
    (run-with-idle-timer 2 nil (lambda ()
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
        ",x w" "windows")))

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


  (provide 'emacs-writing-studio)
                   ;;; emacs-writing-studio.el ends here
