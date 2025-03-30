;;; config-org-agenda-denote.el -- Org Agenda Denote   -*- lexical-binding:t -*-

;; Copyright (C) 2024  Alejandro Barocio A.

;; Author: Alejandro Barocio A. <alejandro@barocio.cc>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file was inspired by the following video and article of
;; David Wilson (a.k.a. daviwil, from System Crafters), which tried to
;; hack Protesilaos Stavrou's denote package.

;; https://systemcrafters.net/live-streams/june-7-2024/
;; https://www.youtube.com/live/q2l_EShllus?si=1nV8yA6IPOcA3tbf

;; This implements a keyword selection with a description (similar to
;; what we can see with marginalia), added the selection for a
;; category (which is automatically added to the file).

;; And, of course, the addition of (some of) the denote files to the
;; agenda, based on keywords (filetags) and the category.

;;; Code:

(defvar my/denote-keywords
  '(;; Project tags
    ("pra" . "Project Active")
    ("prb" . "Project left Behind")
    ("prc" . "Project Cancelled")
    ;;
    ("ply" . "Planning Yearly")
    ("plm" . "Planning Monthly")
    ("plw" . "Planning Weekly")
    ;;
    ("kra" . "Kind: Area")
    ("krr" . "Kind: Reference")
    ("krv" . "Kind: ")
    ;;
    ("rn" . "???")
    ;; Work tags
    ("wa" . "Work: Administrative")
    ("wt" . "Work: Teaching")
    ("ws" . "Work: Student(s)")
    ("wp" . "Work: Parent(s)")
    ("wm" . "Work: Meeting")))

(defvar my/denote-agenda-keywords
  '("p" "w"))

(defvar my/denote-categories
  '("Family"
    "Writing"
    "Branding"
    "Buddhism"))

(use-package denote
  :after org
  :init

  (defun my/prime-org-agenda-files-denote ()
    (mapcar (lambda (kw)
              (add-to-list 'my/denote-keywords
                           `(,kw . ,(capitalize kw))
                           :append))
            denote-known-keywords)
    (setopt denote-known-keywords (mapcar #'car my/denote-keywords))
    (mapcar (lambda (kw)
              (let ((files
                     (flatten-list
                      (append
                       (mapcar #'my/denote-list-category-files
                               '("Family"
                                 "Writing"))
                       (mapcar (lambda (kw)
                                 (denote-directory-files
                                  (format "_%s" kw)))
                               my/denote-agenda-keywords)))))
                (when files
                  (mapcar (lambda (file)
                            (add-to-list 'org-agenda-files
                                         file
                                         :append))
                          files))))
            denote-known-keywords)
    org-agenda-files)

  (advice-add #'my/prime-org-agenda-files :after #'my/prime-org-agenda-files-denote)

  (defun my/denote-keywords-prompt (&optional prompt-text initial-keywords)
    (interactive)
    (mapcar (lambda (str)
              (car (string-split str " +")))
            (completing-read-multiple
             (or prompt-text "Select a tag(s): ")
             (or initial-keywords
                 (mapcar (lambda (pair)
                           (let ((kw (car pair))
                                 (desc (cdr pair)))
                             (put-text-property 0 (length kw)
                                                'face 'org-verbatim kw)
                             (put-text-property 0 (length desc)
                                                'face 'org-code desc)
                             (format "%-20s %s" kw desc)))
                         my/denote-keywords)))))
  (defalias #'prot/denote-keywords-prompt #'denote-keywords-prompt)
  (defalias #'denote-keywords-prompt #'my/denote-keywords-prompt)

  (defun my/denote-add-category (category)
    ""
    (let ((cat (cadr (assoc "CATEGORY" (org-collect-keywords '("CATEGORY"))))))
      (unless cat
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp "^[\s\t]*$")
          (insert (format "#+category:   %s\n" category))))))
  ;; (my/denote-add-category "X")

  (defun my/denote-collect-categories ()
    ""
    (seq-uniq
     (append
      my/denote-categories
      (remove
       nil
       (mapcar
        (lambda (file)
          (with-temp-buffer
            (insert-file-contents file)
            (cadr (assoc "CATEGORY" (org-collect-keywords '("CATEGORY"))))))
        (denote-directory-files))))))

  (defun my/denote-category-prompt ()
    (completing-read "Select a category: " (my/denote-collect-categories)))

  ;; The following code was too buggy to deal with it, so I just
  ;; commented it; but if you are interested on watching how
  ;; marginalia works (with an example), take a look to it.  I figured
  ;; out how it worked (as far as I could go) thanks to the info
  ;; manual (info "marginalia")

  ;; (with-eval-after-load 'denote-marginalia

  ;;   (defun my/marginalia-annotate-denote (cand)
  ;;     ""
  ;;     (marginalia--in-minibuffer
  ;;       (with-temp-buffer
  ;;         (insert-file-contents (expand-file-name
  ;;                                (file-name-nondirectory cand)
  ;;                                denote-directory))
  ;;         (when-let (info (org-collect-keywords
  ;;                          '("TITLE"    "DATE"
  ;;                            "FILETAGS" "CATEGORY")))
  ;;           (let ((title (or (cadr (assoc "TITLE" info)) ""))
  ;;                 (date  (or (cadr (assoc "DATE" info)) ""))
  ;;                 (tags  (or (cadr (assoc "FILETAGS" info)) ""))
  ;;                 (categ (or (cadr (assoc  "CATEGORY" info)) "")))
  ;;             (marginalia--fields
  ;;              (categ :face 'marginalia-key)
  ;;              (title :face 'marginalia-string)
  ;;              (date  :face 'marginalia-date)
  ;;              (tags  :face 'marginalia-mode)
  ;;              ))))))

  ;;   (add-to-list 'marginalia-annotator-registry
  ;;                `(denote ,#'my/marginalia-annotate-denote builtin none)
  ;;                :append))
  ;; (add-to-list 'marginalia-prompt-categories
  ;;              '("\\<denote\\>" . denote)
  ;;              :append)

  (defalias #'prot/denote #'denote)

  (defun my/denote (&optional title keywords category subdir)
    ;; (defun denote (&optional title keywords file-type subdirectory date template signature) ...)
    (interactive)
    (let ((c_ (or category (my/denote-category-prompt))))
      (if subdir
          (let ((denote-prompts
                 (denote--add-prompts
                  '(subdirectory))))
            (call-interactively #'prot/denote))
        (call-interactively #'prot/denote))
      (my/denote-add-category c_)))

  (defalias #'denote #'my/denote)

  (defun my/denote-list-category-files (category)
    "Returns a list of all the denote files within a CATEGORY."
    (remove nil
            (mapcar (lambda (file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (let ((cat
                               (cadr
                                (assoc
                                 "CATEGORY"
                                 (org-collect-keywords '("CATEGORY"))))))
                          (when (and cat
                                     (string= category cat))
                            file))))
                    (denote-directory-files))))
  ;; (my/denote-list-category-files "DENOTE")

  (defun my/denote-select-category-file (category)
    (interactive (list (my/denote-category-prompt)))
    (expand-file-name
     (nth
      4
      (string-split (substring-no-properties
                     (completing-read
                      (format "Select a denote: [%s] "
                              (or category
                                  (my/denote-category-prompt)))
                      (mapcar #'my/denote-format-list-category-file
                              (my/denote-list-category-files category)))
                     0)
                    " | "))
     denote-directory))

  (defun my/denote-open-category-file (arg &optional category read-only)
    (interactive "P")
    (message "%S" arg)
    (let* ((categ (or category (my/denote-category-prompt)))
           (file-name
            (expand-file-name
             (nth
              4
              (string-split (substring-no-properties
                             (completing-read
                              (format "Select a denote: [%s] " categ)
                              (mapcar #'my/denote-format-list-category-file
                                      (my/denote-list-category-files categ)))
                             0)
                            " | "))
             denote-directory)))
      (if (or arg
              read-only)
          (progn
            (funcall #'find-file-read-only file-name)
            (message "Opened in RO mode! (%S)" read-only))
        (funcall #'find-file file-name)
        (message "Opened in WR mode! (%S)" read-only))))
  ;; (my/denote-open-category-file :ro)

  (defun my/denote-open-category-file-read-only (&optional category read-only)
    (interactive)
    (my/denote-open-category-file :read-only category read-only))
  ;; (my/denote-open-category-file-read-only)

  (defun my/denote-format-list-category-file (file)
    ""
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((attrs (org-collect-keywords '("TITLE" "FILETAGS"
                                            "CATEGORY" "DATE")))
             (title    (or  (cadr (assoc "TITLE"
                                         attrs))
                            ""))
             (filetags (or (cadr (assoc "FILETAGS"
                                        attrs))
                           ""))
             (date     (or (cadr (assoc "DATE"
                                        attrs))
                           "or"))
             (category (or (cadr (assoc "CATEGORY"
                                        attrs))
                           ""))
             (f (replace-regexp-in-string
                 (format "^%s" (file-name-as-directory denote-directory))
                 ""
                 file)))
        (put-text-property 0 (length title)
                           'face 'org-document-title
                           title)
        (put-text-property 0 (length date)
                           'face 'org-date
                           date)
        (put-text-property 0 (length filetags)
                           'face 'org-tag
                           filetags)
        (put-text-property 0 (length category)
                           'face 'org-document-info
                           category)
        (put-text-property 0 (length f)
                           'face 'marginalia-file-name
                           f)
        (format "%-20s | %-80s | %-15s | %-25s | %s"
                category
                title
                filetags
                date
                f))))

  (defun my/denote-directory-subdiretories-files ()
    (let ((sub-directories (denote-directory-subdirectories)))
      (append
       (denote-directory-files)
       (when sub-directories
         (remove
          nil
          (car
           (mapcar
            (lambda (subdir)
              (let ((ff (directory-files subdir)))
                (mapcar
                 (lambda (f)
                   (unless (string-suffix-p "." f)
                     (expand-file-name  f subdir)))
                 ff)))
            sub-directories)))))))
  ;; (my/denote-directory-subdiretories-files)

  (defun my/denote-open ()
    (interactive)
    (let* ((sub-directories (denote-directory-subdirectories))
           (file
            (expand-file-name
             (nth 4
                  (string-split
                   (expand-file-name
                    (completing-read
                     "Select a denote: "
                     (mapcar #'my/denote-format-list-category-file
                             (my/denote-directory-subdiretories-files)))
                    denote-directory)
                   " +| +"))
             denote-directory)))
      (if (and file
               (file-exists-p file))
          (find-file file)
        (message "File `%s' doesn't exist!" file))))

  (defalias #'prot/denote-subdirectory #'denote-subdirectory)

  (defun my/denote-subdirectory ()
    (interactive)
    (my/denote nil nil nil :subdir))

  (defalias #'denote-subdirectory #'my/denote-subdirectory)

  (when (fboundp #'my/prime-org-agenda-files-denote)
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode)
                         (string-prefix-p denote-directory default-directory))
                (my/prime-org-agenda-files-denote)))))

  :config
  (my/prime-org-agenda-files))

(provide 'config-org-agenda-denote)
;;; config-org-agenda-denote.el ends here
