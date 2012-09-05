;;; blg-bib.el --- bib grep -*-emacs-lisp-*-

;; Copyright (C) 2005 Masayuki Ataka <ataka@milk.freemail.ne.jp>

;; Author: Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; Keywords: tools, convenience

;; This file is a part of blgrep.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; [Overview]
;;
;; blg-bib.el is a part of blgrep package.
;;
;; blg-bib collects all entries which contains a match for REGEXP from bib file.
;;
;; * Frontends of blg-bib
;;
;; - blg-bib
;;      List entries matching regexp.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-bib "blg-bib" "Bib grep." t)
;;
;; It is good idea to bind them to your favourite keys:
;;
;;   (add-hook 'bibtex-mode-hook
;;             '(lambda ()
;;                (define-key bibtex-mode-map "\C-c\C-g" 'blg-bib)))
;;

;;; Code:
(require 'blgrep)

;;
;; System Variables
;;

(defvar blg-bib-entry-list 
  "*List of strings of Bib entry."
  '("Article" "Book" "Booklet" "InBook" "InCollection" "InProceedings" 
    "Manual" "MastersThesis" "Misc" "PhdThesis" "Proceedings" 
    "TechReport" "Unpublished"))

(defvar blg-bib-entry-regexp
  (concat (if (> emacs-major-version 20) "^@\\(?:" "^@\\(")
	  (regexp-opt blg-bib-entry-list) "\\)"))

;;
;; Blg-bib
;;

;;;###autoload
(defun blg-bib (query rev)
  "Bib grep"
  (interactive "sBib grep: \nP")
  (let ((blgrep-beg-of-block (lambda () (re-search-backward blg-bib-entry-regexp nil t)))
	(blgrep-end-of-block (lambda ()
			       (forward-char 1)
			       (or (re-search-forward blg-bib-entry-regexp nil t)
				   (goto-char (point-max)))
			       (re-search-backward "^}")
			       (forward-line 1))))
    (blgrep query rev #'blg-bib-mode "blg-bib" 'block 1)))

;;
;; blg-bib mode
;;

(defvar blg-bib-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-bib-mode-map))
  (substitute-key-definition 'blgrep-dummy 'blg-bib new-map old-map)
  (substitute-key-definition 'blgrep-next 'blg-bib-next-field new-map old-map)
  (substitute-key-definition 'blgrep-previous 'blg-bib-previous-field new-map old-map)
  (substitute-key-definition 'blgrep-forward 'blg-bib-forward-entry new-map old-map)
  (substitute-key-definition 'blgrep-backward 'blg-bib-backward-entry new-map old-map))

(define-derived-mode blg-bib-mode bibtex-mode "blg-bib"
  "Major mode for blg-bib."
  (run-hooks 'blgrep-mode-hook))

(defun blg-bib-next-field (&optional arg)
  ;; FIXME arg
  (interactive "p")
  (forward-line 1)
  (skip-chars-forward "^=")
  (forward-char 1)
  (skip-chars-forward " \t"))

(defun blg-bib-previous-field (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (skip-chars-backward "^=")
  (skip-chars-forward " \t"))

(defun blg-bib-forward-entry (&optional arg)
  (interactive "p")
  (forward-char 1)
  (re-search-forward blg-bib-entry-regexp nil t arg)
  (beginning-of-line))

(defun blg-bib-backward-entry (&optional arg)
  (interactive "p")
  (re-search-backward blg-bib-entry-regexp nil t arg))

;;;
(provide 'blg-bib)

;; blg-bib.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
