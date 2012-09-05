;;; blg-2ch.el --- 2ch grep -*-emacs-lisp-*-

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
;; blg-2ch.el is a part of blgrep package.
;;
;; blg-2ch collects all contents which contains a match for REGEXP from
;; 2ch articles.  At present, Only emacs-w3m is supported.
;;
;; * Frontends of blg-2ch
;;
;; - blg-2ch
;;      List blocks matching regexp.
;; - blg-2ch-header
;;      List blocks matching regexp in header line.
;; - blg-2ch-noheader
;;      List blocks matching regexp except in header line.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-2ch "blg-2ch" "2ch grep." t)
;;   (autoload 'blg-2ch-header "blg-2ch" "2ch grep for header." t)
;;   (autoload 'blg-2ch-noheader "blg-2ch" "2ch grep for no-header." t)
;;

;;; Code:
(require 'blgrep)

;;
;; System Variables
;;
(defvar blg-2ch-w3m-header-regexp "^\\<[0-9]+")

;;;###autoload
(defun blg-2ch (query &optional rev)
  "List blocks matching regexp."
  (interactive "s2ch grep: \nP")
  (blg-2ch-cond query rev))

;;;###autoload
(defun blg-2ch-header (query &optional rev)
  "List blocks matching regexp in header line."
  (interactive "s2ch grep (header): \nP")
  (blg-2ch-cond query rev "-header"))

;;;###autoload
(defun blg-2ch-noheader (query &optional rev)
  "List blocks matching regexp except in header line."
  (interactive "s2ch grep (noheader): \nP")
  (blg-2ch-cond query rev "-noheader"))

(defun blg-2ch-cond (query rev &optional grep-postfix)
  (cond ((memq major-mode '(w3m-mode blg-2ch-w3m-mode))
	 (funcall (intern (concat "blg-2ch-w3m" grep-postfix)) query rev))
	(t (error "Not supported 2ch"))))

;
; blg-2ch-w3m
;
(defun blg-2ch-w3m (query rev &optional grep-func)
  (let ((blgrep-boundary "^[0-9]+ K")
	(blgrep-beg-of-block (lambda ()
			       (re-search-backward blg-2ch-w3m-header-regexp nil t)
			       (beginning-of-line)))
	(blgrep-end-of-block (lambda ()
			       (forward-char 1)
			       (or (re-search-forward blg-2ch-w3m-header-regexp nil t)
				   (goto-char (point-max)))
			       (beginning-of-line)
			       (skip-chars-backward " \n\t")
			       (forward-line 1))))
    (blgrep query rev #'blg-2ch-w3m-mode (or grep-func "blg-2ch") 'block 1)))

(defun blg-2ch-w3m-header (query rev)
  (let ((blgrep-target-p (lambda ()
			   (beginning-of-line)
			   (looking-at blg-2ch-w3m-header-regexp))))
    (blg-2ch-w3m query rev "blg-2ch-header")))

(defun blg-2ch-w3m-noheader (query rev)
  (let ((blgrep-target-p (lambda ()
			   (beginning-of-line)
			   (not (looking-at blg-2ch-w3m-header-regexp)))))
    (blg-2ch-w3m query rev "blg-2ch-noheader")))


;;
;; blg-2ch mode
;;

;
; blg-2ch-w3m
;
(defvar blg-2ch-w3m-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-2ch-w3m-mode-map))
  (substitute-key-definition 'blgrep-dummy 'blg-2ch-w3m new-map old-map)
  (substitute-key-definition 'blgrep-next 'blg-2ch-w3m-next-item new-map old-map)
  (substitute-key-definition 'blgrep-previous 'blg-2ch-w3m-previous-item new-map old-map)
  (substitute-key-definition 'blgrep-forward 'blg-2ch-w3m-next-item new-map old-map)
  (substitute-key-definition 'blgrep-backward 'blg-2ch-w3m-previous-item new-map old-map))

(define-derived-mode blg-2ch-w3m-mode blgrep-mode "blg-2ch (w3m)"
  "Major mode for blg-2ch."
  (run-hooks 'blgrep-mode-hook))

(defun blg-2ch-w3m-next-item (&optional arg)
  (interactive "p")
  (forward-char 1)
  (re-search-forward blg-2ch-w3m-header-regexp nil t arg)
  (beginning-of-line))

(defun blg-2ch-w3m-previous-item (&optional arg)
  (interactive "p")
  (re-search-backward blg-2ch-w3m-header-regexp nil t arg))

;;;
(provide 'blg-2ch)

;; blg-2ch.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
