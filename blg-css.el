;;; blg-css.el --- CSS grep -*-emacs-lisp-*-

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
;; blg-css.el is a part of blgrep package.
;;
;; blg-css collects all properties which contains a match for REGEXP.
;;
;; * Frontends of blg-css
;;
;; - blg-css
;;      List properties matching regexp.
;;
;; css-mode.el is available at
;;   http://tech.irt.org/articles/js148/el/css-mode.el
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-css "blg-css" "CSS grep." t)
;;   (autoload 'blg-css-line "blg-css" "CSS grep line." t)
;;
;; It is good idea to bind them to your favourite keys:
;;
;;   (add-hook 'css-mode-hook
;;             '(lambda ()
;;                (define-key css-mode-map "\C-c\C-g" 'blg-css)))
;;

;;; Code:
(require 'blgrep)

;;
;; Blg-css
;;

(defvar blg-css-selector-first-letter "^[A-Za-z#.]")

;;;###autoload
(defun blg-css (query rev)
  "CSS grep."
  (interactive "sCSS grep: \nP")
  (let ((blgrep-beg-of-block (lambda () (re-search-backward blg-css-selector-first-letter)))
	(blgrep-end-of-block (lambda () (forward-list) (forward-line))))
    (blgrep query rev #'blg-css-mode "blg-css" 'block 1)))

;;;###autoload
(defun blg-css-line (query rev)
  "CSS grep line."
  (interactive "sCSS grep (line) : \nP")
  (let ((blgrep-up-block (lambda () (re-search-backward blg-css-selector-first-letter))))
    (blgrep query rev #'blg-css-mode "blg-css-line" 'head-and-body)))


;;
;; blg-css mode
;;

(defvar blg-css-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-css-mode-map))
  (substitute-key-definition 'blgrep-dummy 'blg-css new-map old-map)
  (substitute-key-definition 'blgrep-next 'next-line new-map old-map)
  (substitute-key-definition 'blgrep-previous 'previous-line new-map old-map)
  (substitute-key-definition 'blgrep-forward 'blg-css-forward-selector new-map old-map)
  (substitute-key-definition 'blgrep-backward 'blg-css-backward-selector new-map old-map))

(define-derived-mode blg-css-mode emacs-lisp-mode "blg-css"
  "Major mode for blg-css."
  (run-hooks 'blgrep-mode-hook))

(defun blg-css-forward-selector (&optional arg)
  (interactive "p")
  (forward-char 1)
  (re-search-forward blg-css-selector-first-letter nil 'move arg)
  (beginning-of-line))

(defun blg-css-backward-selector (&optional arg)
  (interactive "p")
  (forward-char -1)
  (re-search-backward blg-css-selector-first-letter nil 'move arg))

;;;
(provide 'blg-css)

;; blg-css.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
