;;; blg-elisp.el --- EmacsLisp grep -*-emacs-lisp-*-

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
;; blg-elisp.el is a part of blgrep package.
;;
;; blg-elisp collects all functions, variables, macros, etc... which contains a
;; match for REGEXP.
;;
;; * Frontends of blg-elisp
;;
;; - blg-elisp
;;      List defun matching regexp.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-elisp "blg-elisp" "EmacsLisp grep." t)
;;
;; It is good idea to bind them to your favourite keys:
;;
;;   (add-hook 'emacs-lisp-mode-hook
;;             '(lambda ()
;;                (define-key emacs-lisp-mode-map "\C-c\C-g" 'blg-elisp)))
;;

;;; Code:
(require 'blgrep)

;;
;; Blg-elisp
;;

;;;###autoload
(defun blg-elisp (query rev)
  "EmacsLisp grep."
  (interactive "sEmacsLisp grep: \nP")
  (let ((blgrep-beg-of-block #'beginning-of-defun)
	(blgrep-end-of-block #'end-of-defun))
    (blgrep query rev #'blg-elisp-mode "blg-elisp" 'block 1)))

;;
;; blg-elisp mode
;;

(defvar blg-elisp-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-elisp-mode-map))
  (substitute-key-definition 'blgrep-dummy 'blg-elisp new-map old-map)
  (substitute-key-definition 'blgrep-next 'forward-sexp new-map old-map)
  (substitute-key-definition 'blgrep-previous 'backward-sexp new-map old-map)
  (substitute-key-definition 'blgrep-forward 'blg-elisp-forward-defun new-map old-map)
  (substitute-key-definition 'blgrep-backward 'blg-elisp-backward-defun new-map old-map))

(define-derived-mode blg-elisp-mode emacs-lisp-mode "blg-elisp"
  "Major mode for blg-elisp."
  (run-hooks 'blgrep-mode-hook))

(defun blg-elisp-forward-defun (&optional arg)
  (interactive "p")
  (end-of-defun (1+ arg))
  (beginning-of-defun))

(defun blg-elisp-backward-defun (&optional arg)
  (interactive "p")
  (forward-char -1)
  (beginning-of-defun arg))

;;;
(provide 'blg-elisp)

;; blg-elisp.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
