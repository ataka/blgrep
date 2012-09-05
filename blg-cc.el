;;; blg-cc.el --- cc grep -*-emacs-lisp-*-

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
;; blg-cc.el is a part of blgrep package.
;;
;; blg-cc collects all functions, variables, macros, etc... which contains a
;; match for REGEXP from source file of C, C++, and Java.
;;
;; * Frontends of blg-cc
;;
;; - blg-cc
;;      List defun matching regexp.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-cc "blg-cc" "C/C++/Java grep." t)
;;

;;; Code:
(require 'blgrep)

;;
;; System Variables
;;

;;
;; Blg-cc
;;

;;;###autoload
(defun blg-cc (query rev)
  "C/C++/Java grep."
  (interactive (list (read-string (format "%s grep: " mode-name))
		     prefix-arg))
  (let ((blgrep-beg-of-block #'blg-cc-beginning-of-block)
	(blgrep-end-of-block #'blg-cc-end-of-block))
    (blgrep query rev #'blg-cc-mode "blg-cc" 'block 1)))

(defvar blg-cc-type nil)

(defun blg-cc-beginning-of-block ()
  (re-search-backward "^\\(\\s(\\|[a-zA-Z#]\\)" nil 'move)
  (cond
   ((bobp)                       (setq blg-cc-type nil))
   ((looking-at "^#\\c *define") (setq blg-cc-type 'macro))
   ((looking-at "^[a-zA-Z]")     (setq blg-cc-type 'function))
   ((looking-at "^{")            (setq blg-cc-type 'function)
    (re-search-backward "^[a-zA-Z]"))
   (t (error "No type specified"))))

(defun blg-cc-end-of-block ()
  (cond
   ((eq blg-cc-type 'function)
    (end-of-line)
    (skip-chars-backward " \t")
    (if (eq (char-before) ?\;)
	(forward-line)
      (end-of-defun)))
   ((eq blg-cc-type 'macro)
    (end-of-line)
    (while (eq (char-before) ?\\)
      (end-of-line 2))
    (forward-line))
   (t (forward-line 1))))

;;
;; blg-cc mode
;;

(defvar blg-cc-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-cc-mode-map))
  (substitute-key-definition 'blgrep-dummy 'blg-cc new-map old-map)
  (substitute-key-definition 'blgrep-next 'forward-sexp new-map old-map)
  (substitute-key-definition 'blgrep-previous 'backward-sexp new-map old-map)
  (substitute-key-definition 'blgrep-forward 'blg-cc-forward-defun new-map old-map)
  (substitute-key-definition 'blgrep-backward 'blg-cc-backward-defun new-map old-map))

(define-derived-mode blg-cc-mode c-mode "blg-cc"
  "Major mode for blg-cc."
  (run-hooks 'blgrep-mode-hook))

(defun blg-cc-forward-defun (&optional arg)
  (interactive "p")
  (end-of-defun (1+ arg))
  (beginning-of-defun)
  (forward-line -1))

(defun blg-cc-backward-defun (&optional arg)
  (interactive "p")
  (beginning-of-defun arg)
  (forward-line -1))

;;;
(provide 'blg-cc)

;; blg-cc.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
