;;; blg-changelog.el --- ChangeLog grep -*-emacs-lisp-*-

;; Copyright (C) 2004, 2005 Masayuki Ataka <ataka@milk.freemail.ne.jp>

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
;; blg-changelog.el is a part of blgrep package.
;;
;; blg-changelog collects all items or entries which matches REGEXP in
;; ChangeLog.  See section "Change Logs" in `GNU Emacs Reference Manual'
;; for the term of ChangeLog.
;;
;; * This is a short sample of ChangeLog (GNU Format):
;;   ,
;;  |           2001-01-02  Masayuki Ataka  <ataka@foo.org>
;;  |     ,
;; (1)   |      <-TAB-> * filename.el (function-name): New function.
;;  E  (2)ITEM          In the part of `function-name', author write
;;  N    |              variable-name, option-name and etc.
;;  T     '
;;  R                   * changelog.el (cl-sample): Doc fix.
;;  Y                   (change-log-show-sample): Fix typo.
;;  |                   (change-log-sample-name): New variable.
;;  |
;;   '          2001-01-01  Masayuki Ataka  <ataka@foo.org>
;;
;;                      * filename.el: New file.  This file is written
;;                      for a short sample of ChangeLog file in GNU
;;                      format.
;;
;; (1) ENTRY starts with a header line, which contains the date, author
;;     name and his email address.  ENTRY consists of some ITEMs.
;;
;; (2) ITEM is indented by one TAB.  ITEM starts with `*'(asterisk),
;;     after which follows FILE NAME and FUNCTION NAME (or variable,
;;     macro, user option, etc...).  Normally there should be blank line
;;     between each items.
;;
;; We call the first line of entry ENTRY-HEADER and the first line of
;; item ITEM-HEADER.
;;
;; * Frontends of blg-changelog
;;
;; - blg-changelog-item
;;      List items matching regexp.
;; - blg-changelog-item-header
;;      List items matching regexp in item header.
;; - blg-changelog-entry
;;      List entries matching regexp.
;; - blg-changelog-entry-header
;;      List entries matching regexp in entry header.
;;
;; blg-changelog is alias of blg-changelog-item.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-changelog "blg-changelog" "ChangeLog grep." t)
;;   (autoload 'blg-changelog-item-header "blg-changelog" "ChangeLog grep for item header" t)
;;   (autoload 'blg-changelog-entry "blg-changelog" "ChangeLog grep for entry" t)
;;   (autoload 'blg-changelog-entry-header "blg-changelog" "ChangeLog grep for entry header" t)
;;
;; It is good idea to bind them to your favourite keys:
;;
;;   (add-hook 'change-log-mode-hook
;;             '(lambda ()
;;                (define-key change-log-mode-map "\C-c\C-g" 'blg-changelog)
;;                (define-key change-log-mode-map "\C-c\C-i" 'blg-changelog-item-header)
;;                (define-key change-log-mode-map "\C-c\C-d" 'blg-changelog-entry-header)))
;;

;;; Code:
(require 'blgrep)

;;
;; System Variables
;;

(defvar blg-changelog-entry-header-regexp "^\\<")
(defvar blg-changelog-item-header-regexp "^\t\\* ")
(defvar blg-changelog-header-regexp "^\\(\\<\\|\t\\* \\)")

(defsubst blg-changelog-looking-at (regexp)
  "Return t if REGEXP matches at the beginning of line."
  (beginning-of-line)
  (looking-at regexp))

;
; item
;

(defun blg-changelog-item-common (query rev mode grep-func)
  "Called from function `blg-changelog-item' and `blg-changelog-item-header'."
  (let ((blgrep-point-min    #'blg-changelog-point-min)
	(blgrep-beg-of-block (lambda ()
			       (re-search-backward blg-changelog-item-header-regexp nil t)))
	(blgrep-end-of-block (lambda ()
			       (forward-char 1)
			       (re-search-forward blg-changelog-header-regexp nil 'move)
			       (beginning-of-line)
			       (skip-syntax-backward "->")
			       (forward-line 1)))
	(blgrep-up-block     (lambda ()
			       (re-search-backward blg-changelog-entry-header-regexp nil t))))
    (blgrep query rev mode grep-func 'head-and-body 1)))

;;;###autoload
(defalias 'blg-changelog #'blg-changelog-item)

;;;###autoload
(defun blg-changelog-item (query &optional rev)
  "ChangeLog grep."
  (interactive "sChangeLog grep (item): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at "^\t"))))
    (blg-changelog-item-common query rev #'blg-changelog-mode "blg-changelog-item")))

;;;###autoload
(defun blg-changelog-item-header (query &optional rev)
  "ChangeLog grep for item heading"
  (interactive "sChangeLog grep (item header): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at blg-changelog-item-header-regexp))))
    (blg-changelog-item-common query rev #'blg-changelog-mode "blg-changelog-item-header")))

;
; entry
;

(defun blg-changelog-entry-common (query rev mode grep-func)
  "Called from function `blg-changelog-entry' and `blg-changelog-entry-header'."
  (let ((blgrep-point-min     #'blg-changelog-point-min)
	(blgrep-beg-of-block (lambda ()
			       (re-search-backward blg-changelog-entry-header-regexp nil t)))
	(blgrep-end-of-block (lambda ()
			       (forward-char 1)
			       (re-search-forward blg-changelog-entry-header-regexp nil 'move)
			       (beginning-of-line)
			       (skip-syntax-backward "->")
			       (forward-line 1))))
    (blgrep query rev mode grep-func 'block 1)))

;;;###autoload
(defun blg-changelog-entry (query &optional rev)
  "ChangeLog grep for entry"
  (interactive "sChangeLog grep (entry): \nP")
  (blg-changelog-entry-common query rev #'blg-changelog-mode "blg-changelog-entry"))

;;;###autoload
(defun blg-changelog-entry-header (query &optional rev)
  "ChangeLog grep for entry header"
  (interactive "sChangeLog grep (entry header): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at blg-changelog-entry-header-regexp))))
    (blg-changelog-entry-common query rev #'blg-changelog-mode "blg-changelog-entry-header")))



(defun blg-changelog-point-min ()
  "Return the minimum value of point in the ChangeLog.
If file starts with a copyright and permission notice, skip them.
Assume they end at first blank line."
  (when (looking-at "Copyright")
    (search-forward "\n\n")
    (skip-chars-forward "\n"))
  (point))

;;
;; blg-changelog mode
;;

(defvar blg-changelog-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-changelog-mode-map))
  (define-key new-map "i" 'blg-changelog-item-header)
  (define-key new-map "d" 'blg-changelog-entry-header)
  (substitute-key-definition 'blgrep-dummy 'blg-changelog new-map old-map)
  (substitute-key-definition 'blgrep-next 'blg-changelog-next-item new-map old-map)
  (substitute-key-definition 'blgrep-previous 'blg-changelog-previous-item new-map old-map)
  (substitute-key-definition 'blgrep-forward 'blg-changelog-forward-entry new-map old-map)
  (substitute-key-definition 'blgrep-backward 'blg-changelog-backward-entry new-map old-map)
  (substitute-key-definition 'blgrep-jump 'blg-changelog-jump new-map old-map)
  (substitute-key-definition 'blgrep-jump-other-window 'blg-changelog-jump-other-window new-map old-map)
  (substitute-key-definition 'blgrep-jump-other-frame 'blg-changelog-jump-other-frame new-map old-map))

(define-derived-mode blg-changelog-mode change-log-mode "blg-changelog"
  "Major mode for blg-changelog."
  (run-hooks 'blgrep-mode-hook))


(defun blg-changelog-next-item (&optional arg)
  "Move to the beginning of the next item
With argument ARG, do it ARG times;
a negative argument ARG = -N means move previous N items."
  (interactive "p")
  (re-search-forward blg-changelog-item-header-regexp nil t arg)
  (beginning-of-line)
  (skip-chars-forward "\t"))

(defun blg-changelog-previous-item (&optional arg)
  "Move to the beginning of the previous item
With argument ARG, do it ARG times;
a negative argument ARG = -N means move next N items."
  (interactive "p")
  (re-search-backward blg-changelog-item-header-regexp nil t arg)
  (skip-chars-forward "\t"))

(defun blg-changelog-forward-entry (&optional arg)
  "Move forward to beginning of entry.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N entries."
  (interactive "p")
  (if (and arg (< arg 0))
      (blg-changelog-backward-entry (- arg))
    (beginning-of-line)
    (forward-char 1)
    (re-search-forward blg-changelog-entry-header-regexp nil t arg)
    (beginning-of-line)))

(defun blg-changelog-backward-entry (&optional arg)
  "Move backward to beginning of entry.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N entries."
  (interactive "p")
  (if (and arg (< arg 1))
      (blg-changelog-forward-entry (- arg))
    (beginning-of-line)
    (backward-char 1)
    (re-search-backward blg-changelog-entry-header-regexp nil t arg)
    (beginning-of-line)))

(defun blg-changelog-jump (&optional win-frame)
  "Jump to the source at point."
  (interactive)
  (let (cl-list (pos (point)))
    ;; To get cl-list
    (save-excursion
      ;; item
      (if (blg-changelog-looking-at blg-changelog-entry-header-regexp)
          (progn
            (end-of-line)
            (setq pos (- (point) pos))
            (beginning-of-line))
        (let ((beg (progn (skip-syntax-forward "->")
                          (end-of-line)
                          (blg-changelog-previous-item)
                          (beginning-of-line) (point)))
              (end (progn (end-of-line)
                          (when (re-search-forward blg-changelog-header-regexp nil t)
                            (beginning-of-line)
                            (skip-syntax-backward "->"))
                          (point))))
          (setq cl-list (list (buffer-substring-no-properties beg end))
                pos (- end pos))
          (blg-changelog-backward-entry)))
      ;; entry
      (setq cl-list (cons (buffer-substring-no-properties
                           (point) (progn (end-of-line) (point)))
                          cl-list)))
    ;; Jump to text
    (blgrep-goto-source-at-point cl-list pos t win-frame)))

(defun blg-changelog-jump-other-window ()
  "Jump to the source at point in other window."
  (interactive)
  (blg-changelog-jump 'window))

(defun blg-changelog-jump-other-frame ()
  "Jump to the source at point in other frame."
  (interactive)
  (blg-changelog-jump 'frame))

;;;
(provide 'blg-changelog)

;; blg-changelog.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
