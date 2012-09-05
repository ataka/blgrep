;;; blg-outline.el --- Outline grep -*-emacs-lisp-*-

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
;; blg-outline.el is a part of blgrep package.
;;
;; blg-outline collects all bodies of outline which contains a match for
;; REGEXP.  See section "Outline Mode" in `GNU Emacs Reference Manual'
;; for Outline mode.
;;
;; * Frontends of blg-outline
;;
;; - blg-outline
;;      List blocks matching regexp.
;; - blg-outline-line
;;      List lines matching regexp.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'blg-outline "blg-outline" "Outline grep." t)
;;   (autoload 'blg-outline-line "blg-outline" "Outline grep for one line, like grep" t)
;;
;; It is good idea to bind them to your favourite keys:
;;
;;   (add-hook outline-mode-hook
;;             '(lambda ()
;;                (define-key outline-mode-map "\C-c\C-g" 'blg-outline)))
;;
;;   (add-hook outline-minor-mode-hook
;;             '(lambda ()
;;                (define-key outline-minor-mode-map "\C-c\C-g" 'blg-outline)))
;;
;;

;;; Code:
(require 'blgrep)
(require 'outline)

;;
;; System Variables
;;

(defvar blg-outline-top-level nil)

;;
;; Blg-Outline
;;

;;;###autoload
(defun blg-outline (query &optional rev)
  "Outline grep."
  (interactive "sOutline grep: \nP")
  (setq blg-outline-top-level (blg-outline-top-level-num))
  (let ((blgrep-point-min    #'blg-outline-point-min)
	(blgrep-beg-of-block #'outline-back-to-heading)
	(blgrep-end-of-block (lambda () (outline-next-visible-heading 1)
			       (skip-chars-backward " \t\n")
			       (forward-line 1)))
	(blgrep-up-block     (lambda () (if (looking-at outline-regexp)
					    (outline-up-heading 1)
					  (outline-back-to-heading))))
	(blgrep-top-p        (lambda () (and (looking-at outline-regexp)
					     (>= blg-outline-top-level (outline-level)))))
	(blgrep-body-include-heading t))
    (blgrep query rev #'blg-outline-mode "blg-outline" 'hierarchy 1)))

;;;###autoload
(defun blg-outline-line (query &optional rev)
  "Outline grep for one line, like grep.

This function also show the hierarchy structure of outline."
  (interactive "sOutline grep (line): \nP")
  (setq blg-outline-top-level (blg-outline-top-level-num))
  (let ((blgrep-point-min #'blg-outline-point-min)
	(blgrep-up-block  (lambda () (if (looking-at outline-regexp)
					 (outline-up-heading 1)
				       (outline-back-to-heading))))
	(blgrep-top-p     (lambda () (and (looking-at outline-regexp)
					  (>= blg-outline-top-level (outline-level))))))
    (blgrep query rev #'blg-outline-mode "blg-outline-line" 'hierarchy)))

(defun blg-outline-point-min ()
  "Return the minimum value of point in the outline file."
  (unless (outline-on-heading-p)
    (outline-next-visible-heading 1))
  (point))

(defun blg-outline-top-level-num ()
  "Return the minimum level number of this outline file."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (looking-at outline-regexp)
	(outline-next-visible-heading 1)
	(when (eq (char-after) ?)
	  (outline-next-visible-heading 1)))
      (outline-level))))

;;
;; blg-outline mode
;;

(defvar blg-outline-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map blg-outline-mode-map))
  (define-key new-map "u" 'outline-up-heading)
  (substitute-key-definition 'blgrep-dummy 'blg-outline new-map old-map)
  (substitute-key-definition 'blgrep-next 'outline-next-visible-heading new-map old-map)
  (substitute-key-definition 'blgrep-previous 'outline-previous-visible-heading new-map old-map)
  (substitute-key-definition 'blgrep-forward 'outline-forward-same-level new-map old-map)
  (substitute-key-definition 'blgrep-backward 'outline-backward-same-level new-map old-map)
  (substitute-key-definition 'blgrep-jump 'blg-outline-jump new-map old-map)
  (substitute-key-definition 'blgrep-jump-other-window
			     'blg-outline-jump-other-window new-map old-map)
  (substitute-key-definition 'blgrep-jump-other-frame
			     'blg-outline-jump-other-frame new-map old-map))


(define-derived-mode blg-outline-mode outline-mode "blg-outline"
  "Major mode for blg-outline."
  (run-hooks 'blgrep-mode-hook))

(defun blg-outline-jump (&optional win-frame)
  "Jump to the source at point."
  (interactive)
  (let (ol-list (pos (point)))
    ;; To get ol-list
    (save-excursion
      ;; body line
      (let ((end (progn (end-of-line) (skip-syntax-backward "->") (point))) ;eat blank lines
	    (beg (progn (beginning-of-line) (point))))
	(setq ol-list (list (buffer-substring-no-properties beg end))
	      pos (- end pos)))
      ;; headings
      (setq blg-outline-top-level (blg-outline-top-level-num))
      (cond
       ((not (looking-at outline-regexp)) (outline-back-to-heading))
       ((not (>= blg-outline-top-level (outline-level))) (outline-up-heading 1))
       (t))
      (while (not (>= blg-outline-top-level (outline-level)))
	(setq ol-list (cons (buffer-substring-no-properties
			     (point) (save-excursion (end-of-line) (point)))
			    ol-list))
	(outline-up-heading 1)))
    ;; Jump to text
    (blgrep-goto-source-at-point ol-list pos t win-frame)))

(defun blg-outline-jump-other-window ()
  "Jump to the source at point in other window."
  (interactive)
  (blg-outline-jump 'window))

(defun blg-outline-jump-other-frame ()
  "Jump to the source at point in other frame."
  (interactive)
  (blg-outline-jump 'frame))

;;;
(provide 'blg-outline)

;; blg-outline.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
