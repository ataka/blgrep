;;; blgrep.el --- Block grep -*-emacs-lisp-*-

;; Copyright (C) 2004, 2005 Masayuki Ataka <ataka@milk.freemail.ne.jp>

;; Author: Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; Keywords: tools, convenience

;; This file is not part of GNU Emacs.

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

;;; Code:

;;
;; User Options.
;;
(defvar blgrep-highlight-match-string (fboundp 'overlay-put)
  "*If non-nil, highlight matched text in blgrep buffer.")

(defvar blgrep-buffer-name-alias-alist nil
  "*Alist of aliases for buffer name in modeline.
Each element looks like (ALISES . BUFFER-FILE-NAME).")

(defvar blgrep-boundary "^"
  "*Regexp for boundary of text that blgrep target.
blgrep does not scan the text after the boundary.

If multiple boundaris are in the text, blgrep regards the last
regexp as the boundary.  You can write file variable or something
after the boundary.")

;;
;; System Variables
;;
(defvar blgrep-buffer-file-name nil)
(make-variable-buffer-local 'blgrep-buffer-file-name)
(put 'blgrep-buffer-file-name 'permanent-local t)

(defvar blgrep-file-local-variable nil)
(defvar blgrep-highlight-regexp nil)

;;
;; Virtual functions for blgrep
;;
(defvar blgrep-point-min    #'point)
(defvar blgrep-beg-of-block #'beginning-of-line)
(defvar blgrep-end-of-block #'forward-line)
(defvar blgrep-up-block     #'ignore)
(defvar blgrep-target-p     (lambda () t))
(defvar blgrep-top-p        (lambda () t))

;;
;; Font-Lock
;;
(defface blgrep-highlight-match-face nil
  "Face for highlighting the match text in blgrep buffer.")

(defvar blgrep-highlight-match-face 'blgrep-highlight-match-face)

(if (boundp 'isearch-lazy-highlight-face)
    (copy-face 'isearch-lazy-highlight-face 'blgrep-highlight-match-face)
  (copy-face 'secondary-selection 'blgrep-highlight-match-face))

;;
;; blgrep code
;;
(defun blgrep (query rev mode grep-func structure &optional blank-num)
  (unless (memq structure '(block head-and-body hierarchy))
    (error "Unsupported structure type"))
  (or (numberp blank-num) (setq blank-num 0))
  (setq blgrep-highlight-regexp nil)
  (if (equal query "")
      (blgrep-quasi-viewer grep-func)
    (let (scan-result)
      (save-excursion
	(save-restriction
	  (blgrep-save-file-local-variable)
	  (blgrep-narrow)
	  (setq scan-result
		(blgrep-scan query blank-num structure))))
      (unless scan-result
	(setq blgrep-file-local-variable nil)
	(error "No matches for `%s'" query))
      (setq blgrep-highlight-regexp query)
      (let ((count (car scan-result))
	    (block-tree (cdr scan-result)))
	(message "%d matched" count)
	(blgrep-print block-tree rev grep-func))))
  (funcall mode)
  (blgrep-set-local-variable))

(defun blgrep-scan (query blank-num structure)
  "Scan text and return list of block matching QUERY.

This function forks scan procedure by STRUCTURE; One of the
`blgrep-scan-block', `blgrep-scan-head-and-body', and
`blgrep-scan-hierarchy' will be chosen."
  (goto-char (point-min))
  (let ((case-fold-search (and case-fold-search
                               (isearch-no-upper-case-p query t)))
	(blank (make-string blank-num ?\n)))
    (funcall (symbol-function (intern (format "blgrep-scan-%s" structure)))
	     query blank)))

(defun blgrep-scan-block (query blank)
  "Scan code for block."
  ;; funcall: blgrep-end-of-block, blgrep-beg-of-block, blgrep-target-p
  (let ((count 0) pos beg end tree body)
    (while (setq pos (re-search-forward query nil t))
      (when (save-excursion (funcall blgrep-target-p))
	;; Copy body
	(setq beg (progn (funcall blgrep-beg-of-block) (point))
	      end (progn (funcall blgrep-end-of-block) (point)))
	(if (or (< pos beg) (> pos end))
	    (goto-char pos)
	  (setq body (buffer-substring-no-properties beg end)
		count (1+ count))
	  ;; Construct tree
	  (setq tree (cons (concat body blank) tree)))))
    (when (> count 0)
      (cons count tree))))

(defun blgrep-scan-head-and-body (query blank)
  "Scan code for head-and-body."
  ;; funcall: blgrep-end-of-block, blgrep-beg-of-block, blgrep-target-p,
  ;; blgrep-up-block
  (let ((count 0) pos beg end tree head body)
    (while (setq pos (re-search-forward query nil t))
      (when (save-excursion (funcall blgrep-target-p))
	;; Copy body
	(setq beg (progn (funcall blgrep-beg-of-block) (point))
	      end (progn (funcall blgrep-end-of-block) (point)))
	(if (or (< pos beg) (> pos end))
	    (goto-char pos)
	  (setq body (concat (buffer-substring-no-properties beg end) blank)
		count (1+ count))
	  ;; Copy head
	  (save-excursion
	    (setq head (concat (buffer-substring-no-properties
				(progn (funcall blgrep-up-block)
				       (beginning-of-line) (point)) ;beg
				(save-excursion (forward-line 1) (point))) ; end
			       blank)))
	  ;; Construct block tree
	  (if (equal (caar tree) head)
	      (setcar tree `(,@(car tree) ,body))
	    (setq tree (cons `(,head ,body) tree))))))
    (when (> count 0)
      (cons count tree))))

(defun blgrep-scan-hierarchy (query blank)
  "Scan code for hierarchy."
  (let ((count 0) pos beg end tree sub-tree head)
    (while (setq pos (re-search-forward query nil t))
      (when (save-excursion (funcall blgrep-target-p))
	;; body
	(setq beg (progn (funcall blgrep-beg-of-block) (point))
	      end (progn (funcall blgrep-end-of-block) (point)))
	(if (or (< pos beg) (> pos end))
	    (goto-char pos)
	  (setq sub-tree (list (concat blank (buffer-substring-no-properties beg end)))
		count (1+ count))
	  ;; heads
	  (save-excursion
	    (goto-char beg)
	    (unless (funcall blgrep-top-p)
	      (setq beg (progn (funcall blgrep-up-block) (point))
		    end (save-excursion (forward-line 1) (point))
		    head (buffer-substring-no-properties beg end)
		    sub-tree (list (concat blank head)
				   (if (string-match (concat "^" (regexp-quote head))
						     (car sub-tree))
				       (list (substring (car sub-tree) (match-end 0)))
				     sub-tree))))
	    (while (not (funcall blgrep-top-p))
	      (setq beg (progn (funcall blgrep-up-block) (point))
		    end (save-excursion (forward-line 1) (point))
		    head (concat blank (buffer-substring-no-properties beg end))
		    sub-tree (list head sub-tree))))
	  ;; Construct block tree
	  (if (equal (caar tree) (car sub-tree))
	      (setcar tree (blgrep-cons-tree sub-tree (car tree)))
	    (setq tree (cons sub-tree tree))))))
    (when (> count 0)
      (cons count tree))))

(defsubst blgrep-equal-car (o1 o2)
  (equal (car o1) (car o2)))
(defsubst blgrep-cdr-check (o1 o2)
  (or (consp (cadr o1)) (consp (cadr o2))))

(defun blgrep-cons-tree (sub-tree tree)
  "Construct tree from TREE and SUB-TREE."
  (if (and (blgrep-equal-car tree sub-tree)
	   (blgrep-cdr-check tree sub-tree))
      `(,(car tree) ,(blgrep-cons-tree (cadr sub-tree) (cadr tree)) ,@(cddr tree))
    `(,@tree ,@sub-tree)))

;;; Bug Check
;;
;; (blgrep-cons-tree '("* FOO" ("foo")) '("* FOO" ("bar"))        )
;; (blgrep-cons-tree '("* FOO" ("foo")) '("* FOO" ("foo" ("bar"))))
;; (blgrep-cons-tree '("* FOO" ("foo")) '("* FOO" ("foo"))        )
;; (blgrep-cons-tree '("* FOO" ("foo")) '("* FOO" ("foo" "bar"))  )
;; (blgrep-cons-tree '("* FOO" ("foo")) '("* FOO" ("bar") "* BAR"))
;;
;;   => ("* FOO" ("bar" "foo"))
;;   => ("* FOO" ("foo" ("bar")))
;;   => ("* FOO" ("foo" "foo"))
;;   => ("* FOO" ("foo" "bar" "foo"))
;;   => ("* FOO" ("bar" "foo") "* BAR")
;;

(defun blgrep-print (tree rev grep-func)
  "Switch to blgrep buffer and print TREE."
  ;; preparation
  (blgrep-switch-to-buffer grep-func)
  ;; print
  (unless rev
    (setq tree (nreverse tree)))
  (insert (blgrep-concat-tree tree))
  ;; after treatment
  (goto-char (point-min))
  (when (looking-at "\n+")
    (replace-match ""))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(defun blgrep-concat-tree (node)
  "Concatenate NODE or elements of NODE.
NODE should be a string or list of strings."
  (if (atom node)
      node
    (mapconcat (lambda (sub-node) (blgrep-concat-tree sub-node)) node nil)))

(defun blgrep-narrow ()
  "Narrow the text for blgrep.
The text is narrowed between the point that function `blgrep-point-min' returns
and the variable `blgrep-boundary'.
If mark is active, narrow the region, instead."
  (if (blgrep-region-exists-p)
      (narrow-to-region (region-beginning) (region-end))
    (let (beg end)
      (goto-char (point-min))
      (setq beg (funcall blgrep-point-min))
      (blgrep-search-boundary)
      (setq end (point))
      (narrow-to-region beg end))))

(defun blgrep-quasi-viewer (grep-func)
  "Quasi viewer."
  (unless blgrep-buffer-file-name
    (let ((pos (point))
	  (buf (current-buffer)))
      (blgrep-save-file-local-variable)
      (blgrep-switch-to-buffer grep-func)
      (insert-buffer-substring buf)
      (when blgrep-file-local-variable
	(blgrep-search-boundary)
	(delete-region (point) (point-max)))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char pos))))

(defun blgrep-switch-to-buffer (grep-func)
  "Switch to buffer which name is `*GREF-FUNC: FILENAME*'."
  (let* ((buf-file-name (or blgrep-buffer-file-name (buffer-file-name) (buffer-name)))
	 (alias (car (delq nil (mapcar (lambda (cons-cell)
					 (if (equal buf-file-name
						    (expand-file-name (cdr cons-cell)))
					     (car cons-cell)))
				       blgrep-buffer-name-alias-alist))))
	 (bufname (format "*%s: %s*" grep-func
			  (or alias (abbreviate-file-name buf-file-name))))
	 (blwin (get-buffer-window bufname)))
    (if blwin
	(select-window blwin)
      (switch-to-buffer bufname))
    (setq blgrep-buffer-file-name buf-file-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min)))

(defun blgrep-highlight-match (beg end)
  "Highlight match string.
Variable `blgrep-highlight-match-string' controls highlight or not.
`blgrep-highlight-match-face' is used for highlight face."
  (when (and blgrep-highlight-match-string
	     blgrep-highlight-regexp)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward blgrep-highlight-regexp end t)
	(let ((mb (match-beginning 0))
	      (me (match-end 0)))
	  (if (= mb me)			;zero-length match
	      (forward-char 1)
	    ;; non-zero-length match
	    (overlay-put (make-overlay mb me) 'face blgrep-highlight-match-face)))))))

(defun blgrep-region-exists-p ()
  "Return t if mark is active."
  (cond
   ((boundp 'mark-active) mark-active)		  ;For Emacs
   ((fboundp 'region-exists-p) (region-exists-p)) ;For XEmacs
   (t (error "No funcntions for checking region"))))

(defun blgrep-search-boundary ()
  "Return non-nil if searching `blgrep-boundary' succeeds.
Set point to the last `blgrep-boundary' if found, or end of buffer."
  (goto-char (point-max))
  (and blgrep-boundary
       (re-search-backward blgrep-boundary nil t)))

;;
;; Inherite File Local Variable
;;
(defun blgrep-save-file-local-variable ()
  "Save file local variables."
  (save-excursion
    (save-restriction
      (widen)
      ;; Look for "Local Variables:" line in last page.
      ;; Code is copied from file.el in Emacs 22.0.50
      (goto-char (point-max))
      (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)

      (let ((case-fold-search t))
	(when (search-forward "Local Variables:" nil t)
	  (let ((beg (progn (beginning-of-line) (point))))
	    (when (search-forward "End:" nil t)
	      (end-of-line)
	      (unless (eobp)
		(forward-line 1))
	      (setq blgrep-file-local-variable
		    (buffer-substring-no-properties beg (point))))))))))

(defun blgrep-inherite-file-local-variable ()
  "Inherite file local variables."
  (when blgrep-file-local-variable
    (with-temp-buffer
      (insert blgrep-file-local-variable)
      (goto-char (point-min))
      (while (re-search-forward "[^-A-z]mode:" nil t)
	(beginning-of-line)
	(delete-region (point) (progn (forward-line 1) (point))))
      (setq blgrep-file-local-variable (buffer-substring (point-min) (point-max))))
    (let ((buffer-read-only nil))
      (save-excursion
	(goto-char (point-max))
	(unless (progn (skip-chars-backward " \t\n")
		       (eq (char-before) ?))
	  (insert "\n\n\n"))
	(insert blgrep-file-local-variable))
      (setq blgrep-file-local-variable nil)
      (set-buffer-modified-p nil)))
  (hack-local-variables))


;;
;; blgrep grep -- demo funciton
;;
;;;;
;;
;; (defun blgrep-grep (query rev)
;;   "Not good implementation of function occur."
;;   (interactive "sQuery: \nP")
;;   (blgrep query rev)
;;   (blgrep-mode))
;;

;;
;; Templete of blgrep-mode
;;

(defvar blgrep-mode-map nil)
(if blgrep-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; common functions
    (define-key map "q" 'blgrep-quit)
    (define-key map "R" 'blgrep-regrep)
    (define-key map "\C-m" 'blgrep-jump)
    (define-key map "\C-x4\C-m" 'blgrep-jump-other-window)
    (define-key map "\C-x5\C-m" 'blgrep-jump-other-frame)
    ;; common dummy functions
    (define-key map "g" 'blgrep-dummy)
    (define-key map "n" 'blgrep-next)
    (define-key map "p" 'blgrep-previous)
    (define-key map "f" 'blgrep-forward)
    (define-key map "b" 'blgrep-backward)
    ;; movement functions
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map " " 'scroll-up)
    (define-key map [backspace] 'scroll-down)
    (define-key map "a" 'beginning-of-line)
    (define-key map "e" 'end-of-line)
    (define-key map "l" 'recenter)
    ;; misc commands
    (define-key map "." 'set-mark-command)
    (define-key map "h" 'mark-paragraph)
    (define-key map "," 'pop-to-mark-command)
    (define-key map "m" 'point-to-register)
    (define-key map "'" 'register-to-point)
    (define-key map "x" 'exchange-point-and-mark)
    (setq blgrep-mode-map map)))

(define-derived-mode blgrep-mode text-mode "blgrep"
  "Major mode for blgrep.")

(defun blgrep-set-local-variable ()
  "Set file local variables."
  (if (fboundp 'jit-lock-register)
      (jit-lock-register 'blgrep-highlight-match)
    (blgrep-highlight-match (point-min) (point-max)))
  (blgrep-inherite-file-local-variable))

;;
;; blgrep common functions
;;

(defun blgrep-quit ()
  "Quit blgrep."
  (interactive)
  (quit-window))

(defun blgrep-jump-other-window ()
  "Jump to the source at point in other window."
  (interactive)
  (blgrep-jump 'window))

(defun blgrep-jump-other-frame ()
  "Jump to the source at point in other frame."
  (interactive)
  (blgrep-jump 'frame))

(defun blgrep-jump (&optional win-frame)
  "Jump to the source at point."
  (interactive)
  (let (search-text (pos (point)))
    ;; To get search-text.
    (save-excursion
      (let ((end (progn (end-of-line) (skip-syntax-backward "->") (point))) ;eat blank lines
	    (beg (progn (beginning-of-line) (point))))
	(setq search-text (buffer-substring-no-properties beg end)
	      pos (- end pos))))
    ;; Jump to search-text.
    (blgrep-goto-source-at-point search-text pos t win-frame)))

(defun blgrep-goto-source-at-point (text-list &optional pos bol win-frame)
  "Jump to the source at point."
  (let ((buf (or (get-file-buffer blgrep-buffer-file-name)
		 (get-buffer blgrep-buffer-file-name)
		 (if (file-exists-p blgrep-buffer-file-name)
		     (find-file-noselect blgrep-buffer-file-name)
		   (error "No such file `%s'" blgrep-buffer-file-name)))))
    (cond
     ((eq win-frame 'window) (switch-to-buffer-other-window buf))
     ((eq win-frame 'frame) (switch-to-buffer-other-frame buf))
     (t (switch-to-buffer buf))))
  (goto-char (point-min))
  (when (stringp text-list)
    (setq text-list (list text-list)))
  (while text-list
    (if bol
	(re-search-forward (concat "^" (regexp-quote (car text-list))) nil t)
      (search-forward (car text-list) nil t))
    (setq text-list (cdr text-list)))
  (when pos
    (goto-char (- (point) pos))))

(defun blgrep-regrep (query &optional rev)
  "Regrep with the blgrep frontend function which is displayed in mode line."
  (interactive "sQuery: \nP")
  (let ((grep-func (and (string-match "\\*\\([^:]+\\): " (buffer-name))
			(symbol-function (intern (match-string 1 (buffer-name))))))
	(buf (get-file-buffer blgrep-buffer-file-name)))
    (when (and buf grep-func)
      (switch-to-buffer buf)
      (funcall grep-func query rev))))

;;
;; blgrep common dummy functions
;;

(defalias 'blgrep-dummy    'undefined)
(defalias 'blgrep-next     'next-line)
(defalias 'blgrep-previous 'previous-line)
(defalias 'blgrep-forward  'forward-paragraph)
(defalias 'blgrep-backward 'backward-paragraph)

;;;
(provide 'blgrep)

;;; blgrep.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
