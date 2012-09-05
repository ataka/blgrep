;;; clgrep.el --- ChangeLog Memo grep -*-emacs-lisp-*-

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
;; clgrep.el is a part of blgrep package.
;;
;; clgrep collects all items or entries which matches REGEXP from
;; ChangeLog Memo.  See section "Change Logs" in `GNU Emacs Reference
;; Manual' and commentary section of blg-changelog.el for the term of
;; ChangeLog.
;;
;; clgrep works with clmemo.el, which provides clmemo-mode (ChangeLog
;; Memo mode).  So you need clmemo.el for compiling and using clgrep.el.
;; The latest clmemo.el is available at:
;;
;;   http://isweb22.infoseek.co.jp/computer/pop-club/emacs/changelog.html
;;
;; * Frontends of clgrep
;;
;; - clgrep-item
;;      List items matching regexp.
;; - clgrep-item-header
;;      List items matching regexp in item header.
;; - clgrep-item-tag
;;      List items matching regexp in tag.
;; - clgrep-item-notag
;;      List items matching regexp except tag.
;; - clgrep-item-nourl
;;      List items matching regexp except url.
;; - clgrep-entry
;;      List entries matching regexp.
;; - clgrep-entry-header
;;      List entries matching regexp in entry header.
;; - clgrep-entry-no-entry-header
;;      List entries matching regexp except entry header.
;; - clgrep-entry-tag
;;      List entries matching regexp in tag.
;; - clgrep-entry-notag
;;      List entries matching regexp except tag.
;; - clgrep-entry-nourl
;;      List entries matching regexp except url.
;;
;; clgrep is alias of clgrep-item-notag.
;;

;; [Install]
;;
;; Get the latest clmemo.el and copy it to the directory where clgrep.el
;; exists.
;;

;; [.emacs]
;;
;; Put this in your .emacs
;;
;;   (autoload 'clgrep "clgrep" "ChangeLog grep." t)
;;   (autoload 'clgrep-item "clgrep" "ChangeLog grep." t)
;;   (autoload 'clgrep-item-header "clgrep" "ChangeLog grep for item header" t)
;;   (autoload 'clgrep-item-tag "clgrep" "ChangeLog grep for tag" t)
;;   (autoload 'clgrep-item-notag "clgrep" "ChangeLog grep for item except for tag" t)
;;   (autoload 'clgrep-item-nourl "clgrep" "ChangeLog grep item except for url" t)
;;   (autoload 'clgrep-entry "clgrep" "ChangeLog grep for entry" t)
;;   (autoload 'clgrep-entry-header "clgrep" "ChangeLog grep for entry header" t)
;;   (autoload 'clgrep-entry-no-entry-header "clgrep" "ChangeLog grep for entry except entry header" t)
;;   (autoload 'clgrep-entry-tag "clgrep" "ChangeLog grep for tag" t)
;;   (autoload 'clgrep-entry-notag "clgrep" "ChangeLog grep for tag" t)
;;   (autoload 'clgrep-entry-nourl "clgrep" "ChangeLog grep entry except for url" t)
;;
;; It is good idea to bind them to your favourite keys:
;;
;;   (add-hook 'clmemo-mode-hook
;;             '(lambda ()
;;                (define-key clmemo-mode-map "\C-c\C-g" 'clgrep)))
;;

;; [History]
;;
;; * Satoru Takabayashi wrote original clgrep by Ruby in late 2001.
;;
;; * Masayuki Ataka ported it to EmacsLisp on 2002-03-19.
;;
;; * Masayuki Ataka rewrote clgrep.el from full stratch for blgrep
;;   package on 2004-07-03.
;;

;;; Code:
(require 'blgrep)
(require 'blg-changelog)
(require 'clmemo)

;
; item
;

(defun clgrep-item-common (query rev grep-func)
  (blg-changelog-item-common query rev #'clgrep-mode grep-func))

;;;###autoload
(defalias 'clgrep 'clgrep-item-notag)

;;;###autoload
(defun clgrep-item (query &optional rev)
  "ChangeLog grep."
  (interactive "sclgrep (item): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at "^\t"))))
    (clgrep-item-common query rev "clgrep-item")))

;;;###autoload
(defun clgrep-item-header (query &optional rev)
  "ChangeLog grep for item header"
  (interactive "sclgrep (item header): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at blg-changelog-item-header-regexp))))
    (clgrep-item-common query rev "clgrep-item-header")))

;;;###autoload
(defun clgrep-item-tag (query &optional rev)
  "ChangeLog grep for tag"
  (interactive "sclgrep (item tag): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at "^\t(.+: "))))
    (clgrep-item-common query rev "clgrep-item-tag")))

;;;###autoload
(defun clgrep-item-notag (query &optional rev)
  "ChangeLog grep for item except for tag"
  (interactive "sclgrep (item no-tag): \nP")
  (let* ((target (concat "^\t(.+: " "\\|" blg-changelog-entry-header-regexp))
	 (blgrep-target-p (lambda () (not (blg-changelog-looking-at target)))))
      (clgrep-item-common query rev "clgrep-item-notag")))

;;;###autoload
(defun clgrep-item-nourl (query &optional rev)
  "ChangeLog grep item except for url"
  (interactive "sclgrep (item no-url): \nP")
  (let* ((target (concat (regexp-opt '("http" "ftp" "https")) "://"))
	 (blgrep-target-p (lambda ()
			    (skip-chars-backward "^ \t")
			    (skip-chars-forward "^a-zA-Z")
			    (not (looking-at target)))))
    (clgrep-item-common query rev "clgrep-item-nourl")))


;
; entry
;

(defun clgrep-entry-common (query rev grep-func)
  (blg-changelog-entry-common query rev #'clgrep-mode grep-func))

;;;###autoload
(defun clgrep-entry (query &optional rev)
  "ChangeLog grep for entry"
  (interactive "sclgrep (entry): \nP")
  (clgrep-entry-common query rev "clgrep-entry"))

;;;###autoload
(defun clgrep-entry-header (query &optional rev)
  "ChangeLog grep for entry header"
  (interactive "sclgrep (entry header): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at blg-changelog-entry-header-regexp))))
  (clgrep-entry-common query rev "clgrep-entry-header")))

;;;###autoload
(defun clgrep-entry-no-entry-header (query &optional rev)
  "ChangeLog grep for entry except entry header"
  (interactive "sclgrep (no entry header): \nP")
  (let ((blgrep-target-p (lambda () (not (blg-changelog-looking-at blg-changelog-entry-header-regexp)))))
  (clgrep-entry-common query rev "clgrep-entry-no-header")))

;;;###autoload
(defun clgrep-entry-tag (query &optional rev)
  "ChangeLog grep for tag"
  (interactive "sclgrep (entry tag): \nP")
  (let ((blgrep-target-p (lambda () (blg-changelog-looking-at "^\t(.+: "))))
  (clgrep-entry-common query rev "clgrep-entry-tag")))

;;;###autoload
(defun clgrep-entry-notag (query &optional rev)
  "ChangeLog grep for tag"
  (interactive "sclgrep (entry no-tag): \nP")
  (let ((blgrep-target-p (lambda () (not (blg-changelog-looking-at "^\t(.+: ")))))
  (clgrep-entry-common query rev "clgrep-entry-notag")))

;;;###autoload
(defun clgrep-entry-nourl (query &optional rev)
  "ChangeLog grep entry except for url"
  (interactive "sclgrep (entry no-url): \nP")
  (let* ((target (concat (regexp-opt '("http" "ftp" "https")) "://"))
	 (blgrep-target-p (lambda ()
			    (skip-chars-backward "^ \t")
			    (skip-chars-forward "^a-zA-Z")
			    (not (looking-at target)))))
    (clgrep-entry-common query rev "clgrep-entry-nourl")))

;;
;; clgrep mode
;;

(defvar clgrep-mode-map (copy-keymap blgrep-mode-map))

(let ((old-map blgrep-mode-map)
      (new-map clgrep-mode-map))
  (define-key new-map "G" 'clgrep-item)
  (define-key new-map "i" 'clgrep-item-header)
  (define-key new-map "E" 'clgrep-entry-header)
  (substitute-key-definition 'blgrep-dummy 'clgrep new-map old-map)
  (substitute-key-definition 'blgrep-next 'clmemo-next-item new-map old-map)
  (substitute-key-definition 'blgrep-previous 'clmemo-previous-item new-map old-map)
  (substitute-key-definition 'blgrep-forward 'clmemo-forward-entry new-map old-map)
  (substitute-key-definition 'blgrep-backward 'clmemo-backward-entry new-map old-map)
  (substitute-key-definition 'blgrep-jump 'blg-changelog-jump new-map old-map)
  (substitute-key-definition 'blgrep-jump-other-window 
			     'blg-changelog-jump-other-window new-map old-map)
  (substitute-key-definition 'blgrep-jump-other-frame
			     'blg-changelog-jump-other-frame new-map old-map)
  (define-key new-map "t" 'clmemo-forward-tag)
  (define-key new-map "T" 'clmemo-backward-tag))

(define-derived-mode clgrep-mode change-log-mode "clgrep"
  "Major mode for clgrep."
  (run-hooks 'blgrep-mode-hook))

;;;
(provide 'clgrep)

;; clgrep.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
