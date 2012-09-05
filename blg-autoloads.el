;; Auto-generated part of blgrep

;;;### (autoloads (blg-2ch-noheader blg-2ch-header blg-2ch) "blg-2ch"
;;;;;;  "blg-2ch.el" (17051 44798))
;;; Generated autoloads from blg-2ch.el

(autoload (quote blg-2ch) "blg-2ch" "\
List blocks matching regexp.

\(fn QUERY &optional REV)" t nil)

(autoload (quote blg-2ch-header) "blg-2ch" "\
List blocks matching regexp in header line.

\(fn QUERY &optional REV)" t nil)

(autoload (quote blg-2ch-noheader) "blg-2ch" "\
List blocks matching regexp except in header line.

\(fn QUERY &optional REV)" t nil)

;;;***

;;;### (autoloads (blg-bib) "blg-bib" "blg-bib.el" (17051 44798))
;;; Generated autoloads from blg-bib.el

(autoload (quote blg-bib) "blg-bib" "\
Bib grep

\(fn QUERY REV)" t nil)

;;;***

;;;### (autoloads (blg-cc) "blg-cc" "blg-cc.el" (17051 44798))
;;; Generated autoloads from blg-cc.el

(autoload (quote blg-cc) "blg-cc" "\
C/C++/Java grep.

\(fn QUERY REV)" t nil)

;;;***

;;;### (autoloads (blg-changelog-entry-header blg-changelog-entry
;;;;;;  blg-changelog-item-header blg-changelog-item) "blg-changelog"
;;;;;;  "blg-changelog.el" (17051 44798))
;;; Generated autoloads from blg-changelog.el

(defalias (quote blg-changelog) (function blg-changelog-item))

(autoload (quote blg-changelog-item) "blg-changelog" "\
ChangeLog grep.

\(fn QUERY &optional REV)" t nil)

(autoload (quote blg-changelog-item-header) "blg-changelog" "\
ChangeLog grep for item heading

\(fn QUERY &optional REV)" t nil)

(autoload (quote blg-changelog-entry) "blg-changelog" "\
ChangeLog grep for entry

\(fn QUERY &optional REV)" t nil)

(autoload (quote blg-changelog-entry-header) "blg-changelog" "\
ChangeLog grep for entry header

\(fn QUERY &optional REV)" t nil)

;;;***

;;;### (autoloads (blg-css-line blg-css) "blg-css" "blg-css.el" (17055
;;;;;;  7242))
;;; Generated autoloads from blg-css.el

(autoload (quote blg-css) "blg-css" "\
CSS grep.

\(fn QUERY REV)" t nil)

(autoload (quote blg-css-line) "blg-css" "\
CSS grep line.

\(fn QUERY REV)" t nil)

;;;***

;;;### (autoloads (blg-elisp) "blg-elisp" "blg-elisp.el" (17051 44798))
;;; Generated autoloads from blg-elisp.el

(autoload (quote blg-elisp) "blg-elisp" "\
EmacsLisp grep.

\(fn QUERY REV)" t nil)

;;;***

;;;### (autoloads (blg-outline-line blg-outline) "blg-outline" "blg-outline.el"
;;;;;;  (17051 44798))
;;; Generated autoloads from blg-outline.el

(autoload (quote blg-outline) "blg-outline" "\
Outline grep.

\(fn QUERY &optional REV)" t nil)

(autoload (quote blg-outline-line) "blg-outline" "\
Outline grep for one line, like grep.

This function also show the hierarchy structure of outline.

\(fn QUERY &optional REV)" t nil)

;;;***

;;;### (autoloads (clgrep-entry-nourl clgrep-entry-notag clgrep-entry-tag
;;;;;;  clgrep-entry-no-entry-header clgrep-entry-header clgrep-entry
;;;;;;  clgrep-item-nourl clgrep-item-notag clgrep-item-tag clgrep-item-header
;;;;;;  clgrep-item) "clgrep" "clgrep.el" (17051 44798))
;;; Generated autoloads from clgrep.el

(defalias (quote clgrep) (quote clgrep-item-notag))

(autoload (quote clgrep-item) "clgrep" "\
ChangeLog grep.

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-item-header) "clgrep" "\
ChangeLog grep for item header

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-item-tag) "clgrep" "\
ChangeLog grep for tag

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-item-notag) "clgrep" "\
ChangeLog grep for item except for tag

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-item-nourl) "clgrep" "\
ChangeLog grep item except for url

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-entry) "clgrep" "\
ChangeLog grep for entry

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-entry-header) "clgrep" "\
ChangeLog grep for entry header

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-entry-no-entry-header) "clgrep" "\
ChangeLog grep for entry except entry header

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-entry-tag) "clgrep" "\
ChangeLog grep for tag

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-entry-notag) "clgrep" "\
ChangeLog grep for tag

\(fn QUERY &optional REV)" t nil)

(autoload (quote clgrep-entry-nourl) "clgrep" "\
ChangeLog grep entry except for url

\(fn QUERY &optional REV)" t nil)

;;;***

(provide 'blg-autoloads)
