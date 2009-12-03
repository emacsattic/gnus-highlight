;;; gnus-highlight.el --- easy highlighting of summary and article headers

;; Copyright (C) 2003-2006 Free Software Foundation, Inc.

;; Author: Wes Hardaker

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Variables:

(defgroup gnus-highlight nil
  "summary buffer highlighting configuration."
  :group 'gnus-summary)

(defcustom gnus-summary-highlight-expressions nil
  "Defines expressions which cause a line to be highlighted.

This should be a assoc list full of EXPRESSIONs and HIGHLIGHTs, IE:

      '((EXPRESSION . HIGHLIGHT)
        (EXPRESSION . HIGHLIGHT)
        ...)

Every element in the list will be evaluated for a potential match
against the EXPRESSION.

EXPRESSION should a regular expression in string format.  The
highlighted region will be mapped to either the entire line or a
particular region of the matched expression if () grouping is used.
See the match keyword in HIGHLIGHT, below, for details.

HIGHLIGHT is one of the following forms:

     - A color STRING, which is the equivelent to '(bg . STRING)

     - An association list with the following possible keys:

       sub: A sub-list to recursively process, the format of which is
       identical to the format of this variable.  This is usefully for
       putting stop clauses in a sub-list to pick between only 1 of a
       number of expression matches.  Note that other face data within
       this HIGHILIGHT expression is ignored when a sub clause is
       specified, although the stop clause is still honored.

       face: A face to apply to the region.  If not specified, an
       automatic face will be generated using the fg and bg tokens
       below (because the author hates defining faces by hand he made
       it easy to do in these definitions):

       bg: the background color for highlighting.

       fg: the foreground color for highlighting.

       match: a integer field indicating which match position to use
       in a regexp that contains multiple () groupings.  Defaults to
       1.  note that if EXPRESSION contains no () groupings, the
       entire line will be highlighted.

       stop: Any value (including nil) assigned to stop will cause the
       processing of the current EXPRESSION/HIGHLIGHT pairs to stop
       after this one if it succeeds.  (note that the default is to
       contiune, and if this current processing is contained within a
       \"sub\"-clause then processing will finish within the
       \"sub\"-clause and return upward and finish the parent list
       (assuming it didn't contain a stop clause as well).

Example usage:

     (require 'gnus-highlight)
     (setq gnus-summary-highlight-expressions '(
       ;; turns \"keyword\" green.
       (\".*\\\\(keyword\\\\)\"                          . \"green\")
       ;; turns entire line yellow
       (\".*otherword\"                              . ((face . \"yellow\")))
       ;; turns \"thirdex\" blue
       (\".*\\\\(choice1\\\\|choice2\\\\).*\\\\(thirdex\\\\)\" . ((face . \"blue\"
                                                       match . 2)))
     )
"
  :group 'gnus-highlight
  )

(defcustom gnus-article-highlight-expressions nil
  "Defines article highlight expressions to highlight headers with.
See the help for gnus-summary-highlight-expressions for information
about legal values for this variable.

Example:

	(setq gnus-treat-highlight-headers 'head)
	(setq gnus-article-highlight-expressions
	      '(
                ;; highlights \"keyword\" in any header.
		(\".*\\(keyword\\)\" . \"yellow\")
                ;; highlights particular source addresses
		(\"From:.*\\(somewhere\\|elsewhere\\)\" . \"red2\")
		))

"
  :group 'gnus-highlight
  )

(defcustom gnus-treat-highlight-expressions t
  "Emphasize text based on `gnus-article-highlight-expressions'.
Valid values are nil, t, `head'."
  :group 'gnus-highlight)
(put 'gnus-treat-highlight-expressions 'highlight t)

;;; Internal Variables:

(defvar gnus-highlight-face-map nil)

;;; Code:

(defun gnus-highlight-get-face (faceinfo)
  "Return an automatically created face symbol.
FACEINFO should be either a symbol or a textual description of a
background color."
  (let (result
	(autoname "gnus-highlight-autoface"))
    (if (setq result (assq 'bg faceinfo))
	(setq autoname (concat autoname "-bg-" (cdr result))))
    (if (setq result (assq 'fg faceinfo))
	(setq autoname (concat autoname "-fg-" (cdr result))))
    (cond
     ((assq 'face faceinfo)
      (cdr (assq 'face faceinfo)) facedescr)
     ;; look up the face in the cache and use if present
     ((setq result (assoc autoname gnus-highlight-face-map))
      (cdr result))
;      (and (facep (cdr result)) (cdr result)))
     ;; create the face
     (t
      (let* ((symname (make-symbol autoname))
	     (facelist (list)))
;;; maybe in the future:
;;	(if (setq result (assq 'facespec faceinfo))
;;	    (setq facelist (cdr (assq 'facespec faceinfo))))
	(if (setq result (or (assq 'bg faceinfo) (assq 'background faceinfo)))
	    (setq facelist (append (list :background (cdr result)) facelist)))
	(if (setq result (or (assq 'fg faceinfo) (assq 'foreground faceinfo)))
	    (setq facelist (append (list :foreground (cdr result)) facelist)))
	(custom-declare-face symname
	  (list (list '((class color)) facelist ))
	  (concat "Auto-generated face for background: " autoname))
	(setq gnus-highlight-face-map
	      (push (cons autoname symname) gnus-highlight-face-map))
	symname)))))
;	(and (facep symname) symname))))))

(defun gnus-highlight-line-by-expression (&optional expressions)
  "Highlight a given line based on EXPRESSIONS.
If EXPRESSIONS is not specified, the
`gnus-summary-highlight-expressions' value will be used."
  (let ((exprs (or expressions gnus-summary-highlight-expressions))
	stop result)
    (mapcar (lambda (def)
	      (if (and (not stop)
		       (looking-at (car def)))
		  ;; apply the face
		  (let* ((facedata (if (stringp (cdr def))
				       (list (cons 'bg (cdr def)))
				       (cdr def)))
			 (start (or (match-beginning
				     (or (cdr (assq 'match facedata))
					 1))
				    (point-at-bol)))
			 (end (or (match-end
				   (or (cdr (assq 'match facedata)) 1))
				  (point-at-eol))))
		    (if (setq result (assq 'sub facedata))
			(gnus-highlight-line-by-expression (cdr result))
		      (gnus-put-text-property-excluding-characters-with-faces
		       start end 'face
		       (gnus-highlight-get-face facedata)))
		    (if (assq 'stop facedata)
			(setq stop t)))))
	  exprs)))

(defun gnus-article-highlight-by-expression ()
  "Highlight header lines as specified by `gnus-article-highlight-expressions'."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (let ((buffer-read-only nil)
	    (inhibit-point-motion-hooks t))
	(widen)
	(article-narrow-to-head)
	(goto-char (point-min))
	(while (progn
	  (gnus-highlight-line-by-expression gnus-article-highlight-expressions)
	  (equal (forward-line 1) 0)
	  ))))))

;; Initialization
(require 'gnus-art)
(add-hook 'gnus-summary-update-hook 'gnus-highlight-line-by-expression t)
(setq gnus-treatment-function-alist
      (append
	      gnus-treatment-function-alist
	      '((gnus-treat-highlight-expressions
		gnus-article-highlight-by-expression))
))

(setq gnus-treat-highlight-expressions 'head)

(provide 'gnus-highlight)

;; gnus-highlight.el ends here
