;;; pshell-mode.el --- Mode for editing Powershell scripts

;; Copyright (C) 2009 Frédéric Perrin

;; Author: Frédéric Perrin
;; Keywords: Powershell, Monad

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Comment: WIP

(setq debug-on-error t)

(defvar pshell-continued-regexp  ".*\\(|[\\t ]*\\|`\\)"
  "Regexp matching a continued line (ending either with an
explicit backtick, or with a pipe).")

(defvar pshell-indent 8
  "Amount of horizontal space to indent after, for instance, an
opening brace")

(defvar pshell-continuation-indent 4
  "Amount of horizontal space to indent a continuation line")

(defun pshell-continuation-line-p ()
  "Returns t is the current line is a continuation line (i.e. the
previous line is a continued line, ending with a backtick or a pipe"
  (interactive)
  (save-excursion
    (forward-line -1)
    (looking-at pshell-continued-regexp)))

(defun pshell-indent-line-amount ()
  "Returns the column to which the current line ought to be indented."
  (interactive)
  (beginning-of-line)
  (let ((closing-paren (looking-at "[\t ]*[])}]")))
    (cond
     ((pshell-continuation-line-p)
      (while (pshell-continuation-line-p)
	(forward-line -1))
      (+ (current-indentation) pshell-continuation-indent))
     ((save-excursion
	(forward-line -1)
	(pshell-continuation-line-p))
      (forward-line -1)
      (- (current-indentation) pshell-continuation-indent))
     (t
      (condition-case nil
	  (progn
	    (backward-up-list)
	    (cond ((not (looking-at ".[\t ]*\\(#.*\\)?$"))
		   (1+ (current-column)))
		  (closing-paren
		   (current-indentation))
		  (t
		   (+ (current-indentation) pshell-indent))))
	(scan-error ;; most likely, we are at the top-level
	 0))))))

(defun pshell-indent-line ()
  "Indent the current line of powershell mode, leaving the point
in place if it is inside the meat of the line"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(amount (save-excursion (pshell-indent-line-amount))))
    (if savep
	(save-excursion (indent-line-to amount))
      (indent-line-to amount))))

 (define-derived-mode pshell-mode fundamental-mode "PS"
   "A major mode for editing Powershell script files."
   (set (make-local-variable 'indent-line-function) 'pshell-indent-line))
