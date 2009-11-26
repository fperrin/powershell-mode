;;; pshell-mode.el --- Mode for editing Powershell scripts

;; Copyright (C) 2009 Frédéric Perrin

;; Author: Frédéric Perrin
;; Keywords: Powershell, Monad, MSH

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

(defvar pshell-indent 8
  "Amount of horizontal space to indent after, for instance, an
opening brace")

(defvar pshell-continuation-indent 4
  "Amount of horizontal space to indent a continuation line")

(defvar pshell-continued-regexp  ".*\\(|[\\t ]*\\|`\\)$"
  "Regexp matching a continued line (ending either with an
explicit backtick, or with a pipe).")

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
    ;; a very simple indentation method: if on a continuation line (i.e. the
    ;; previous line ends with a trailing backtick or pipe), we indent relative
    ;; to the continued line; otherwise, we indent relative to the ([{ that
    ;; opened the current block.
    (if (pshell-continuation-line-p)
	(progn
	  (while (pshell-continuation-line-p)
	    (forward-line -1))
	  (+ (current-indentation) pshell-continuation-indent))
      (condition-case nil
	  (progn
	    (backward-up-list)
	    ;; indentation relative to the opening paren: if there is text (no
	    ;; comment) after the opening paren, vertically align the block
	    ;; under the opening paren; if we are looking at the closing
	    ;; paren, reset the indentation; otherwise, indent the block by
	    ;; pshell-indent.
	    (cond ((not (looking-at ".[\t ]*\\(#.*\\)?$"))
		   (1+ (current-column)))
		  (closing-paren
		   (current-indentation))
		  (t
		   (+ (current-indentation) pshell-indent))))
	(scan-error ;; most likely, we are at the top-level
	 0)))))

(defun pshell-indent-line ()
  "Indent the current line of powershell mode, leaving the point
in place if it is inside the meat of the line"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(amount (save-excursion (pshell-indent-line-amount))))
    (if savep
	(save-excursion (indent-line-to amount))
      (indent-line-to amount))))

(defvar pshell-keywords
  (regexp-opt '("begin" "break" "catch" "continue" "data" "do" "dynamicparam"
		"else" "elseif" "end" "exit" "filter" "finally" "for" "foreach"
		"from" "function" "if" "in" "param" "process" "return"
		"switch" "throw" "trap" "try" "until" "while"))
  "Powershell keywords")

(defvar pshell-font-lock-keywords-3
  (list
   (cons (concat "\\<" pshell-keywords "\\>") 'font-lock-keyword-face)
   '("$\\(\\w+\\)\\>" . '(1 font-lock-variable-name-face)))
  "Keywords for font-locking in Powershell mode. Only one level
of font-locking is defined.")

(defvar pshell-mode-syntax-table (make-syntax-table)
  "Syntax table for Powershell mode")

(modify-syntax-entry ?# "<" pshell-mode-syntax-table)
(modify-syntax-entry ?\n ">" pshell-mode-syntax-table)
;; Powershell uses a backtick as its escape character.
(modify-syntax-entry ?` "\\" pshell-mode-syntax-table)
(modify-syntax-entry ?\\ "_" pshell-mode-syntax-table)

(define-derived-mode pshell-mode fundamental-mode "PS"
  "A major mode for editing Powershell script files."
  (set (make-local-variable 'indent-line-function) 'pshell-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '((pshell-font-lock-keywords-3) nil t))
  (set-syntax-table pshell-mode-syntax-table))
