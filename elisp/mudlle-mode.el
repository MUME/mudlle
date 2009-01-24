;; mudlle code editing commands for Emacs
;; Copyright (mudlle) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'help-mode)

(defvar mudlle-mode-abbrev-table nil
  "Abbrev table in use in Mudlle-mode buffers.")
(define-abbrev-table 'mudlle-mode-abbrev-table ())

(defvar mudlle-mode-map ()
  "Keymap used in mudlle mode.")
(if mudlle-mode-map
    ()
  (setq mudlle-mode-map (make-sparse-keymap))
  (define-key mudlle-mode-map "[" 'electric-mudlle-brace)
  (define-key mudlle-mode-map "]" 'electric-mudlle-brace)
  (define-key mudlle-mode-map ";" 'electric-mudlle-semi)
  (define-key mudlle-mode-map "\e\C-q" 'indent-mudlle-exp)
  (define-key mudlle-mode-map "\177" 'backward-delete-char-untabify)
  (define-key mudlle-mode-map "\t" 'mudlle-indent-command)
  (define-key mudlle-mode-map "\e\t" 'mudlle-complete-symbol))

(defvar mudlle-apropos-mode-map nil "Keymap for mudlle-apropos-mode.")
(unless mudlle-apropos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r"   'mudlle-apropos-follow)
    (define-key map "q"    'quit-window)
    (define-key map " "    'scroll-up)
    (define-key map "\177" 'scroll-down)
    (define-key map "a"    'mudlle-apropos)
    (define-key map "n"    'mudlle-apropos-next)
    (define-key map "p"    'mudlle-apropos-previous)
    (setq mudlle-apropos-mode-map map)))

(defvar mudlle-mode-syntax-table nil
  "Syntax table in use in mudlle-mode buffers.")

(if mudlle-mode-syntax-table
    ()
  (setq mudlle-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" mudlle-mode-syntax-table)
  (modify-syntax-entry ?/ ". 124b" mudlle-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" mudlle-mode-syntax-table)
  (modify-syntax-entry ?\r "> b" mudlle-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" mudlle-mode-syntax-table)
  (modify-syntax-entry ?_ "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?+ "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?- "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?= "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?% "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?< "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?> "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?& "." mudlle-mode-syntax-table)
  (modify-syntax-entry ?| "." mudlle-mode-syntax-table))

(defvar mudlle-mode-keyword-syntax-table nil
  "Syntax table in use in mudlle-mode for symbols.")
(unless mudlle-mode-keyword-syntax-table
  (setq mudlle-mode-keyword-syntax-table (copy-syntax-table mudlle-mode-syntax-table))
  (modify-syntax-entry ?_ "w" mudlle-mode-keyword-syntax-table))

(defvar mudlle-font-lock-keywords
  '(
    ("\\<\\(fn\\|if\\|else\\|while\\|for\\|or\\|and\\|not\\|exit\\|loop\\|match\\)\\>" 1 font-lock-keyword-face)
    ("^\\s-*\\(library\\|module\\|requires\\|reads\\|writes\\|defines\\)\\>" 1 font-lock-keyword-face)
    ("\\[\\(\\s-\\|\n\\)*|"
     ("\\(\\s-\\|\n\\)*\\([a-zA-Z][a-zA-Z0-9$?_:?!]*\\)\\(\\s-\\|\n\\)*,?\\(\\s-\\|\n\\)*" nil nil (2 font-lock-variable-name-face)))
    ("\\([a-zA-Z_]+[a-zA-Z0-9$_:?!]*\\)\\(\\s-\\|\n\\)*=\\(\\s-\\|\n\\)*fn" 1 font-lock-function-name-face)
    ))

(defgroup mudlle nil "Major mode for mudlle."
  :group 'languages
  :prefix "mudlle-")

(defcustom mudlle-help-highlight-face 'bold
  "Face used by `mudlle-help' to highlight variable names."
  :group 'mudlle
  :type 'face)

(defcustom mudlle-source-path nil
  "Top directory of mudlle source tree."
  :group 'mudlle
  :type  '(choice (const nil :Tag none) (directory)))
(defcustom mudlle-c-source-path nil
  "Top directory of mudlle's C source tree."
  :group 'mudlle
  :type  '(choice (const nil :Tag none) (directory)))

(defcustom mudlle-indent-level 2
  "*Indentation of mudlle statements with respect to containing block."
  :type 'integer
  :group 'mudlle)
(defcustom mudlle-brace-imaginary-offset 0
  "*Imagined indentation of a mudlle open brace that actually follows a statement."
  :type 'integer
  :group 'mudlle)
(defcustom mudlle-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context."
  :type 'integer
  :group 'mudlle)
(defcustom mudlle-argdecl-indent 5
  "*Indentation level of declarations of mudlle function arguments."
  :type 'integer
  :group 'mudlle)
(defcustom mudlle-continued-statement-offset 2
  "*Extra indent for lines not starting new statements."
  :type 'integer
  :group 'mudlle)
(defcustom mudlle-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to mudlle-continued-statement-offset."
  :type 'integer
  :group 'mudlle)
(defcustom mudlle-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in mudlle code."
  :type 'boolean
  :group 'mudlle)
(defcustom mudlle-tab-always-indent t
  "*Non-nil means TAB in mudlle mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'mudlle)
(defcustom mudlle-string-regexp "\"\\([^\"\\]\\|\\\\.\\|\\\\\n\\)*\""
  "*Regular expression matching a mudlle string."
  :type 'regexp
  :group 'mudlle)
(defcustom mudlle-symbol-regexp "[a-zA-Z][a-zA-Z0-9$_:?!]*"
  "*Regular expression matching a mudlle symbol."
  :type 'regexp
  :group 'mudlle)
(defcustom mudlle-function-data-file nil
  "*File name to load in order to set `mudlle-functions' to an alist with
mudlle documentation."
  :type '(choice (const nil :tag None) (file :must-match t))
  :group 'mudlle)



(defun mudlle-mode ()
  "Major mode for editing mudlle code.
Expression and list commands understand all mudlle brackets.
Tab indents for mudlle code.
Comments are delimited with // and end of line.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{mudlle-mode-map}
Variables controlling indentation style:
 `mudlle-tab-always-indent'
    Non-nil means TAB in mudlle mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `mudlle-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in mudlle code.
 `mudlle-indent-level'
    Indentation of mudlle statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `mudlle-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 `mudlle-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to mudlle-continued-statement-offset.
 `mudlle-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `mudlle-brace-imaginary-offset'
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 `mudlle-argdecl-indent'
    Indentation level of declarations of mudlle function arguments.
 `mudlle-function-data-file'
    Data file of mudlle functions

Turning on mudlle mode calls the value of the variable mudlle-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map mudlle-mode-map)
  (setq major-mode 'mudlle-mode)
  (setq mode-name "mudlle")
  (setq local-abbrev-table mudlle-mode-abbrev-table)
  (set-syntax-table mudlle-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mudlle-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "// ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "// *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'mudlle-comment-indent)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mudlle-font-lock-keywords
			     nil
			     case-fold
			     ((?_ . "w"))
			     mudlle-beginning-of-defun))
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'beginning-of-defun-function)
       'mudlle-beginning-of-defun)
  (run-hooks 'mudlle-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in mudlle code
;; based on its context.
(defun mudlle-comment-indent ()
  (if (looking-at "^//")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-syntax-backward "-")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun electric-mudlle-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (when (and (not arg)
	       (eolp)
	       (or (save-excursion
		     (skip-syntax-backward "-")
		     (bolp))
		   (when mudlle-auto-newline
		     (mudlle-indent-line)
		     (newline)
		     t)))
      (insert last-command-char)
      (mudlle-indent-line)
      (when mudlle-auto-newline
	(newline)
	;; (newline) may have done auto-fill
	(setq insertpos (- (point) 2))
	(mudlle-indent-line))
      (save-excursion
	(if insertpos (goto-char (1+ insertpos)))
	(delete-char -1)))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-mudlle-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if mudlle-auto-newline
      (electric-mudlle-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))


(defun mudlle-indent-command (&optional whole-exp)
  "Indent current line as mudlle code, or in some cases insert a tab character.
If mudlle-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as mudlle
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (mudlle-indent-line))
	    beg end)
	(save-excursion
	  (if mudlle-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not mudlle-tab-always-indent)
	     (save-excursion
	       (skip-syntax-backward "-")
	       (not (bolp))))
	(insert-tab)
      (mudlle-indent-line))))

(defun mudlle-indent-line ()
  "Indent current line as mudlle code.
Return the amount the indentation changed by."
  (with-syntax-table mudlle-mode-keyword-syntax-table
    (let ((indent (calculate-mudlle-indent nil))
	  beg shift-amt
	  (case-fold-search nil)
	  (pos (- (point-max) (point))))
      (beginning-of-line)
      (setq beg (point))
      (cond ((eq indent nil)
	     (setq indent (current-indentation)))
	    ((looking-at "\\s-*#")
	     (setq indent 0))
	    (t
	     (skip-syntax-forward "-")
	     (if (listp indent) (setq indent (car indent)))
	     (cond ((looking-at "\\<else\\>")
		    (setq indent (save-excursion
				   (mudlle-backward-to-start-of-if)
				   (current-indentation))))
		   ((= (following-char) ?\])
		    (setq indent (- indent mudlle-indent-level)))
		   ((= (following-char) ?\[)
		    (setq indent (+ indent mudlle-brace-offset))))))
      (skip-syntax-forward "-")
      (when (eq indent t)
        (setq indent (current-column)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  (if (> (- (point-max) pos) (point))
	      (goto-char (- (point-max) pos)))
	(delete-region beg (point))
	(indent-to indent)
	;; If initial point was within line's indentation,
	;; position after the indentation.  Else stay at same point in text.
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos))))
      shift-amt)))

(defun at-mudlle-statementp (start end)
  (save-excursion
    (goto-char start)
    (cond ((/= (char-after start) ?\[)
	   nil)
	  ((re-search-backward "'\\(\\s-\\|\n\\)*\\=" nil t)  
	   nil)
	  (t
	   t))))

(defun calculate-mudlle-indent (&optional parse-start)
  "Return appropriate indentation for current line as mudlle code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(goto-char (point-min)))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))

      (when (and containing-sexp
		 (= (char-after containing-sexp) ?\[))
	(let ((start (point))
	      open-bar)
	  (goto-char containing-sexp)
	  (and (re-search-forward "\\=\\[\\(\\s-\\|\n\\)*|" indent-point t)
	       (setq open-bar (point))
	       (looking-at "\\([^|]*\\)")
	       (>= (match-end 1) indent-point)
	       (setq containing-sexp (1- open-bar)))
	  (goto-char start)))

      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((looking-at "^\\s-*\\(library\\|module\\|requires\\|defines\\|reads\\|writes\\)\\>")
	     0)
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     ;; or may be function argument declaration.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (goto-char indent-point)
	     (skip-syntax-forward "-")
	     (if (= (following-char) ?\[)
		 0   ; Unless it starts a function body
	       (mudlle-backward-to-noncomment (or parse-start (point-min)))
	       ;; Add a little if this is a continuation line.
	       (if (or (bobp)
		       (memq (preceding-char) '(?\) ?\; ?\])))
		   0 mudlle-continued-statement-offset)))
	    ((not (at-mudlle-statementp containing-sexp indent-point))
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (when (looking-at "[ \t]*[^ \t\n]")
	       (skip-syntax-forward "-"))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (mudlle-backward-to-noncomment containing-sexp)
	     ;; Now we get the answer.
	     (if (and (not (memq (preceding-char) '(nil ?\; ?\[ ?\|)))
		      (save-excursion
			(goto-char indent-point)
			(skip-chars-forward " \t\n\f")
			(/= (following-char) ?\])))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  mudlle-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (mudlle-backward-to-start-of-continued-exp containing-sexp)
		   (+ mudlle-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-syntax-forward "-")
					  (eq (following-char) ?\[))
			  mudlle-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (while (progn (skip-chars-forward " \t\n")
				 (looking-at "//"))
		     ;; Skip over comments following openbrace.
		     (forward-line 1))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(- (current-column)
			   (if (= (following-char) ?\[) mudlle-brace-offset 0))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If mudlle-indent-level is zero,
		 ;; use mudlle-brace-offset + mudlle-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in mudlle-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop mudlle-indent-level))
			(+ mudlle-brace-offset mudlle-continued-statement-offset)
		      mudlle-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the mudlle-brace-imaginary-offset.
		    (progn (skip-syntax-backward "-")
			   (if (bolp) 0 mudlle-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun mudlle-beginning-of-defun ()
  (let (start where done)
    (with-syntax-table mudlle-mode-keyword-syntax-table
      (while (not done)
	(search-backward-regexp "\\<fn\\>\\s-*[\"(]" nil
				'point-min-on-fail)
	(setq where (point))
	(save-excursion
	  (while (and (end-of-line 0)
		      (= (preceding-char) ?\\))
	    nil)
	  (beginning-of-line)
	  (setq start (point)))
	(let* ((parse-state (parse-partial-sexp start where))
	       (in-string (nth 3 parse-state))
	       (in-comment (nth 4 parse-state)))
	  (unless (or in-string in-comment)
	    (setq done t)))))))

(defun mudlle-backward-to-noncomment (lim)
  (let (stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)

      (let ((opoint (point)))
	(beginning-of-line)
	(if (not (search-forward "//" opoint 'move))
	    (setq stop t)
	    (forward-char -2))))))

(defun mudlle-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-syntax-forward "-"))

(defun mudlle-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (with-syntax-table mudlle-mode-keyword-syntax-table
    (or limit (setq limit (point-min)))
    (let ((if-level 1)
	  (case-fold-search nil))
      (while (not (zerop if-level))
	(backward-sexp 1)
	(cond ((looking-at "else\\>")
	       (setq if-level (1+ if-level)))
	      ((looking-at "if\\>")
	       (setq if-level (1- if-level)))
	      ((< (point) limit)
	       (setq if-level 0)
	       (goto-char limit)))))))

(defvar mudlle-functions nil
  "An alist with the help text on mudlle functions. Typically set by loading
the file specified by `mudlle-function-data-file'")
(when mudlle-function-data-file
  (load mudlle-function-data-file 1))

(defun mudlle-find-symbol-start ()
  (with-syntax-table mudlle-mode-keyword-syntax-table
    (let ((orig (point))
	  pos
	  start)
      (if (not (or (looking-at "\\<[a-zA-Z]")
		   (search-backward-regexp "\\<[a-zA-Z]" (point-min) t)))
	  nil
	(setq pos (point))
	(setq start (point))
	(while (and (search-backward-regexp "\\<[a-zA-Z]" (point-min) t)
		    (looking-at mudlle-symbol-regexp)
		    (>= (match-end 0) pos))
	  (setq start (point)))
	(goto-char orig)
	start))))

(defun mudlle-complete-symbol ()
  "Perform completion on Mudlle symbol preceding point"
  (interactive)
  (let (pattern 
	completion
	(end (point))
	(beg (mudlle-find-symbol-start)))
    (if (not beg)
	(error "Nothing to complete")
      (setq pattern (buffer-substring beg end))
      (setq completion (try-completion pattern mudlle-functions))
      (cond ((eq completion t))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string-equal pattern completion))
	     (delete-region beg end)
	     (insert completion))
	    (t
	     (message "Making completion list...")
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list
		(all-completions pattern mudlle-functions)))
	     (message "Making completion list...done"))))))

(defun mudlle-apropos-abbreviate (str)
  (let ((width (- (min fill-column (- (window-width) 1)) 8))
        cut)
    (when (string-match "\n" str)
      (setq str (substring str 0 (match-beginning 0))
	    cut t))
    (when (> (length str) width)
      (setq str (substring str 0 width)
	    cut t))
    (if cut
	(concat str " ...")
      str)))

(define-derived-mode mudlle-apropos-mode fundamental-mode "Mudlle Apropos"
  "Major mode for following hyperlinks in output of mudlle-apropos command.

\\{mudlle-apropos-mode-map}")

(defun mudlle-apropos-follow ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((kw (get-text-property (point) 'mudlle-item)))
      (mudlle-help kw))))

(defun mudlle-apropos-next (&optional arg)
  "Move to the next entry"
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((op (if (< arg 0) 'previous-single-property-change
              'next-single-property-change))
        (lim (and (< arg 0) (point-min)))
        (arg (abs arg)))
    (while (not (zerop arg))
      (let ((p (funcall op (point) 'mudlle-item nil lim)))
        (if (or (not p)
                (= (point) p))
          (error "No more entries"))
        (goto-char p))
      (setq arg (1- arg)))))

(defun mudlle-apropos-previous (&optional arg)
  "Move to the next entry (or ARG'th next)"
  (interactive "p")
  (mudlle-apropos-next (if arg (- arg) -1)))

(defun mudlle-apropos (regexp)
  "Search for mudlle functions"
  (interactive "sApropos mudlle function (regexp): ")
  (switch-to-buffer (get-buffer-create "*mudlle-apropos*"))
  (toggle-read-only 0)
  (erase-buffer)
  (mudlle-apropos-mode)
  (let ((funcs mudlle-functions))
    (while funcs
      (let ((func (car funcs)))
	(when (or (string-match regexp (car func))
		  (and (cadr func)
		       (string-match regexp (cadr func))))
	  (insert-string (propertize (format "%-38s  %s\n    %s\n"
					     (propertize (car func)
							 'face 'bold)
					     (car (cddr func))
					     (if (cadr func)
						 (mudlle-apropos-abbreviate (cadr func))
					       "<no help>"))
				     'mudlle-item (car func)))))
      (setq funcs (cdr funcs))))
  (goto-char (point-min))
  (toggle-read-only 1))

(defun find-file-and-line (path file line)
  (setq file (concat path "/" file))
  (unless (file-readable-p file)
    (error "Cannot open file `%s'" file))
  (find-file file)
  (goto-line line))

(if (< emacs-major-version 22)
    (defun mudlle-find-file-button (path file type)
      (help-xref-button 0 'find-file-and-line
                        (list path file type)))

  (defun mudlle-find-file-button (path file type)
    (help-xref-button 0 'find-file-and-line
                      path file type))

  (define-button-type 'find-file-and-line
    :supertype 'help-xref
    'help-function 'find-file-and-line)

  (define-button-type 'mudlle-help
    :supertype 'help-xref
    'help-function 'mudlle-help))

(defun mudlle-help (&optional keyword)
  "Get help on mudlle function"
  (interactive)
  (unless keyword
    (let (default
	   (beg (mudlle-find-symbol-start)))
      (if beg
	  (save-excursion
	    (goto-char beg)
	    (if (looking-at mudlle-symbol-regexp)
		(setq default (match-string 0)))))
      (let* ((str (completing-read
		   (format "Describe mudlle function%s: "
			   (if default
			       (format " (default %s)" default)
			     ""))
		   mudlle-functions
		   nil
		   t)))
	(setq keyword (if (string-equal str "") default str)))))
 
  (let ((ahelp (assoc keyword mudlle-functions)))
    (if (eq ahelp nil)
	(error "No help on `%s'" keyword)
      (message "Formatting help text...")
      (with-output-to-temp-buffer "*Mudlle help*"
        (let ((type (cddr ahelp)))
          (princ (format "%s is a " (car ahelp)))

          (set-buffer standard-output)

          (let (typename
                (path mudlle-c-source-path)
                (file (cadr type)))
            (message "FILE %s" file)
            (cond ((eq (car type) 'closure)
                   (setq typename "closure")
                   (if (and mudlle-c-source-path
                            (string-match "^compiler/" file))
                       (setq file (concat "mudlle/" (substring file 9)))
                     (setq path mudlle-source-path))
                   )
                  ((eq (car type) 'primitive)
                   (setq typename "primitive"))
                  ((eq (car type) 'varargs)
                   (setq typename "vararg primitive"))
                  ((eq (car type) 'secure)
                   (setq typename (format "secure %s primitive" (cadr (cddr type)))))
                  ((eq (car type) 'variable)
                   (setq typename "variable"))
                  (t
                   (setq typename "something weird")))

            (princ typename)
            (when file
              (princ (format " in %s" file))
              (when path
                (save-excursion
                  (when (re-search-backward (regexp-quote file))
                    (mudlle-find-file-button path file (car (cddr type))))))))

          (princ "\n\n")

          (let ((start (point))
                (helpstr (cadr ahelp)))
            (cond ((stringp helpstr)
                   (insert-string helpstr))
                  ((not helpstr)
                   (insert-string "no help text"))
                  (t
                   (princ helpstr)))
            (goto-char start)

            ;; fontify backtick-prefixed variables
            (while (re-search-forward "`\\(.[a-zA-Z0-9_$?!]*\\)" nil t)
              (if (assoc (match-string 1) mudlle-functions)
                  (help-xref-button 1 'mudlle-help (match-string 1))
                (put-text-property (match-beginning 1) (match-end 1)
                                   'face mudlle-help-highlight-face))
              (goto-char (match-beginning 1))
              (delete-char -1)
              (help-setup-xref (list 'mudlle-help keyword) (interactive-p)))
          
            ;; line wrap
            (goto-char start)
            (let ((tab 0)
                  (col 0)
                  (word 0)
                  (c t)
                  (endc (min fill-column (- (window-width) 1))))
              (while c
                (while (and c (< col endc))
                  (setq c (char-after))
                  (cond ((null c))
                        ((= c ?\t)
                         (setq tab col)
                         (delete-char 1))
                        ((= c ?\n)
                         (forward-char 1)
                         (setq col 0
                               tab 0
                               word 0))
                        (t
                         (forward-char 1)
                         (setq col (1+ col))
                         (when (= c ?\ )
                           (setq word col)))))
                (when c
                  (backward-char (- col word))
                  (insert-char ?\n 1)
                  (insert-char ?\  tab)
                  (setq col tab
                        word tab))))
            )))
      (message "Formatting help text...done"))))


(defun indent-mudlle-exp ()
  "Indent each line of the mudlle grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace at-close
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 state)) ; comment
	      (mudlle-indent-line)
	    (setcar (nthcdr 4 state) nil))
	  (if (or (nth 3 state)) ; string
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-syntax-forward "-")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?\[)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\>"))
		    (setq at-brace (= (following-char) ?\[))
		    (setq at-close (= (following-char) ?\]))
		    (mudlle-backward-to-noncomment opoint)
		    (if at-else
			(progn (mudlle-backward-to-start-of-if opoint)
			       (setq this-indent (current-indentation)))
		      (if (and (not (memq (preceding-char) '(nil ?\; ?\[ ?\|)))
			       (not at-close))
			  ;; Preceding line did not end in comma or semi;
			  ;; indent this line  mudlle-continued-statement-offset
			  ;; more than previous.
			  (progn
			    (mudlle-backward-to-start-of-continued-exp (car contain-stack))
			    (setq this-indent
				  (+ mudlle-continued-statement-offset (current-column)
				     (if at-brace mudlle-continued-brace-offset 0))))
			;; Preceding line ended in comma or semi;
			;; use the standard indent for this level.
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-mudlle-indent
			   (if (car indent-stack)
			       (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (= (following-char) ?\])
		(setq this-indent (- this-indent mudlle-indent-level)))
	    (if (= (following-char) ?\[)
		(setq this-indent (+ this-indent mudlle-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line))))
	    ))))))
