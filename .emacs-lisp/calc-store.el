;; Calculator for GNU Emacs, part II [calc-store.el]
;; Copyright (C) 1990, 1991, 1992, 1993 Free Software Foundation, Inc.
;; Written by Dave Gillespie, daveg@synaptics.com.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.



;; This file is autoloaded from calc-ext.el.
(require 'calc-ext)

(require 'calc-macs)

(defun calc-Need-calc-store () nil)


;;; Memory commands.

(defun calc-store (&optional var)
  (interactive)
  (let ((calc-store-keep t))
    (calc-store-into var))
)
(setq calc-store-keep nil)

(defun calc-store-into (&optional var)
  (interactive)
  (calc-wrapper
   (let ((calc-given-value nil)
	 (calc-given-value-flag 1))
     (or var (setq var (calc-read-var-name "Store: " t)))
     (if var
	 (let ((found (assq var '( ( + . calc-store-plus )
				   ( - . calc-store-minus )
				   ( * . calc-store-times )
				   ( / . calc-store-div )
				   ( ^ . calc-store-power )
				   ( | . calc-store-concat ) ))))
	   (if found
	       (funcall (cdr found))
	     (calc-store-value var (or calc-given-value (calc-top 1))
			       "" calc-given-value-flag)
	     (message "Stored to variable \"%s\"" (calc-var-name var))))
       (setq var (calc-is-assignments (calc-top 1)))
       (if var
	   (while var
	     (calc-store-value (car (car var)) (cdr (car var))
			       (if (not (cdr var)) "")
			       (if (not (cdr var)) 1))
	     (setq var (cdr var)))))))
)

(defun calc-store-plus (&optional var)
  (interactive)
  (calc-store-binary var "+" '+)
)

(defun calc-store-minus (&optional var)
  (interactive)
  (calc-store-binary var "-" '-)
)

(defun calc-store-times (&optional var)
  (interactive)
  (calc-store-binary var "*" '*)
)

(defun calc-store-div (&optional var)
  (interactive)
  (calc-store-binary var "/" '/)
)

(defun calc-store-power (&optional var)
  (interactive)
  (calc-store-binary var "^" '^)
)

(defun calc-store-concat (&optional var)
  (interactive)
  (calc-store-binary var "|" '|)
)

(defun calc-store-neg (n &optional var)
  (interactive "p")
  (calc-store-binary var "n" '/ (- n))
)

(defun calc-store-inv (n &optional var)
  (interactive "p")
  (calc-store-binary var "&" '^ (- n))
)

(defun calc-store-incr (n &optional var)
  (interactive "p")
  (calc-store-binary var "n" '- (- n))
)

(defun calc-store-decr (n &optional var)
  (interactive "p")
  (calc-store-binary var "n" '- n)
)

(defun calc-store-value (var value tag &optional pop)
  (if var
      (let ((old (calc-var-value var)))
	(set var value)
	(if pop (or calc-store-keep (calc-pop-stack pop)))
	(calc-record-undo (list 'store (symbol-name var) old))
	(if tag
	    (let ((calc-full-trail-vectors nil))
	      (calc-record value (format ">%s%s" tag (calc-var-name var)))))
	(and (memq var '(var-e var-i var-pi var-phi var-gamma))
	     (eq (car-safe old) 'special-const)
	     (message "(Note: Built-in definition of %s has been lost)" var))
	(and (memq var '(var-inf var-uinf var-nan))
	     (null old)
	     (message "(Note: %s has built-in meanings which may interfere)"
		      var))
	(calc-refresh-evaltos var)))
)

(defun calc-var-name (var)
  (if (symbolp var) (setq var (symbol-name var)))
  (if (string-match "\\`var-." var)
      (substring var 4)
    var)
)

(defun calc-store-binary (var tag func &optional val)
  (calc-wrapper
   (let ((calc-simplify-mode (if (eq calc-simplify-mode 'none)
				 'num calc-simplify-mode))
	 (value (or val (calc-top 1))))
     (or var (setq var (calc-read-var-name (format "Store %s: " tag))))
     (if var
	 (let ((old (calc-var-value var)))
	   (or old
	       (error "No such variable: \"%s\"" (calc-var-name var)))
	   (if (stringp old)
	       (setq old (math-read-expr old)))
	   (if (eq (car-safe old) 'error)
	       (error "Bad format in variable contents: %s" (nth 2 old)))
	   (calc-store-value var
			     (calc-normalize (if (calc-is-inverse)
						 (list func value old)
					       (list func old value)))
			     tag (and (not val) 1))
	   (message "Stored to variable \"%s\"" (calc-var-name var))))))
)

(defun calc-read-var-name (prompt &optional calc-store-opers)
  (setq calc-given-value nil
	calc-aborted-prefix nil)
  (let ((var (let ((minibuffer-completion-table obarray)
		   (minibuffer-completion-predicate 'boundp)
		   (minibuffer-completion-confirm t))
	       (read-from-minibuffer prompt "var-" calc-var-name-map nil))))
    (setq calc-aborted-prefix "")
    (and (not (equal var ""))
	 (not (equal var "var-"))
	 (if (string-match "\\`\\([-a-zA-Z0-9]+\\) *:?=" var)
	     (if (null calc-given-value-flag)
		 (error "Assignment is not allowed in this command")
	       (let ((svar (intern (substring var 0 (match-end 1)))))
		 (setq calc-given-value-flag 0
		       calc-given-value (math-read-expr
					 (substring var (match-end 0))))
		 (if (eq (car-safe calc-given-value) 'error)
		     (error "Bad format: %s" (nth 2 calc-given-value)))
		 (setq calc-given-value (math-evaluate-expr calc-given-value))
		 svar))
	   (intern var))))
)
(setq calc-given-value-flag nil)

(defvar calc-var-name-map nil "Keymap for reading Calc variable names.")
(if calc-var-name-map
    ()
  (setq calc-var-name-map (copy-keymap minibuffer-local-completion-map))
  (define-key calc-var-name-map " " 'self-insert-command)
  (mapcar (function
	   (lambda (x)
	     (define-key calc-var-name-map (char-to-string x)
	       'calcVar-digit)))
	  "0123456789")
  (mapcar (function
	   (lambda (x)
	     (define-key calc-var-name-map (char-to-string x)
	       'calcVar-oper)))
	  "+-*/^|")
)

(defun calcVar-digit ()
  (interactive)
  (if (calc-minibuffer-contains "var-\\'")
      (if (eq calc-store-opers 0)
	  (beep)
	(insert "q")
	(self-insert-and-exit))
    (self-insert-command 1))
)

(defun calcVar-oper ()
  (interactive)
  (if (and (eq calc-store-opers t)
	   (calc-minibuffer-contains "var-\\'"))
      (progn
	(erase-buffer)
	(self-insert-and-exit))
    (self-insert-command 1))
)

(defun calc-store-map (&optional oper var)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (oper (or oper (calc-get-operator "Store Mapping")))
	  (nargs (car oper)))
     (or var (setq var (calc-read-var-name (format "Store Mapping %s: "
						   (nth 2 oper)))))
     (if var
	 (let ((old (or (calc-var-value var)
			(error "No such variable: \"%s\""
			       (calc-var-name var))))
	       (calc-simplify-mode (if (eq calc-simplify-mode 'none)
				       'num calc-simplify-mode))
	       (values (and (> nargs 1)
			    (calc-top-list (1- nargs) (1+ calc-dollar-used)))))
	   (message "Working...")
	   (calc-set-command-flag 'clear-message)
	   (if (stringp old)
	       (setq old (math-read-expr old)))
	   (if (eq (car-safe old) 'error)
	       (error "Bad format in variable contents: %s" (nth 2 old)))
	   (setq values (if (calc-is-inverse)
			    (append values (list old))
			  (append (list old) values)))
	   (calc-store-value var
			     (calc-normalize (cons (nth 1 oper) values))
			     (nth 2 oper)
			     (+ calc-dollar-used (1- nargs)))))))
)

(defun calc-store-exchange (&optional var)
  (interactive)
  (calc-wrapper
   (let ((calc-given-value nil)
	 (calc-given-value-flag 1)
	 top)
     (or var (setq var (calc-read-var-name "Exchange with: ")))
     (if var
	 (let ((value (calc-var-value var)))
	   (or value
	       (error "No such variable: \"%s\"" (calc-var-name var)))
	   (if (eq (car-safe value) 'special-const)
	       (error "%s is a special constant" var))
	   (setq top (or calc-given-value (calc-top 1)))
	   (calc-store-value var top nil)
	   (calc-pop-push-record calc-given-value-flag
				 (concat "<>" (calc-var-name var)) value)))))
)

(defun calc-unstore (&optional var)
  (interactive)
  (calc-wrapper
   (or var (setq var (calc-read-var-name "Unstore: ")))
   (if var
       (progn
	 (and (memq var '(var-e var-i var-pi var-phi var-gamma))
	      (eq (car-safe (calc-var-value var)) 'special-const)
	      (message "(Note: Built-in definition of %s has been lost)" var))
	 (if (and (boundp var) (symbol-value var))
	     (message "Unstored variable \"%s\"" (calc-var-name var))
	   (message "Variable \"%s\" remains unstored" (calc-var-name var)))
	 (makunbound var)
	 (calc-refresh-evaltos var))))
)

(defun calc-let (&optional var)
  (interactive)
  (calc-wrapper
   (let* ((calc-given-value nil)
	  (calc-given-value-flag 1)
	  thing value)
     (or var (setq var (calc-read-var-name "Let variable: ")))
     (if calc-given-value
	 (setq value calc-given-value
	       thing (calc-top 1))
       (setq value (calc-top 1)
	     thing (calc-top 2)))
     (setq var (if var
		   (list (cons var value))
		 (calc-is-assignments value)))
     (if var
	 (calc-pop-push-record
	  (1+ calc-given-value-flag)
	  (concat "=" (calc-var-name (car (car var))))
	  (let ((saved-val (mapcar (function
				    (lambda (v)
				      (and (boundp (car v))
					   (symbol-value (car v)))))
				   var)))
	    (unwind-protect
		(let ((vv var))
		  (while vv
		    (set (car (car vv)) (calc-normalize (cdr (car vv))))
		    (calc-refresh-evaltos (car (car vv)))
		    (setq vv (cdr vv)))
		  (math-evaluate-expr thing))
	      (while saved-val
		(if (car saved-val)
		    (set (car (car var)) (car saved-val))
		  (makunbound (car (car var))))
		(setq saved-val (cdr saved-val)
		      var (cdr var)))
	      (calc-handle-whys)))))))
)

(defun calc-is-assignments (value)
  (if (memq (car-safe value) '(calcFunc-eq calcFunc-assign))
      (and (eq (car-safe (nth 1 value)) 'var)
	   (list (cons (nth 2 (nth 1 value)) (nth 2 value))))
    (if (eq (car-safe value) 'vec)
	(let ((vv nil))
	  (while (and (setq value (cdr value))
		      (memq (car-safe (car value))
			    '(calcFunc-eq calcFunc-assign))
		      (eq (car-safe (nth 1 (car value))) 'var))
	    (setq vv (cons (cons (nth 2 (nth 1 (car value)))
				 (nth 2 (car value)))
			   vv)))
	  (and (not value)
	       vv))))
)

(defun calc-recall (&optional var)
  (interactive)
  (calc-wrapper
   (or var (setq var (calc-read-var-name "Recall: ")))
   (if var
       (let ((value (calc-var-value var)))
	 (or value
	     (error "No such variable: \"%s\"" (calc-var-name var)))
	 (if (stringp value)
	     (setq value (math-read-expr value)))
	 (if (eq (car-safe value) 'error)
	     (error "Bad format in variable contents: %s" (nth 2 value)))
	 (setq value (calc-normalize value))
	 (let ((calc-full-trail-vectors nil))
	   (calc-record value (concat "<" (calc-var-name var))))
	 (calc-push value))))
)

(defun calc-store-quick ()
  (interactive)
  (calc-store (intern (format "var-q%c" last-command-char)))
)

(defun calc-store-into-quick ()
  (interactive)
  (calc-store-into (intern (format "var-q%c" last-command-char)))
)

(defun calc-recall-quick ()
  (interactive)
  (calc-recall (intern (format "var-q%c" last-command-char)))
)

(defun calc-copy-variable (&optional var1 var2)
  (interactive)
  (calc-wrapper
   (or var1 (setq var1 (calc-read-var-name "Copy variable: ")))
   (if var1
       (let ((value (calc-var-value var1)))
	 (or value
	     (error "No such variable: \"%s\"" (calc-var-name var)))
	 (or var2 (setq var2 (calc-read-var-name
			      (format "Copy variable: %s, to: " var1))))
	 (if var2
	     (calc-store-value var2 value "")))))
)

(defun calc-edit-variable (&optional var)
  (interactive)
  (calc-wrapper
   (or var (setq var (calc-read-var-name
		      (if calc-last-edited-variable
			  (format "Edit: (default %s) "
				  (calc-var-name calc-last-edited-variable))
			"Edit: "))))
   (or var (setq var calc-last-edited-variable))
   (if var
       (let* ((value (calc-var-value var)))
	 (if (eq (car-safe value) 'special-const)
	     (error "%s is a special constant" var))
	 (setq calc-last-edited-variable var)
	 (calc-edit-mode (list 'calc-finish-stack-edit (list 'quote var))
			 t
			 (concat "Editing " (calc-var-name var)))
	 (and value
	      (insert (math-format-nice-expr value (screen-width)) "\n")))))
  (calc-show-edit-buffer)
)
(setq calc-last-edited-variable nil)

(defun calc-edit-Decls ()
  (interactive)
  (calc-edit-variable 'var-Decls)
)

(defun calc-edit-EvalRules ()
  (interactive)
  (calc-edit-variable 'var-EvalRules)
)

(defun calc-edit-FitRules ()
  (interactive)
  (calc-edit-variable 'var-FitRules)
)

(defun calc-edit-GenCount ()
  (interactive)
  (calc-edit-variable 'var-GenCount)
)

(defun calc-edit-Holidays ()
  (interactive)
  (calc-edit-variable 'var-Holidays)
)

(defun calc-edit-IntegLimit ()
  (interactive)
  (calc-edit-variable 'var-IntegLimit)
)

(defun calc-edit-LineStyles ()
  (interactive)
  (calc-edit-variable 'var-LineStyles)
)

(defun calc-edit-PointStyles ()
  (interactive)
  (calc-edit-variable 'var-PointStyles)
)

(defun calc-edit-PlotRejects ()
  (interactive)
  (calc-edit-variable 'var-PlotRejects)
)

(defun calc-edit-AlgSimpRules ()
  (interactive)
  (calc-edit-variable 'var-AlgSimpRules)
)

(defun calc-edit-TimeZone ()
  (interactive)
  (calc-edit-variable 'var-TimeZone)
)

(defun calc-edit-Units ()
  (interactive)
  (calc-edit-variable 'var-Units)
)

(defun calc-edit-ExtSimpRules ()
  (interactive)
  (calc-edit-variable 'var-ExtSimpRules)
)

(defun calc-declare-variable (&optional var)
  (interactive)
  (calc-wrapper
   (or var (setq var (calc-read-var-name "Declare: " 0)))
   (or var (setq var 'var-All))
   (let* (dp decl def row rp)
     (or (and (calc-var-value 'var-Decls)
	      (eq (car-safe var-Decls) 'vec))
	 (setq var-Decls (list 'vec)))
     (setq dp var-Decls)
     (while (and (setq dp (cdr dp))
		 (or (not (eq (car-safe (car dp)) 'vec))
		     (/= (length (car dp)) 3)
		     (progn
		       (setq row (nth 1 (car dp))
			     rp row)
		       (if (eq (car-safe row) 'vec)
			   (progn
			     (while
				 (and (setq rp (cdr rp))
				      (or (not (eq (car-safe (car rp)) 'var))
					  (not (eq (nth 2 (car rp)) var)))))
			     (setq rp (car rp)))
			 (if (or (not (eq (car-safe row) 'var))
				 (not (eq (nth 2 row) var)))
			     (setq rp nil)))
		       (not rp)))))
     (calc-unread-command ?\C-a)
     (setq decl (read-string (format "Declare: %s  to be: " var)
			     (and rp
				  (math-format-flat-expr (nth 2 (car dp)) 0))))
     (setq decl (and (string-match "[^ \t]" decl)
		     (math-read-exprs decl)))
     (if (eq (car-safe decl) 'error)
	 (error "Bad format in declaration: %s" (nth 2 decl)))
     (if (cdr decl)
	 (setq decl (cons 'vec decl))
       (setq decl (car decl)))
     (and (eq (car-safe decl) 'vec)
	  (= (length decl) 2)
	  (setq decl (nth 1 decl)))
     (calc-record (append '(vec) (list (math-build-var-name var))
			  (and decl (list decl)))
		  "decl")
     (setq var-Decls (copy-sequence var-Decls))
     (if (eq (car-safe row) 'vec)
	 (progn
	   (setcdr row (delq rp (cdr row)))
	   (or (cdr row)
	       (setq var-Decls (delq (car dp) var-Decls))))
       (setq var-Decls (delq (car dp) var-Decls)))
     (if decl
	 (progn
	   (setq dp (and (not (eq var 'var-All)) var-Decls))
	   (while (and (setq dp (cdr dp))
		       (or (not (eq (car-safe (car dp)) 'vec))
			   (/= (length (car dp)) 3)
			   (not (equal (nth 2 (car dp)) decl)))))
	   (if dp
	       (setcar (cdr (car dp))
		       (append (if (eq (car-safe (nth 1 (car dp))) 'vec)
				   (nth 1 (car dp))
				 (list 'vec (nth 1 (car dp))))
			       (list (math-build-var-name var))))
	     (setq var-Decls (append var-Decls
				     (list (list 'vec
						 (math-build-var-name var)
						 decl)))))))
     (calc-refresh-evaltos 'var-Decls)))
)

(defun calc-permanent-variable (&optional var)
  (interactive)
  (calc-wrapper
   (or var (setq var (calc-read-var-name "Save variable (default=all): ")))
   (let (pos)
     (and var (or (and (boundp var) (symbol-value var))
		  (error "No such variable")))
     (set-buffer (find-file-noselect (substitute-in-file-name
				      calc-settings-file)))
     (if var
	 (calc-insert-permanent-variable var)
       (mapatoms (function
		  (lambda (x)
		    (and (string-match "\\`var-" (symbol-name x))
			 (not (memq x calc-dont-insert-variables))
			 (calc-var-value x)
			 (not (eq (car-safe (symbol-value x)) 'special-const))
			 (calc-insert-permanent-variable x))))))
     (save-buffer)))
)
(defvar calc-dont-insert-variables '(var-FitRules var-FactorRules
				     var-CommuteRules var-JumpRules
				     var-DistribRules var-MergeRules
				     var-NegateRules var-InvertRules
				     var-IntegAfterRules
				     var-TimeZone var-PlotRejects
				     var-PlotData1 var-PlotData2
				     var-PlotData3 var-PlotData4
				     var-PlotData5 var-PlotData6
				     var-DUMMY
))

(defun calc-insert-permanent-variable (var)
  (goto-char (point-min))
  (if (search-forward (concat "(setq " (symbol-name var) " '") nil t)
      (progn
	(setq pos (point-marker))
	(forward-line -1)
	(if (looking-at ";;; Variable .* stored by Calc on ")
	    (progn
	      (delete-region (match-end 0) (progn (end-of-line) (point)))
	      (insert (current-time-string))))
	(goto-char (- pos 8 (length (symbol-name var))))
	(forward-sexp 1)
	(backward-char 1)
	(delete-region pos (point)))
    (goto-char (point-max))
    (insert "\n;;; Variable \""
	    (symbol-name var)
	    "\" stored by Calc on "
	    (current-time-string)
	    "\n(setq "
	    (symbol-name var)
	    " ')\n")
    (backward-char 2))
  (insert (prin1-to-string (calc-var-value var)))
  (forward-line 1)
)

(defun calc-insert-variables (buf)
  (interactive "bBuffer in which to save variable values: ")
  (save-excursion
    (set-buffer buf)
    (mapatoms (function
	       (lambda (x)
		 (and (string-match "\\`var-" (symbol-name x))
		      (not (memq x calc-dont-insert-variables))
		      (calc-var-value x)
		      (not (eq (car-safe (symbol-value x)) 'special-const))
		      (or (not (eq x 'var-Decls))
			  (not (equal var-Decls '(vec))))
		      (or (not (eq x 'var-Holidays))
			  (not (equal var-Holidays '(vec (var sat var-sat)
							 (var sun var-sun)))))
		      (insert "(setq "
			      (symbol-name x)
			      " "
			      (prin1-to-string
			       (let ((calc-language
				      (if (memq calc-language '(nil big))
					  'flat
					calc-language)))
				 (math-format-value (symbol-value x) 100000)))
			      ")\n"))))))
)

(defun calc-assign (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op ":=" 'calcFunc-assign arg))
)

(defun calc-evalto (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "=>" 'calcFunc-evalto arg))
)

(defun calc-subscript (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "sub" 'calcFunc-subscr arg))
)

