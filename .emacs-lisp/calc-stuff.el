;; Calculator for GNU Emacs, part II [calc-stuff.el]
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

(defun calc-Need-calc-stuff () nil)


(defun calc-num-prefix (n)
  "Use the number at the top of stack as the numeric prefix for the next command.
With a prefix, push that prefix as a number onto the stack."
  (interactive "P")
  (calc-wrapper
   (if n
       (calc-enter-result 0 "" (prefix-numeric-value n))
     (let ((num (calc-top 1)))
       (if (math-messy-integerp num)
	   (setq num (math-trunc num)))
       (or (integerp num)
	   (error "Argument must be a small integer"))
       (calc-pop-stack 1)
       (setq prefix-arg num)
       (message "%d-" num))))    ; a (lame) simulation of the real thing...
)


(defun calc-more-recursion-depth (n)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-inverse)
       (calc-less-recursion-depth n)
     (let ((n (if n (prefix-numeric-value n) 2)))
       (if (> n 1)
	   (setq max-specpdl-size (* max-specpdl-size n)
		 max-lisp-eval-depth (* max-lisp-eval-depth n))))
     (message "max-lisp-eval-depth is now %d" max-lisp-eval-depth)))
)

(defun calc-less-recursion-depth (n)
  (interactive "P")
  (let ((n (if n (prefix-numeric-value n) 2)))
    (if (> n 1)
	(setq max-specpdl-size
	      (max (/ max-specpdl-size n) 600)
	      max-lisp-eval-depth
	      (max (/ max-lisp-eval-depth n) 200))))
  (message "max-lisp-eval-depth is now %d" max-lisp-eval-depth)
)


(defun calc-explain-why (why &optional more)
  (if (eq (car why) '*)
      (setq why (cdr why)))
  (let* ((pred (car why))
	 (arg (nth 1 why))
	 (msg (cond ((not pred) "Wrong type of argument")
		    ((stringp pred) pred)
		    ((eq pred 'integerp) "Integer expected")
		    ((eq pred 'natnump)
		     (if (and arg (Math-objvecp arg) (not (Math-integerp arg)))
			 "Integer expected"
		       "Nonnegative integer expected"))
		    ((eq pred 'posintp)
		     (if (and arg (Math-objvecp arg) (not (Math-integerp arg)))
			 "Integer expected"
		       "Positive integer expected"))
		    ((eq pred 'fixnump)
		     (if (and arg (Math-integerp arg))
			 "Small integer expected"
		       "Integer expected"))
		    ((eq pred 'fixnatnump)
		     (if (and arg (Math-natnump arg))
			 "Small integer expected"
		       (if (and arg (Math-objvecp arg)
				(not (Math-integerp arg)))
			   "Integer expected"
			 "Nonnegative integer expected")))
		    ((eq pred 'fixposintp)
		     (if (and arg (Math-integerp arg) (Math-posp arg))
			 "Small integer expected"
		       (if (and arg (Math-objvecp arg)
				(not (Math-integerp arg)))
			   "Integer expected"
			 "Positive integer expected")))
		    ((eq pred 'posp) "Positive number expected")
		    ((eq pred 'negp) "Negative number expected")
		    ((eq pred 'nonzerop) "Nonzero number expected")
		    ((eq pred 'realp) "Real number expected")
		    ((eq pred 'anglep) "Real number expected")
		    ((eq pred 'hmsp) "HMS form expected")
		    ((eq pred 'datep)
		     (if (and arg (Math-objectp arg)
			      (not (Math-realp arg)))
			 "Real number or date form expected"
		       "Date form expected"))
		    ((eq pred 'numberp) "Number expected")
		    ((eq pred 'scalarp) "Number expected")
		    ((eq pred 'vectorp) "Vector or matrix expected")
		    ((eq pred 'numvecp) "Number or vector expected")
		    ((eq pred 'matrixp) "Matrix expected")
		    ((eq pred 'square-matrixp)
		     (if (and arg (math-matrixp arg))
			 "Square matrix expected"
		       "Matrix expected"))
		    ((eq pred 'objectp) "Number expected")
		    ((eq pred 'constp) "Constant expected")
		    ((eq pred 'range) "Argument out of range")
		    (t (format "%s expected" pred))))
	 (punc ": ")
	 (calc-can-abbrev-vectors t))
    (while (setq why (cdr why))
      (and (car why)
	   (setq msg (concat msg punc (if (stringp (car why))
					  (car why)
					(math-format-flat-expr (car why) 0)))
		 punc ", ")))
    (message "%s%s" msg (if more "  [w=more]" "")))
)

(defun calc-why ()
  (interactive)
  (if (not (eq this-command last-command))
      (if (eq last-command calc-last-why-command)
	  (setq calc-which-why (cdr calc-why))
	(setq calc-which-why calc-why)))
  (if calc-which-why
      (progn
	(calc-explain-why (car calc-which-why) (cdr calc-which-why))
	(setq calc-which-why (cdr calc-which-why)))
    (if calc-why
	(progn
	  (message "(No further explanations available)")
	  (setq calc-which-why calc-why))
      (message "No explanations available")))
)
(setq calc-which-why nil)
(setq calc-last-why-command nil)


(defun calc-version ()
  (interactive)
  (message "Calc %s, installed %s" calc-version calc-installed-date))


(defun calc-flush-caches ()
  (interactive)
  (calc-wrapper
   (setq math-lud-cache nil
	 math-log2-cache nil
	 math-radix-digits-cache nil
	 math-radix-float-cache-tag nil
	 math-random-cache nil
	 math-max-digits-cache nil
	 math-checked-rewrites nil
	 math-integral-cache nil
	 math-units-table nil
	 math-decls-cache-tag nil
	 math-eval-rules-cache-tag t
	 math-graph-var-cache nil
	 math-graph-data-cache nil
	 math-format-date-cache nil
	 math-holidays-cache-tag t)
   (mapcar (function (lambda (x) (set x -100))) math-cache-list)
   (message "All internal calculator caches have been reset."))
)


;;; Conversions.

(defun calc-clean (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-with-default-simplification
    (let ((func (if (calc-is-hyperbolic) 'calcFunc-clean 'calcFunc-pclean)))
      (calc-enter-result 1 "cln"
			 (if n
			     (let ((n (prefix-numeric-value n)))
			       (list func
				     (calc-top-n 1)
				     (if (<= n 0)
					 (+ n calc-internal-prec)
				       n)))
			   (list func (calc-top-n 1)))))))
)

(defun calc-clean-num (num)
  (interactive "P")
  (calc-clean (- (if num
		     (prefix-numeric-value num) 
		   (if (and (>= last-command-char ?0)
			    (<= last-command-char ?9))
		       (- last-command-char ?0)
		     (error "Number required")))))
)


(defun calcFunc-clean (a &optional prec)   ; [X X S] [Public]
  (if prec
      (cond ((Math-messy-integerp prec)
	     (calcFunc-clean a (math-trunc prec)))
	    ((or (not (integerp prec))
		 (< prec 3))
	     (calc-record-why "*Precision must be an integer 3 or above")
	     (list 'calcFunc-clean a prec))
	    ((not (Math-objvecp a))
	     (list 'calcFunc-clean a prec))
	    (t (let ((calc-internal-prec prec)
		     (math-chopping-small t))
		 (calcFunc-clean (math-normalize a)))))
    (cond ((eq (car-safe a) 'polar)
	   (let ((theta (math-mod (nth 2 a)
				  (if (eq calc-angle-mode 'rad)
				      (math-two-pi)
				    360))))
	     (math-neg
	      (math-neg
	       (math-normalize
		(list 'polar
		      (calcFunc-clean (nth 1 a))
		      (calcFunc-clean theta)))))))
	  ((memq (car-safe a) '(vec date hms))
	   (cons (car a) (mapcar 'calcFunc-clean (cdr a))))
	  ((memq (car-safe a) '(cplx mod sdev intv))
	   (math-normalize (cons (car a) (mapcar 'calcFunc-clean (cdr a)))))
	  ((eq (car-safe a) 'float)
	   (if math-chopping-small
	       (if (or (> (nth 2 a) (- calc-internal-prec))
		       (Math-lessp (- calc-internal-prec) (calcFunc-xpon a)))
		   (if (and (math-num-integerp a)
			    (math-lessp (calcFunc-xpon a) calc-internal-prec))
		       (math-trunc a)
		     a)
		 0)
	     a))
	  ((Math-objectp a) a)
	  ((math-infinitep a) a)
	  (t (list 'calcFunc-clean a))))
)
(setq math-chopping-small nil)

(defun calcFunc-pclean (a &optional prec)
  (math-map-over-constants (function (lambda (x) (calcFunc-clean x prec)))
			   a)
)

(defun calcFunc-pfloat (a)
  (math-map-over-constants 'math-float a)
)

(defun calcFunc-pfrac (a &optional tol)
  (math-map-over-constants (function (lambda (x) (calcFunc-frac x tol)))
			   a)
)

(defun math-map-over-constants (func expr)
  (math-map-over-constants-rec expr)
)

(defun math-map-over-constants-rec (expr)
  (cond ((or (Math-primp expr)
	     (memq (car expr) '(intv sdev)))
	 (or (and (Math-objectp expr)
		  (funcall func expr))
	     expr))
	((and (memq (car expr) '(^ calcFunc-subscr))
	      (eq func 'math-float)
	      (= (length expr) 3)
	      (Math-integerp (nth 2 expr)))
	 (list (car expr)
	       (math-map-over-constants-rec (nth 1 expr))
	       (nth 2 expr)))
	(t (cons (car expr) (mapcar 'math-map-over-constants-rec (cdr expr)))))
)




