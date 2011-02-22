;; Calculator for GNU Emacs, part II [calc-yank.el]
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

(defun calc-Need-calc-yank () nil)


;;; Kill ring commands.

(defun calc-kill (nn &optional no-delete)
  (interactive "P")
  (if (eq major-mode 'calc-mode)
      (calc-wrapper
       (calc-force-refresh)
       (calc-set-command-flag 'no-align)
       (let ((num (max (calc-locate-cursor-element (point)) 1))
	     (n (prefix-numeric-value nn)))
	 (if (< n 0)
	     (progn
	       (if (eobp)
		   (setq num (1- num)))
	       (setq num (- num n)
		     n (- n))))
	 (let ((stuff (calc-top-list n (- num n -1))))
	   (calc-cursor-stack-index num)
	   (let ((first (point)))
	     (calc-cursor-stack-index (- num n))
	     (if (null nn)
		 (backward-char 1))   ; don't include newline for raw C-k
	     (copy-region-as-kill first (point))
	     (if (not no-delete)
		 (calc-pop-stack n (- num n -1))))
	   (setq calc-last-kill (cons (car kill-ring) stuff)))))
    (kill-line nn))
)

(defun calc-force-refresh ()
  (if (or calc-executing-macro calc-display-dirty)
      (let ((calc-executing-macro nil))
	(calc-refresh)))
)

(defun calc-locate-cursor-element (pt)
  (save-excursion
    (goto-char (point-max))
    (calc-locate-cursor-scan (- calc-stack-top) calc-stack pt))
)

(defun calc-locate-cursor-scan (n stack pt)
  (if (or (<= (point) pt)
	  (null stack))
      n
    (forward-line (- (nth 1 (car stack))))
    (calc-locate-cursor-scan (1+ n) (cdr stack) pt))
)

(defun calc-kill-region (top bot &optional no-delete)
  (interactive "r")
  (if (eq major-mode 'calc-mode)
      (calc-wrapper
       (calc-force-refresh)
       (calc-set-command-flag 'no-align)
       (let* ((top-num (calc-locate-cursor-element top))
	      (bot-num (calc-locate-cursor-element (1- bot)))
	      (num (- top-num bot-num -1)))
	 (copy-region-as-kill top bot)
	 (setq calc-last-kill (cons (car kill-ring)
				    (calc-top-list num bot-num)))
	 (if (not no-delete)
	     (calc-pop-stack num bot-num))))
    (if no-delete
	(copy-region-as-kill top bot)
      (kill-region top bot)))
)

(defun calc-copy-as-kill (n)
  (interactive "P")
  (calc-kill n t)
)

(defun calc-copy-region-as-kill (top bot)
  (interactive "r")
  (calc-kill-region top bot t)
)

;;; This function uses calc-last-kill if possible to get an exact result,
;;; otherwise it just parses the yanked string.
;;; Modified to use Emacs 19 extended concept of kill-ring. -- daveg 12/15/96
(defun calc-yank ()
  (interactive)
  (calc-wrapper
   (calc-pop-push-record-list
    0 "yank"
    (let ((thing (if (fboundp 'current-kill)
		     (current-kill 0 t)
		   (car kill-ring-yank-pointer))))
      (if (eq (car-safe calc-last-kill) thing)
	  (cdr calc-last-kill)
	(if (stringp thing)
	    (let ((val (math-read-exprs (calc-clean-newlines thing))))
	      (if (eq (car-safe val) 'error)
		  (progn
		    (setq val (math-read-exprs thing))
		    (if (eq (car-safe val) 'error)
			(error "Bad format in yanked data")
		      val))
		val)))))))
)

(defun calc-clean-newlines (s)
  (cond
   
   ;; Omit leading/trailing whitespace
   ((or (string-match "\\`[ \n\r]+\\([^\001]*\\)\\'" s)
	(string-match "\\`\\([^\001]*\\)[ \n\r]+\\'" s))
    (calc-clean-newlines (math-match-substring s 1)))

   ;; Convert newlines to commas
   ((string-match "\\`\\(.*\\)[\n\r]+\\([^\001]*\\)\\'" s)
    (calc-clean-newlines (concat (math-match-substring s 1) ","
				 (math-match-substring s 2))))
   
   (t s))
)


(defun calc-do-grab-region (top bot arg)
  (and (memq major-mode '(calc-mode calc-trail-mode))
       (error "This command works only in a regular text buffer."))
  (let* ((from-buffer (current-buffer))
	 (calc-was-started (get-buffer-window "*Calculator*"))
	 (single nil)
	 data vals pos)
    (if arg
	(if (consp arg)
	    (setq single t)
	  (setq arg (prefix-numeric-value arg))
	  (if (= arg 0)
	      (save-excursion
		(beginning-of-line)
		(setq top (point))
		(end-of-line)
		(setq bot (point)))
	    (save-excursion
	      (setq top (point))
	      (forward-line arg)
	      (if (> arg 0)
		  (setq bot (point))
		(setq bot top
		      top (point)))))))
    (setq data (buffer-substring top bot))
    (calc)
    (if single
	(setq vals (math-read-expr data))
      (setq vals (math-read-expr (concat "[" data "]")))
      (and (eq (car-safe vals) 'vec)
	   (= (length vals) 2)
	   (eq (car-safe (nth 1 vals)) 'vec)
	   (setq vals (nth 1 vals))))
    (if (eq (car-safe vals) 'error)
	(progn
	  (if calc-was-started
	      (pop-to-buffer from-buffer)
	    (calc-quit t)
	    (switch-to-buffer from-buffer))
	  (goto-char top)
	  (forward-char (+ (nth 1 vals) (if single 0 1)))
	  (error (nth 2 vals))))
    (calc-slow-wrapper
     (calc-enter-result 0 "grab" vals)))
)


(defun calc-do-grab-rectangle (top bot arg &optional reduce)
  (and (memq major-mode '(calc-mode calc-trail-mode))
       (error "This command works only in a regular text buffer."))
  (let* ((col1 (save-excursion (goto-char top) (current-column)))
	 (col2 (save-excursion (goto-char bot) (current-column)))
	 (from-buffer (current-buffer))
	 (calc-was-started (get-buffer-window "*Calculator*"))
	 data mat vals lnum pt pos)
    (if (= col1 col2)
	(save-excursion
	  (or (= col1 0)
	      (error "Point and mark must be at beginning of line, or define a rectangle"))
	  (goto-char top)
	  (while (< (point) bot)
	    (setq pt (point))
	    (forward-line 1)
	    (setq data (cons (buffer-substring pt (1- (point))) data)))
	  (setq data (nreverse data)))
      (setq data (extract-rectangle top bot)))
    (calc)
    (setq mat (list 'vec)
	  lnum 0)
    (and arg
	 (setq arg (if (consp arg) 0 (prefix-numeric-value arg))))
    (while data
      (if (natnump arg)
	  (progn
	    (if (= arg 0)
		(setq arg 1000000))
	    (setq pos 0
		  vals (list 'vec))
	    (let ((w (length (car data)))
		  j v)
	      (while (< pos w)
		(setq j (+ pos arg)
		      v (if (>= j w)
			    (math-read-expr (substring (car data) pos))
			  (math-read-expr (substring (car data) pos j))))
		(if (eq (car-safe v) 'error)
		    (setq vals v w 0)
		  (setq vals (nconc vals (list v))
			pos j)))))
	(if (string-match "\\` *-?[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]? *\\'"
			  (car data))
	    (setq vals (list 'vec (string-to-int (car data))))
	  (if (and (null arg)
		   (string-match "[[{][^][{}]*[]}]" (car data)))
	      (setq pos (match-beginning 0)
		    vals (math-read-expr (math-match-substring (car data) 0)))
	    (let ((s (if (string-match
			  "\\`\\([0-9]+:[ \t]\\)?\\(.*[^, \t]\\)[, \t]*\\'"
			  (car data))
			 (math-match-substring (car data) 2)
		       (car data))))
	      (setq pos -1
		    vals (math-read-expr (concat "[" s "]")))
	      (if (eq (car-safe vals) 'error)
		  (let ((v2 (math-read-expr s)))
		    (or (eq (car-safe v2) 'error)
			(setq vals (list 'vec v2)))))))))
      (if (eq (car-safe vals) 'error)
	  (progn
	    (if calc-was-started
		(pop-to-buffer from-buffer)
	      (calc-quit t)
	      (switch-to-buffer from-buffer))
	    (goto-char top)
	    (forward-line lnum)
	    (forward-char (+ (nth 1 vals) (min col1 col2) pos))
	    (error (nth 2 vals))))
      (or (equal vals '(vec))
	  (setq mat (cons vals mat)))
      (setq data (cdr data)
	    lnum (1+ lnum)))
    (calc-slow-wrapper
     (if reduce
	 (calc-enter-result 0 "grb+" (list reduce '(var add var-add)
					   (nreverse mat)))
       (calc-enter-result 0 "grab" (nreverse mat)))))
)


(defun calc-copy-to-buffer (nn)
  "Copy the top of stack into an editing buffer."
  (interactive "P")
  (let ((thebuf (and (not (memq major-mode '(calc-mode calc-trail-mode)))
		     (current-buffer)))
	(movept nil)
	oldbuf newbuf)
    (calc-wrapper
     (save-excursion
       (calc-force-refresh)
       (let ((n (prefix-numeric-value nn))
	     (eat-lnums calc-line-numbering)
	     (big-offset (if (eq calc-language 'big) 1 0))
	     top bot)
	 (setq oldbuf (current-buffer)
	       newbuf (or thebuf
			  (calc-find-writable-buffer (buffer-list) 0)
			  (calc-find-writable-buffer (buffer-list) 1)
			  (error "No other buffer")))
	 (cond ((and (or (null nn)
			 (consp nn))
		     (= (calc-substack-height 0)
			(- (1- (calc-substack-height 1)) big-offset)))
		(calc-cursor-stack-index 1)
		(if (looking-at
		     (if calc-line-numbering "[0-9]+: *[^ \n]" " *[^ \n]"))
		    (goto-char (1- (match-end 0))))
		(setq eat-lnums nil
		      top (point))
		(calc-cursor-stack-index 0)
		(setq bot (- (1- (point)) big-offset)))
	       ((> n 0)
		(calc-cursor-stack-index n)
		(setq top (point))
		(calc-cursor-stack-index 0)
		(setq bot (- (point) big-offset)))
	       ((< n 0)
		(calc-cursor-stack-index (- n))
		(setq top (point))
		(calc-cursor-stack-index (1- (- n)))
		(setq bot (point)))
	       (t
		(goto-char (point-min))
		(forward-line 1)
		(setq top (point))
		(calc-cursor-stack-index 0)
		(setq bot (point))))
	 (save-excursion
	   (set-buffer newbuf)
	   (if (consp nn)
	       (kill-region (region-beginning) (region-end)))
	   (push-mark (point) t)
	   (if (and overwrite-mode (not (consp nn)))
	       (calc-overwrite-string (save-excursion
					(set-buffer oldbuf)
					(buffer-substring top bot))
				      eat-lnums)
	     (or (bolp) (setq eat-lnums nil))
	     (insert-buffer-substring oldbuf top bot)
	     (and eat-lnums
		  (let ((n 1))
		    (while (and (> (point) (mark))
				(progn
				  (forward-line -1)
				  (>= (point) (mark))))
		      (delete-char 4)
		      (setq n (1+ n)))
		    (forward-line n))))
	   (if thebuf (setq movept (point)))
	   (if (get-buffer-window (current-buffer))
	       (set-window-point (get-buffer-window (current-buffer))
				 (point)))))))
    (if movept (goto-char movept))
    (and (consp nn)
	 (not thebuf)
	 (progn
	   (calc-quit t)
	   (switch-to-buffer newbuf))))
)

(defun calc-overwrite-string (str eat-lnums)
  (if (string-match "\n\\'" str)
      (setq str (substring str 0 -1)))
  (if eat-lnums
      (setq str (substring str 4)))
  (if (and (string-match "\\`[-+]?[0-9.]+\\(e-?[0-9]+\\)?\\'" str)
	   (looking-at "[-+]?[0-9.]+\\(e-?[0-9]+\\)?"))
      (progn
	(delete-region (point) (match-end 0))
	(insert str))
    (let ((i 0))
      (while (< i (length str))
	(if (= (setq last-command-char (aref str i)) ?\n)
	    (or (= i (1- (length str)))
		(let ((pt (point)))
		  (end-of-line)
		  (delete-region pt (point))
		  (if (eobp)
		      (insert "\n")
		    (forward-char 1))
		  (if eat-lnums (setq i (+ i 4)))))
	  (self-insert-command 1))
	(setq i (1+ i)))))
)

;;; First, require that buffer is visible and does not begin with "*"
;;; Second, require only that it not begin with "*Calc"
(defun calc-find-writable-buffer (buf mode)
  (and buf
       (if (or (string-match "\\`\\( .*\\|\\*Calc.*\\)"
			     (buffer-name (car buf)))
	       (and (= mode 0)
		    (or (string-match "\\`\\*.*" (buffer-name (car buf)))
			(not (get-buffer-window (car buf))))))
	   (calc-find-writable-buffer (cdr buf) mode)
	 (car buf)))
)


(defun calc-edit (n)
  (interactive "p")
  (calc-slow-wrapper
   (if (eq n 0)
       (setq n (calc-stack-size)))
   (let* ((flag nil)
	  (allow-ret (> n 1))
	  (list (math-showing-full-precision
		 (mapcar (if (> n 1)
			     (function (lambda (x)
					 (math-format-flat-expr x 0)))
			   (function
			    (lambda (x)
			      (if (math-vectorp x) (setq allow-ret t))
			      (math-format-nice-expr x (screen-width)))))
			 (if (> n 0)
			     (calc-top-list n)
			   (calc-top-list 1 (- n)))))))
     (calc-edit-mode (list 'calc-finish-stack-edit (or flag n)) allow-ret)
     (while list
       (insert (car list) "\n")
       (setq list (cdr list)))))
  (calc-show-edit-buffer)
)

(defun calc-alg-edit (str)
  (calc-edit-mode '(calc-finish-stack-edit 0))
  (calc-show-edit-buffer)
  (insert str "\n")
  (backward-char 1)
  (calc-set-command-flag 'do-edit)
)

(defvar calc-edit-mode-map nil "Keymap for use by the calc-edit command.")
(if calc-edit-mode-map
    ()
  (setq calc-edit-mode-map (make-sparse-keymap))
  (define-key calc-edit-mode-map "\n" 'calc-edit-finish)
  (define-key calc-edit-mode-map "\r" 'calc-edit-return)
  (define-key calc-edit-mode-map "\C-c\C-c" 'calc-edit-finish)
)

(defun calc-edit-mode (&optional handler allow-ret title)
  "Calculator editing mode.  Press RET, LFD, or C-c C-c to finish.
To cancel the edit, simply kill the *Calc Edit* buffer."
  (interactive)
  (or handler
      (error "This command can be used only indirectly through calc-edit."))
  (let ((oldbuf (current-buffer))
	(buf (get-buffer-create "*Calc Edit*")))
    (set-buffer buf)
    (kill-all-local-variables)
    (use-local-map calc-edit-mode-map)
    (setq buffer-read-only nil)
    (setq truncate-lines nil)
    (setq major-mode 'calc-edit-mode)
    (setq mode-name "Calc Edit")
    (run-hooks 'calc-edit-mode-hook)
    (make-local-variable 'calc-original-buffer)
    (setq calc-original-buffer oldbuf)
    (make-local-variable 'calc-return-buffer)
    (setq calc-return-buffer oldbuf)
    (make-local-variable 'calc-one-window)
    (setq calc-one-window (and (one-window-p t) pop-up-windows))
    (make-local-variable 'calc-edit-handler)
    (setq calc-edit-handler handler)
    (make-local-variable 'calc-restore-trail)
    (setq calc-restore-trail (get-buffer-window (calc-trail-buffer)))
    (make-local-variable 'calc-allow-ret)
    (setq calc-allow-ret allow-ret)
    (erase-buffer)
    (insert (or title title "Calc Edit Mode")
	    ".  Press "
	    (if (eq (lookup-key (current-global-map) "\e#") 'calc-dispatch)
		"M-# M-# or C-c C-c"
	      (if allow-ret "C-c C-c" "RET"))
	    " to finish, "
	    (if (eq (lookup-key (current-global-map) "\e#") 'calc-dispatch)
		"M-# x"
	      "C-x k RET")
	    " to cancel.\n"))
)
(put 'calc-edit-mode 'mode-class 'special)

(defun calc-show-edit-buffer ()
  (let ((buf (current-buffer)))
    (if (and (one-window-p t) pop-up-windows)
	(pop-to-buffer (get-buffer-create "*Calc Edit*"))
      (and calc-embedded-info (get-buffer-window (aref calc-embedded-info 1))
	   (select-window (get-buffer-window (aref calc-embedded-info 1))))
      (switch-to-buffer (get-buffer-create "*Calc Edit*")))
    (setq calc-return-buffer buf)
    (if (and (< (window-width) (screen-width))
	     calc-display-trail)
	(let ((win (get-buffer-window (calc-trail-buffer))))
	  (if win
	      (delete-window win))))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (forward-line 1))
)

(defun calc-edit-return ()
  (interactive)
  (if (and (boundp 'calc-allow-ret) calc-allow-ret)
      (newline)
    (calc-edit-finish))
)

(defun calc-edit-finish (&optional keep)
  "Finish calc-edit mode.  Parse buffer contents and push them on the stack."
  (interactive "P")
  (message "Working...")
  (or (and (boundp 'calc-original-buffer)
	   (boundp 'calc-return-buffer)
	   (boundp 'calc-one-window)
	   (boundp 'calc-edit-handler)
	   (boundp 'calc-restore-trail)
	   (eq major-mode 'calc-edit-mode))
      (error "This command is valid only in buffers created by calc-edit."))
  (let ((buf (current-buffer))
	(original calc-original-buffer)
	(return calc-return-buffer)
	(one-window calc-one-window)
	(disp-trail calc-restore-trail))
    (save-excursion
      (if (or (null (buffer-name original))
	      (progn
		(set-buffer original)
		(not (eq major-mode 'calc-mode))))
	  (error "Original calculator buffer has been corrupted.")))
    (goto-char (point-min))
    (if (looking-at "Calc Edit\\|Editing ")
	(forward-line 1))
    (if (buffer-modified-p)
	(eval calc-edit-handler))
    (if one-window
	(delete-window))
    (if (get-buffer-window return)
	(select-window (get-buffer-window return))
      (switch-to-buffer return))
    (if keep
	(bury-buffer buf)
      (kill-buffer buf))
    (if disp-trail
	(calc-wrapper
	 (calc-trail-display 1 t)))
    (message ""))
)

(defun calc-edit-cancel ()
  "Cancel calc-edit mode.  Ignore the Calc Edit buffer and don't change stack."
  (interactive)
  (let ((calc-edit-handler nil))
    (calc-edit-finish))
  (message "(Cancelled)")
)

(defun calc-finish-stack-edit (num)
  (let ((buf (current-buffer))
	(str (buffer-substring (point) (point-max)))
	(start (point))
	pos)
    (if (and (integerp num) (> num 1))
	(while (setq pos (string-match "\n." str))
	  (aset str pos ?\,)))
    (switch-to-buffer calc-original-buffer)
    (let ((vals (let ((calc-language nil)
		      (math-expr-opers math-standard-opers))
		  (and (string-match "[^\n\t ]" str)
		       (math-read-exprs str)))))
      (if (eq (car-safe vals) 'error)
	  (progn
	    (switch-to-buffer buf)
	    (goto-char (+ start (nth 1 vals)))
	    (error (nth 2 vals))))
      (calc-wrapper
       (if (symbolp num)
	   (progn
	     (set num (car vals))
	     (calc-refresh-evaltos num))
	 (if disp-trail
	     (calc-trail-display 1 t))
	 (and vals
	      (let ((calc-simplify-mode (if (eq last-command-char ?\C-j)
					    'none
					  calc-simplify-mode)))
		(if (>= num 0)
		    (calc-enter-result num "edit" vals)
		  (calc-enter-result 1 "edit" vals (- num)))))))))
)




