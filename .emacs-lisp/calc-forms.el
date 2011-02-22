;; Calculator for GNU Emacs, part II [calc-forms.el]
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

(defun calc-Need-calc-forms () nil)


(defun calc-time ()
  (interactive)
  (calc-wrapper
   (let ((time (current-time-string)))
     (calc-enter-result 0 "time"
			(list 'mod
			      (list 'hms
				    (string-to-int (substring time 11 13))
				    (string-to-int (substring time 14 16))
				    (string-to-int (substring time 17 19)))
			      (list 'hms 24 0 0)))))
)




(defun calc-to-hms (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-inverse)
       (if (eq calc-angle-mode 'rad)
	   (calc-unary-op ">rad" 'calcFunc-rad arg)
	 (calc-unary-op ">deg" 'calcFunc-deg arg))
     (calc-unary-op ">hms" 'calcFunc-hms arg)))
)

(defun calc-from-hms (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-to-hms arg)
)


(defun calc-hms-notation (fmt)
  (interactive "sHours-minutes-seconds format (hms, @ ' \", etc.): ")
  (calc-wrapper
   (if (string-match "\\`\\([^,; ]+\\)\\([,; ]*\\)\\([^,; ]\\)\\([,; ]*\\)\\([^,; ]\\)\\'" fmt)
       (progn
	 (calc-change-mode 'calc-hms-format
			   (concat "%s" (math-match-substring fmt 1)
				   (math-match-substring fmt 2)
				   "%s" (math-match-substring fmt 3)
				   (math-match-substring fmt 4)
				   "%s" (math-match-substring fmt 5))
			   t)
	 (setq-default calc-hms-format calc-hms-format))  ; for minibuffer
     (error "Bad hours-minutes-seconds format.")))
)

(defun calc-date-notation (fmt arg)
  (interactive "sDate format (e.g., M/D/YY h:mm:ss): \nP")
  (calc-wrapper
   (if (equal fmt "")
       (setq fmt "1"))
   (if (string-match "\\` *[0-9] *\\'" fmt)
       (setq fmt (nth (string-to-int fmt) calc-standard-date-formats)))
   (or (string-match "[a-zA-Z]" fmt)
       (error "Bad date format specifier"))
   (and arg
	(>= (setq arg (prefix-numeric-value arg)) 0)
	(<= arg 9)
	(setq calc-standard-date-formats
	      (copy-sequence calc-standard-date-formats))
	(setcar (nthcdr arg calc-standard-date-formats) fmt))
   (let ((case-fold-search nil))
     (and (not (string-match "<.*>" fmt))
	  (string-match "\\`[^hHspP]*\\([^ac-gi-lnoqrt-zAC-GI-OQRT-Z]*[bBhHmpPsS]+[^ac-gi-lnoqrt-zAC-GI-OQRT-Z]*\\)[^hHspP]*\\'" fmt)
	  (string-match (concat "[^ac-gi-lnoqrt-zAC-GI-OQRT-Z]*"
				(regexp-quote (math-match-substring fmt 1))
				"[^ac-gi-lnoqrt-zAC-GI-OQRT-Z]*") fmt)
	  (setq fmt (concat (substring fmt 0 (match-beginning 0))
			    "<"
			    (substring fmt (match-beginning 0) (match-end 0))
			    ">"
			    (substring fmt (match-end 0))))))
   (let ((lfmt nil)
	 (fullfmt nil)
	 (time nil)
	 pos pos2 sym temp)
     (let ((case-fold-search nil))
       (and (setq temp (string-match ":[BS]S" fmt))
	    (aset fmt temp ?C)))
     (while (setq pos (string-match "[<>a-zA-Z]" fmt))
       (if (> pos 0)
	   (setq lfmt (cons (substring fmt 0 pos) lfmt)))
       (setq pos2 (1+ pos))
       (cond ((= (aref fmt pos) ?\<)
	      (and time (error "Nested <'s not allowed"))
	      (and lfmt (setq fullfmt (nconc lfmt fullfmt)
			      lfmt nil))
	      (setq time t))
	     ((= (aref fmt pos) ?\>)
	      (or time (error "Misplaced > in format"))
	      (and lfmt (setq fullfmt (cons (nreverse lfmt) fullfmt)
			      lfmt nil))
	      (setq time nil))
	     (t
	      (if (string-match "\\`[^a-zA-Z]*[bB][a-zA-Z]" fmt)
		  (setq pos2 (1+ pos2)))
	      (while (and (< pos2 (length fmt))
			  (= (upcase (aref fmt pos2))
			     (upcase (aref fmt (1- pos2)))))
		(setq pos2 (1+ pos2)))
	      (setq sym (intern (substring fmt pos pos2)))
	      (or (memq sym '(Y YY BY YYY YYYY
				aa AA aaa AAA aaaa AAAA
				bb BB bbb BBB bbbb BBBB
				M MM BM mmm Mmm Mmmm MMM MMMM
				D DD BD d ddd bdd
				W www Www Wwww WWW WWWW
				h hh bh H HH BH
				p P pp PP pppp PPPP
				m mm bm s ss bss SS BS C
				N n J j U b))
		  (and (eq sym 'X) (not lfmt) (not fullfmt))
		  (error "Bad format code: %s" sym))
	      (and (memq sym '(bb BB bbb BBB bbbb BBBB))
		   (setq lfmt (cons 'b lfmt)))
	      (setq lfmt (cons sym lfmt))))
       (setq fmt (substring fmt pos2)))
     (or (equal fmt "")
	 (setq lfmt (cons fmt lfmt)))
     (and lfmt (if time
		   (setq fullfmt (cons (nreverse lfmt) fullfmt))
		 (setq fullfmt (nconc lfmt fullfmt))))
     (calc-change-mode 'calc-date-format (nreverse fullfmt) t)))
)


(defun calc-hms-mode ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-angle-mode 'hms)
   (message "Angles measured in degrees-minutes-seconds."))
)


(defun calc-now (arg)
  (interactive "P")
  (calc-date-zero-args "now" 'calcFunc-now arg)
)

(defun calc-date-part (arg)
  (interactive "NPart code (1-9 = Y,M,D,H,M,S,Wd,Yd,Hms): ")
  (if (or (< arg 1) (> arg 9))
      (error "Part code out of range"))
  (calc-wrapper
   (calc-enter-result 1
		      (nth arg '(nil "year" "mnth" "day" "hour" "minu"
				      "sec" "wday" "yday" "hmst"))
		      (list (nth arg '(nil calcFunc-year calcFunc-month
					   calcFunc-day calcFunc-hour
					   calcFunc-minute calcFunc-second
					   calcFunc-weekday calcFunc-yearday
					   calcFunc-time))
			    (calc-top-n 1))))
)

(defun calc-date (arg)
  (interactive "p")
  (if (or (< arg 1) (> arg 6))
      (error "Between one and six arguments are allowed"))
  (calc-wrapper
   (calc-enter-result arg "date" (cons 'calcFunc-date (calc-top-list-n arg))))
)

(defun calc-julian (arg)
  (interactive "P")
  (calc-date-one-arg "juln" 'calcFunc-julian arg)
)

(defun calc-unix-time (arg)
  (interactive "P")
  (calc-date-one-arg "unix" 'calcFunc-unixtime arg)
)

(defun calc-time-zone (arg)
  (interactive "P")
  (calc-date-zero-args "zone" 'calcFunc-tzone arg)
)

(defun calc-convert-time-zones (old &optional new)
  (interactive "sFrom time zone: ")
  (calc-wrapper
   (if (equal old "$")
       (calc-enter-result 3 "tzcv" (cons 'calcFunc-tzconv (calc-top-list-n 3)))
     (if (equal old "") (setq old "local"))
     (or new
	 (setq new (read-string (concat "From time zone: " old
					", to zone: "))))
     (if (stringp old) (setq old (math-read-expr old)))
     (if (eq (car-safe old) 'error)
	 (error "Error in expression: " (nth 1 old)))
     (if (equal new "") (setq new "local"))
     (if (stringp new) (setq new (math-read-expr new)))
     (if (eq (car-safe new) 'error)
	 (error "Error in expression: " (nth 1 new)))
     (calc-enter-result 1 "tzcv" (list 'calcFunc-tzconv
				       (calc-top-n 1) old new))))
)

(defun calc-new-week (arg)
  (interactive "P")
  (calc-date-one-arg "nwwk" 'calcFunc-newweek arg)
)

(defun calc-new-month (arg)
  (interactive "P")
  (calc-date-one-arg "nwmn" 'calcFunc-newmonth arg)
)

(defun calc-new-year (arg)
  (interactive "P")
  (calc-date-one-arg "nwyr" 'calcFunc-newyear arg)
)

(defun calc-inc-month (arg)
  (interactive "p")
  (calc-date-one-arg "incm" 'calcFunc-incmonth arg)
)

(defun calc-business-days-plus (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "bus+" 'calcFunc-badd arg))
)

(defun calc-business-days-minus (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "bus-" 'calcFunc-bsub arg))
)

(defun calc-date-zero-args (prefix func arg)
  (calc-wrapper
   (if (consp arg)
       (calc-enter-result 1 prefix (list func (calc-top-n 1)))
     (calc-enter-result 0 prefix (if arg
				     (list func (prefix-numeric-value arg))
				   (list func)))))
)

(defun calc-date-one-arg (prefix func arg)
  (calc-wrapper
   (if (consp arg)
       (calc-enter-result 2 prefix (cons func (calc-top-list-n 2)))
     (calc-enter-result 1 prefix (if arg
				     (list func (calc-top-n 1)
					   (prefix-numeric-value arg))
				   (list func (calc-top-n 1))))))
)








;;;; Hours-minutes-seconds forms.

(defun math-normalize-hms (a)
  (let ((h (math-normalize (nth 1 a)))
	(m (math-normalize (nth 2 a)))
	(s (let ((calc-internal-prec (max (- calc-internal-prec 4) 3)))
	     (math-normalize (nth 3 a)))))
    (if (math-negp h)
	(progn
	  (if (math-posp s)
	      (setq s (math-add s -60)
		    m (math-add m 1)))
	  (if (math-posp m)
	      (setq m (math-add m -60)
		    h (math-add h 1)))
	  (if (not (Math-lessp -60 s))
	      (setq s (math-add s 60)
		    m (math-add m -1)))
	  (if (not (Math-lessp -60 m))
	      (setq m (math-add m 60)
		    h (math-add h -1))))
      (if (math-negp s)
	  (setq s (math-add s 60)
		m (math-add m -1)))
      (if (math-negp m)
	  (setq m (math-add m 60)
		h (math-add h -1)))
      (if (not (Math-lessp s 60))
	  (setq s (math-add s -60)
		m (math-add m 1)))
      (if (not (Math-lessp m 60))
	  (setq m (math-add m -60)
		h (math-add h 1))))
    (if (and (eq (car-safe s) 'float)
	     (<= (+ (math-numdigs (nth 1 s)) (nth 2 s))
		 (- 2 calc-internal-prec)))
	(setq s 0))
    (list 'hms h m s))
)

;;; Convert A from ANG or current angular mode to HMS format.
(defun math-to-hms (a &optional ang)   ; [X R] [Public]
  (cond ((eq (car-safe a) 'hms) a)
	((eq (car-safe a) 'sdev)
	 (math-make-sdev (math-to-hms (nth 1 a))
			 (math-to-hms (nth 2 a))))
	((not (Math-numberp a))
	 (list 'calcFunc-hms a))
	((math-negp a)
	 (math-neg (math-to-hms (math-neg a) ang)))
	((eq (or ang calc-angle-mode) 'rad)
	 (math-to-hms (math-div a (math-pi-over-180)) 'deg))
	((memq (car-safe a) '(cplx polar)) a)
	(t
	 ;(setq a (let ((calc-internal-prec (max (1- calc-internal-prec) 3)))
	 ;	    (math-normalize a)))
	 (math-normalize
	  (let* ((b (math-mul a 3600))
		 (hm (math-trunc (math-div b 60)))
		 (hmd (math-idivmod hm 60)))
	    (list 'hms
		  (car hmd)
		  (cdr hmd)
		  (math-sub b (math-mul hm 60)))))))
)
(defun calcFunc-hms (h &optional m s)
  (or (Math-realp h) (math-reject-arg h 'realp))
  (or m (setq m 0))
  (or (Math-realp m) (math-reject-arg m 'realp))
  (or s (setq s 0))
  (or (Math-realp s) (math-reject-arg s 'realp))
  (if (and (not (Math-lessp m 0)) (Math-lessp m 60)
	   (not (Math-lessp s 0)) (Math-lessp s 60))
      (math-add (math-to-hms h)
		(list 'hms 0 m s))
    (math-to-hms (math-add h
			   (math-add (math-div (or m 0) 60)
				     (math-div (or s 0) 3600)))
		 'deg))
)

;;; Convert A from HMS format to ANG or current angular mode.
(defun math-from-hms (a &optional ang)   ; [R X] [Public]
  (cond ((not (eq (car-safe a) 'hms))
	 (if (Math-numberp a)
	     a
	   (if (eq (car-safe a) 'sdev)
	       (math-make-sdev (math-from-hms (nth 1 a) ang)
			       (math-from-hms (nth 2 a) ang))
	     (if (eq (or ang calc-angle-mode) 'rad)
		 (list 'calcFunc-rad a)
	       (list 'calcFunc-deg a)))))
	((math-negp a)
	 (math-neg (math-from-hms (math-neg a) ang)))
	((eq (or ang calc-angle-mode) 'rad)
	 (math-mul (math-from-hms a 'deg) (math-pi-over-180)))
	(t
	 (math-add (math-div (math-add (math-div (nth 3 a)
						 '(float 6 1))
				       (nth 2 a))
			     60)
		   (nth 1 a))))
)



;;;; Date forms.


;;; Some of these functions are adapted from Edward Reingold's "calendar.el".
;;; These versions are rewritten to use arbitrary-size integers.
;;; The Julian calendar is used up to 9/2/1752, after which the Gregorian
;;; calendar is used; the first day after 9/2/1752 is 9/14/1752.

;;; A numerical date is the number of days since midnight on
;;; the morning of January 1, 1 A.D.  If the date is a non-integer,
;;; it represents a specific date and time.
;;; A "dt" is a list of the form, (year month day), corresponding to
;;; an integer code, or (year month day hour minute second), corresponding
;;; to a non-integer code.

(defun math-date-to-dt (value)
  (if (eq (car-safe value) 'date)
      (setq value (nth 1 value)))
  (or (math-realp value)
      (math-reject-arg value 'datep))
  (let* ((parts (math-date-parts value))
	 (date (car parts))
	 (time (nth 1 parts))
	 (month 1)
	 day
	 (year (math-quotient (math-add date (if (Math-lessp date 711859)
						 365  ; for speed, we take
					       -108)) ; >1950 as a special case
			      (if (math-negp value) 366 365)))
					; this result may be an overestimate
	 temp)
    (while (Math-lessp date (setq temp (math-absolute-from-date year 1 1)))
      (setq year (math-add year -1)))
    (if (eq year 0) (setq year -1))
    (setq date (1+ (math-sub date temp)))
    (and (eq year 1752) (>= date 247)
	 (setq date (+ date 11)))
    (setq temp (if (math-leap-year-p year)
		   [1 32 61 92 122 153 183 214 245 275 306 336 999]
		 [1 32 60 91 121 152 182 213 244 274 305 335 999]))
    (while (>= date (aref temp month))
      (setq month (1+ month)))
    (setq day (1+ (- date (aref temp (1- month)))))
    (if (math-integerp value)
	(list year month day)
      (list year month day
	    (/ time 3600)
	    (% (/ time 60) 60)
	    (math-add (% time 60) (nth 2 parts)))))
)

(defun math-dt-to-date (dt)
  (or (integerp (nth 1 dt))
      (math-reject-arg (nth 1 dt) 'fixnump))
  (if (or (< (nth 1 dt) 1) (> (nth 1 dt) 12))
      (math-reject-arg (nth 1 dt) "Month value is out of range"))
  (or (integerp (nth 2 dt))
      (math-reject-arg (nth 2 dt) 'fixnump))
  (if (or (< (nth 2 dt) 1) (> (nth 2 dt) 31))
      (math-reject-arg (nth 2 dt) "Day value is out of range"))
  (let ((date (math-absolute-from-date (car dt) (nth 1 dt) (nth 2 dt))))
    (if (nth 3 dt)
	(math-add (math-float date)
		  (math-div (math-add (+ (* (nth 3 dt) 3600)
					 (* (nth 4 dt) 60))
				      (nth 5 dt))
			    '(float 864 2)))
      date))
)

(defun math-date-parts (value &optional offset)
  (let* ((date (math-floor value))
	 (time (math-round (math-mul (math-sub value (or offset date)) 86400)
			   (and (> calc-internal-prec 12)
				(- calc-internal-prec 12))))
	 (ftime (math-floor time)))
    (list date
	  ftime
	  (math-sub time ftime)))
)


(defun math-this-year ()
  (string-to-int (substring (current-time-string) -4))
)

(defun math-leap-year-p (year)
  (if (Math-lessp year 1752)
      (if (math-negp year)
	  (= (math-imod (math-neg year) 4) 1)
	(= (math-imod year 4) 0))
    (setq year (math-imod year 400))
    (or (and (= (% year 4) 0) (/= (% year 100) 0))
	(= year 0)))
)

(defun math-days-in-month (year month)
  (if (and (= month 2) (math-leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month)))
)

(defun math-day-number (year month day)
  (let ((day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
	(progn
	  (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
	  (if (math-leap-year-p year)
	      (setq day-of-year (1+ day-of-year)))))
    (and (eq year 1752)
	 (or (> month 9)
	     (and (= month 9) (>= day 14)))
	 (setq day-of-year (- day-of-year 11)))
    day-of-year)
)

(defun math-absolute-from-date (year month day)
  (if (eq year 0) (setq year -1))
  (let ((yearm1 (math-sub year 1)))
    (math-sub (math-add (math-day-number year month day)
			(math-add (math-mul 365 yearm1)
				  (if (math-posp year)
				      (math-quotient yearm1 4)
				    (math-sub 365
					      (math-quotient (math-sub 3 year)
							     4)))))
	      (if (or (Math-lessp year 1753)
		      (and (eq year 1752) (<= month 9)))
		  1
		(let ((correction (math-mul (math-quotient yearm1 100) 3)))
		  (let ((res (math-idivmod correction 4)))
		    (math-add (if (= (cdr res) 0)
				  -1
				0)
			      (car res)))))))
)


;;; It is safe to redefine these in your .emacs file to use a different
;;; language.

(defvar math-long-weekday-names '( "Sunday" "Monday" "Tuesday" "Wednesday"
				   "Thursday" "Friday" "Saturday" ))
(defvar math-short-weekday-names '( "Sun" "Mon" "Tue" "Wed"
				    "Thu" "Fri" "Sat" ))

(defvar math-long-month-names '( "January" "February" "March" "April"
				 "May" "June" "July" "August"
				 "September" "October" "November" "December" ))
(defvar math-short-month-names '( "Jan" "Feb" "Mar" "Apr" "May" "Jun"
				  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec" ))


(defun math-format-date (date)
  (if (eq (car-safe date) 'date)
      (setq date (nth 1 date)))
  (let ((entry (list date calc-internal-prec calc-date-format)))
    (or (cdr (assoc entry math-format-date-cache))
	(let* ((dt nil)
	       (calc-group-digits nil)
	       (calc-leading-zeros nil)
	       (calc-number-radix 10)
	       year month day weekday hour minute second
	       (bc-flag nil)
	       (fmt (apply 'concat (mapcar 'math-format-date-part
					   calc-date-format))))
	  (setq math-format-date-cache (cons (cons entry fmt)
					     math-format-date-cache))
	  (and (setq dt (nthcdr 10 math-format-date-cache))
	       (setcdr dt nil))
	  fmt)))
)
(setq math-format-date-cache nil)

(defun math-format-date-part (x)
  (cond ((stringp x)
	 x)
	((listp x)
	 (if (math-integerp date)
	     ""
	   (apply 'concat (mapcar 'math-format-date-part x))))
	((eq x 'X)
	 "")
	((eq x 'N)
	 (math-format-number date))
	((eq x 'n)
	 (math-format-number (math-floor date)))
	((eq x 'J)
	 (math-format-number (math-add date '(float (bigpos 235 214 17) -1))))
	((eq x 'j)
	 (math-format-number (math-add (math-floor date) '(bigpos 424 721 1))))
	((eq x 'U)
	 (math-format-number (nth 1 (math-date-parts date 719164))))
	((progn
	   (or dt
	       (progn
		 (setq dt (math-date-to-dt date)
		       year (car dt)
		       month (nth 1 dt)
		       day (nth 2 dt)
		       weekday (math-mod (math-add (math-floor date) 6) 7)
		       hour (nth 3 dt)
		       minute (nth 4 dt)
		       second (nth 5 dt))
		 (and (memq 'b calc-date-format)
		      (math-negp year)
		      (setq year (math-neg year)
			    bc-flag t))))
	   (memq x '(Y YY BY)))
	 (if (and (integerp year) (> year 1940) (< year 2040))
	     (format (cond ((eq x 'YY) "%02d")
			   ((eq x 'BYY) "%2d")
			   (t "%d"))
		     (% year 100))
	   (if (and (natnump year) (< year 100))
	       (format "+%d" year)
	     (math-format-number year))))
	((eq x 'YYY)
	 (math-format-number year))
	((eq x 'YYYY)
	 (if (and (natnump year) (< year 100))
	     (format "+%d" year)
	   (math-format-number year)))
	((eq x 'b) "")
	((eq x 'aa)
	 (and (not bc-flag) "ad"))
	((eq x 'AA)
	 (and (not bc-flag) "AD"))
	((eq x 'aaa)
	 (and (not bc-flag) "ad "))
	((eq x 'AAA)
	 (and (not bc-flag) "AD "))
	((eq x 'aaaa)
	 (and (not bc-flag) "a.d."))
	((eq x 'AAAA)
	 (and (not bc-flag) "A.D."))
	((eq x 'bb)
	 (and bc-flag "bc"))
	((eq x 'BB)
	 (and bc-flag "BC"))
	((eq x 'bbb)
	 (and bc-flag " bc"))
	((eq x 'BBB)
	 (and bc-flag " BC"))
	((eq x 'bbbb)
	 (and bc-flag "b.c."))
	((eq x 'BBBB)
	 (and bc-flag "B.C."))
	((eq x 'M)
	 (format "%d" month))
	((eq x 'MM)
	 (format "%02d" month))
	((eq x 'BM)
	 (format "%2d" month))
	((eq x 'mmm)
	 (downcase (nth (1- month) math-short-month-names)))
	((eq x 'Mmm)
	 (nth (1- month) math-short-month-names))
	((eq x 'MMM)
	 (upcase (nth (1- month) math-short-month-names)))
	((eq x 'Mmmm)
	 (nth (1- month) math-long-month-names))
	((eq x 'MMMM)
	 (upcase (nth (1- month) math-long-month-names)))
	((eq x 'D)
	 (format "%d" day))
	((eq x 'DD)
	 (format "%02d" day))
	((eq x 'BD)
	 (format "%2d" day))
	((eq x 'W)
	 (format "%d" weekday))
	((eq x 'www)
	 (downcase (nth weekday math-short-weekday-names)))
	((eq x 'Www)
	 (nth weekday math-short-weekday-names))
	((eq x 'WWW)
	 (upcase (nth weekday math-short-weekday-names)))
	((eq x 'Wwww)
	 (nth weekday math-long-weekday-names))
	((eq x 'WWWW)
	 (upcase (nth weekday math-long-weekday-names)))
	((eq x 'd)
	 (format "%d" (math-day-number year month day)))
	((eq x 'ddd)
	 (format "%03d" (math-day-number year month day)))
	((eq x 'bdd)
	 (format "%3d" (math-day-number year month day)))
	((eq x 'h)
	 (and hour (format "%d" hour)))
	((eq x 'hh)
	 (and hour (format "%02d" hour)))
	((eq x 'bh)
	 (and hour (format "%2d" hour)))
	((eq x 'H)
	 (and hour (format "%d" (1+ (% (+ hour 11) 12)))))
	((eq x 'HH)
	 (and hour (format "%02d" (1+ (% (+ hour 11) 12)))))
	((eq x 'BH)
	 (and hour (format "%2d" (1+ (% (+ hour 11) 12)))))
	((eq x 'p)
	 (and hour (if (< hour 12) "a" "p")))
	((eq x 'P)
	 (and hour (if (< hour 12) "A" "P")))
	((eq x 'pp)
	 (and hour (if (< hour 12) "am" "pm")))
	((eq x 'PP)
	 (and hour (if (< hour 12) "AM" "PM")))
	((eq x 'pppp)
	 (and hour (if (< hour 12) "a.m." "p.m.")))
	((eq x 'PPPP)
	 (and hour (if (< hour 12) "A.M." "P.M.")))
	((eq x 'm)
	 (and minute (format "%d" minute)))
	((eq x 'mm)
	 (and minute (format "%02d" minute)))
	((eq x 'bm)
	 (and minute (format "%2d" minute)))
	((eq x 'C)
	 (and second (not (math-zerop second))
	      ":"))
	((memq x '(s ss bs SS BS))
	 (and second
	      (not (and (memq x '(SS BS)) (math-zerop second)))
	      (if (integerp second)
		  (format (cond ((memq x '(ss SS)) "%02d")
				((memq x '(bs BS)) "%2d")
				(t "%d"))
			  second)
		(concat (if (Math-lessp second 10)
			    (cond ((memq x '(ss SS)) "0")
				  ((memq x '(bs BS)) " ")
				  (t ""))
			  "")
			(let ((calc-float-format
			       (list 'fix (min (- 12 calc-internal-prec)
					       0))))
			  (math-format-number second)))))))
)


(defun math-parse-date (str)
  (catch 'syntax
    (or (math-parse-standard-date str t)
	(math-parse-standard-date str nil)
	(and (string-match "\\`[^-+/0-9a-zA-Z]*\\([-+]?[0-9]+\\.?[0-9]*\\([eE][-+]?[0-9]+\\)?\\)[^-+/0-9a-zA-Z]*\\'" str)
	     (list 'date (math-read-number (math-match-substring str 1))))
	(let ((case-fold-search t)
	      (year nil) (month nil) (day nil) (weekday nil)
	      (hour nil) (minute nil) (second nil) (bc-flag nil)
	      (a nil) (b nil) (c nil) (bigyear nil) temp)

	  ;; Extract the time, if any.
	  (if (or (string-match "\\([0-9][0-9]?\\):\\([0-9][0-9]?\\)\\(:\\([0-9][0-9]?\\(\\.[0-9]+\\)?\\)\\)? *\\([ap]m?\\>\\|[ap]\\. *m\\.\\|noon\\|n\\>\\|midnight\\|mid\\>\\|m\\>\\)?" str)
		  (string-match "\\([0-9][0-9]?\\)\\(\\)\\(\\(\\(\\)\\)\\) *\\([ap]m?\\>\\|[ap]\\. *m\\.\\|noon\\|n\\>\\|midnight\\|mid\\>\\|m\\>\\)" str))
	      (let ((ampm (math-match-substring str 6)))
		(setq hour (string-to-int (math-match-substring str 1))
		      minute (math-match-substring str 2)
		      second (math-match-substring str 4)
		      str (concat (substring str 0 (match-beginning 0))
				  (substring str (match-end 0))))
		(if (equal minute "")
		    (setq minute 0)
		  (setq minute (string-to-int minute)))
		(if (equal second "")
		    (setq second 0)
		  (setq second (math-read-number second)))
		(if (equal ampm "")
		    (if (> hour 23)
			(throw 'syntax "Hour value out of range"))
		  (setq ampm (upcase (aref ampm 0)))
		  (if (memq ampm '(?N ?M))
		      (if (and (= hour 12) (= minute 0) (eq second 0))
			  (if (eq ampm ?M) (setq hour 0))
			(throw 'syntax
			       "Time must be 12:00:00 in this context"))
		    (if (or (= hour 0) (> hour 12))
			(throw 'syntax "Hour value out of range"))
		    (if (eq (= ampm ?A) (= hour 12))
			(setq hour (% (+ hour 12) 24)))))))

	  ;; Rewrite xx-yy-zz to xx/yy/zz to avoid seeing "-" as a minus sign.
	  (while (string-match "[0-9a-zA-Z]\\(-\\)[0-9a-zA-Z]" str)
	    (progn
	      (setq str (copy-sequence str))
	      (aset str (match-beginning 1) ?\/)))

	  ;; Extract obvious month or weekday names.
	  (if (string-match "[a-zA-Z]" str)
	      (progn
		(setq month (math-parse-date-word math-long-month-names))
		(setq weekday (math-parse-date-word math-long-weekday-names))
		(or month (setq month
				(math-parse-date-word math-short-month-names)))
		(or weekday (math-parse-date-word math-short-weekday-names))
		(or hour
		    (if (setq temp (math-parse-date-word
				    '( "noon" "midnight" "mid" )))
			(setq hour (if (= temp 1) 12 0) minute 0 second 0)))
		(or (math-parse-date-word '( "ad" "a.d." ))
		    (if (math-parse-date-word '( "bc" "b.c." ))
			(setq bc-flag t)))
		(if (string-match "[a-zA-Z]+" str)
		    (throw 'syntax (format "Bad word in date: \"%s\""
					   (math-match-substring str 0))))))

	  ;; If there is a huge number other than the year, ignore it.
	  (while (and (string-match "[-+]?0*[1-9][0-9][0-9][0-9][0-9]+" str)
		      (setq temp (concat (substring str 0 (match-beginning 0))
					 (substring str (match-end 0))))
		      (string-match "[4-9][0-9]\\|[0-9][0-9][0-9]\\|[-+][0-9]+[^-]*\\'" temp))
	    (setq str temp))

	  ;; If there is a number with a sign or a large number, it is a year.
	  (if (or (string-match "\\([-+][0-9]+\\)[^-]*\\'" str)
		  (string-match "\\(0*[1-9][0-9][0-9]+\\)" str))
	      (setq year (math-match-substring str 1)
		    str (concat (substring str 0 (match-beginning 1))
				(substring str (match-end 1)))
		    year (math-read-number year)
		    bigyear t))

	  ;; Collect remaining numbers.
	  (setq temp 0)
	  (while (string-match "[0-9]+" str temp)
	    (and c (throw 'syntax "Too many numbers in date"))
	    (setq c (string-to-int (math-match-substring str 0)))
	    (or b (setq b c c nil))
	    (or a (setq a b b nil))
	    (setq temp (match-end 0)))

	  ;; Check that we have the right amount of information.
	  (setq temp (+ (if year 1 0) (if month 1 0) (if day 1 0)
			(if a 1 0) (if b 1 0) (if c 1 0)))
	  (if (> temp 3)
	      (throw 'syntax "Too many numbers in date")
	    (if (or (< temp 2) (and year (= temp 2)))
		(throw 'syntax "Not enough numbers in date")
	      (if (= temp 2)   ; if year omitted, assume current year
		  (setq year (math-this-year)))))

	  ;; A large number must be a year.
	  (or year
	      (if (and a (or (> a 31) (< a 1)))
		  (setq year a a b b c c nil)
		(if (and b (or (> b 31) (< b 1)))
		    (setq year b b c c nil)
		  (if (and c (or (> c 31) (< c 1)))
		      (setq year c c nil)))))

	  ;; A medium-large number must be a day.
	  (if year
	      (if (and a (> a 12))
		  (setq day a a b b c c nil)
		(if (and b (> b 12))
		    (setq day b b c c nil)
		  (if (and c (> c 12))
		      (setq day c c nil)))))

	  ;; We may know enough to sort it out now.
	  (if (and year day)
	      (or month (setq month a))
	    (if (and year month)
		(setq day a)

	      ;; Interpret order of numbers as same as for display format.
	      (setq temp calc-date-format)
	      (while temp
		(cond ((not (symbolp (car temp))))
		      ((memq (car temp) '(Y YY BY YYY YYYY))
		       (or year (setq year a a b b c)))
		      ((memq (car temp) '(M MM BM mmm Mmm Mmmm MMM MMMM))
		       (or month (setq month a a b b c)))
		      ((memq (car temp) '(D DD BD))
		       (or day (setq day a a b b c))))
		(setq temp (cdr temp)))

	      ;; If display format was not complete, assume American style.
	      (or month (setq month a a b b c))
	      (or day (setq day a a b b c))
	      (or year (setq year a a b b c))))

	  (if bc-flag
	      (setq year (math-neg (math-abs year))))

	  (math-parse-date-validate year bigyear month day
				    hour minute second))))
)

(defun math-parse-date-validate (year bigyear month day hour minute second)
  (and (not bigyear) (natnump year) (< year 100)
       (setq year (+ year (if (< year 40) 2000 1900))))
  (if (eq year 0)
      (throw 'syntax "Year value is out of range"))
  (if (or (< month 1) (> month 12))
      (throw 'syntax "Month value is out of range"))
  (if (or (< day 1) (> day (math-days-in-month year month)))
      (throw 'syntax "Day value is out of range"))
  (and hour
       (progn
	 (if (or (< hour 0) (> hour 23))
	     (throw 'syntax "Hour value is out of range"))
	 (if (or (< minute 0) (> minute 59))
	     (throw 'syntax "Minute value is out of range"))
	 (if (or (math-negp second) (not (Math-lessp second 60)))
	     (throw 'syntax "Seconds value is out of range"))))
  (list 'date (math-dt-to-date (append (list year month day)
				       (and hour (list hour minute second)))))
)

(defun math-parse-date-word (names &optional front)
  (let ((n 1))
    (while (and names (not (string-match (if (equal (car names) "Sep")
					     "Sept?"
					   (regexp-quote (car names)))
					 str)))
      (setq names (cdr names)
	    n (1+ n)))
    (and names
	 (or (not front) (= (match-beginning 0) 0))
	 (progn
	   (setq str (concat (substring str 0 (match-beginning 0))
			     (if front "" " ")
			     (substring str (match-end 0))))
	   n)))
)

(defun math-parse-standard-date (str with-time)
  (let ((case-fold-search t)
	(okay t) num
	(fmt calc-date-format) this next (gnext nil)
	(year nil) (month nil) (day nil) (bigyear nil) (yearday nil)
	(hour nil) (minute nil) (second nil) (bc-flag nil))
    (while (and fmt okay)
      (setq this (car fmt)
	    fmt (setq fmt (or (cdr fmt)
				(prog1
				    gnext
				  (setq gnext nil))))
	    next (car fmt))
      (if (consp next) (setq next (car next)))
      (or (cond ((listp this)
		 (or (not with-time)
		     (not this)
		     (setq gnext fmt
			   fmt this)))
		((stringp this)
		 (if (and (<= (length this) (length str))
			  (equal this
				 (substring str 0 (length this))))
		     (setq str (substring str (length this)))))
		((eq this 'X)
		 t)
		((memq this '(n N j J))
		 (and (string-match "\\`[-+]?[0-9.]+\\([eE][-+]?[0-9]+\\)?" str)
		      (setq num (math-match-substring str 0)
			    str (substring str (match-end 0))
			    num (math-date-to-dt (math-read-number num))
			    num (math-sub num
					  (if (memq this '(n N))
					      0
					    (if (or (eq this 'j)
						    (math-integerp num))
						'(bigpos 424 721 1)
					      '(float (bigpos 235 214 17)
						      -1))))
			    hour (or (nth 3 num) hour)
			    minute (or (nth 4 num) minute)
			    second (or (nth 5 num) second)
			    year (car num)
			    month (nth 1 num)
			    day (nth 2 num))))
		((eq this 'U)
		 (and (string-match "\\`[-+]?[0-9]+" str)
		      (setq num (math-match-substring str 0)
			    str (substring str (match-end 0))
			    num (math-date-to-dt
				 (math-add 719164
					   (math-div (math-read-number num)
						     '(float 864 2))))
			    hour (nth 3 num)
			    minute (nth 4 num)
			    second (nth 5 num)
			    year (car num)
			    month (nth 1 num)
			    day (nth 2 num))))
		((memq this '(mmm Mmm MMM))
		 (setq month (math-parse-date-word math-short-month-names t)))
		((memq this '(Mmmm MMMM))
		 (setq month (math-parse-date-word math-long-month-names t)))
		((memq this '(www Www WWW))
		 (math-parse-date-word math-short-weekday-names t))
		((memq this '(Wwww WWWW))
		 (math-parse-date-word math-long-weekday-names t))
		((memq this '(p P))
		 (if (string-match "\\`a" str)
		     (setq hour (if (= hour 12) 0 hour)
			   str (substring str 1))
		   (if (string-match "\\`p" str)
		       (setq hour (if (= hour 12) 12 (% (+ hour 12) 24))
			     str (substring str 1)))))
		((memq this '(pp PP pppp PPPP))
		 (if (string-match "\\`am\\|a\\.m\\." str)
		     (setq hour (if (= hour 12) 0 hour)
			   str (substring str (match-end 0)))
		   (if (string-match "\\`pm\\|p\\.m\\." str)
		       (setq hour (if (= hour 12) 12 (% (+ hour 12) 24))
			     str (substring str (match-end 0))))))
		((memq this '(Y YY BY YYY YYYY))
		 (and (if (memq next '(MM DD ddd hh HH mm ss SS))
			  (if (memq this '(Y YY BYY))
			      (string-match "\\` *[0-9][0-9]" str)
			    (string-match "\\`[0-9][0-9][0-9][0-9]" str))
			(string-match "\\`[-+]?[0-9]+" str))
		      (setq year (math-match-substring str 0)
			    bigyear (or (eq this 'YYY)
					(memq (aref str 0) '(?\+ ?\-)))
			    str (substring str (match-end 0))
			    year (math-read-number year))))
		((eq this 'b)
		 t)
		((memq this '(aa AA aaaa AAAA))
		 (if (string-match "\\` *\\(ad\\|a\\.d\\.\\)" str)
		     (setq str (substring str (match-end 0)))))
		((memq this '(aaa AAA))
		 (if (string-match "\\` *ad *" str)
		     (setq str (substring str (match-end 0)))))
		((memq this '(bb BB bbb BBB bbbb BBBB))
		 (if (string-match "\\` *\\(bc\\|b\\.c\\.\\)" str)
		     (setq str (substring str (match-end 0))
			   bc-flag t)))
		((memq this '(s ss bs SS BS))
		 (and (if (memq next '(YY YYYY MM DD hh HH mm))
			  (string-match "\\` *[0-9][0-9]\\(\\.[0-9]+\\)?" str)
			(string-match "\\` *[0-9][0-9]?\\(\\.[0-9]+\\)?" str))
		      (setq second (math-match-substring str 0)
			    str (substring str (match-end 0))
			    second (math-read-number second))))
		((eq this 'C)
		 (if (string-match "\\`:[0-9][0-9]" str)
		     (setq str (substring str 1))
		   t))
		((or (not (if (and (memq this '(ddd MM DD hh HH mm))
				   (memq next '(YY YYYY MM DD ddd
						   hh HH mm ss SS)))
			      (if (eq this 'ddd)
				  (string-match "\\` *[0-9][0-9][0-9]" str)
				(string-match "\\` *[0-9][0-9]" str))
			    (string-match "\\` *[0-9]+" str)))
		     (and (setq num (string-to-int
				     (math-match-substring str 0))
				str (substring str (match-end 0)))
			  nil))
		 nil)
		((eq this 'W)
		 (and (>= num 0) (< num 7)))
		((memq this '(d ddd bdd))
		 (setq yearday num))
		((memq this '(M MM BM))
		 (setq month num))
		((memq this '(D DD BD))
		 (setq day num))
		((memq this '(h hh bh H HH BH))
		 (setq hour num))
		((memq this '(m mm bm))
		 (setq minute num)))
	  (setq okay nil)))
    (if yearday
	(if (and month day)
	    (setq yearday nil)
	  (setq month 1 day 1)))
    (if (and okay (equal str ""))
	(and month day (or (not (or hour minute second))
			   (and hour minute))
	     (progn
	       (or year (setq year (math-this-year)))
	       (or second (setq second 0))
	       (if bc-flag
		   (setq year (math-neg (math-abs year))))
	       (setq day (math-parse-date-validate year bigyear month day
						   hour minute second))
	       (if yearday
		   (setq day (math-add day (1- yearday))))
	       day))))
)


(defun calcFunc-now (&optional zone)
  (let ((date (let ((calc-date-format nil))
		(math-parse-date (current-time-string)))))
    (if (consp date)
	(if zone
	    (math-add date (math-div (math-sub (calcFunc-tzone nil date)
					       (calcFunc-tzone zone date))
				     '(float 864 2)))
	  date)
      (calc-record-why "*Unable to interpret current date from system")
      (append (list 'calcFunc-now) (and zone (list zone)))))
)

(defun calcFunc-year (date)
  (car (math-date-to-dt date))
)

(defun calcFunc-month (date)
  (nth 1 (math-date-to-dt date))
)

(defun calcFunc-day (date)
  (nth 2 (math-date-to-dt date))
)

(defun calcFunc-weekday (date)
  (if (eq (car-safe date) 'date)
      (setq date (nth 1 date)))
  (or (math-realp date)
      (math-reject-arg date 'datep))
  (math-mod (math-add (math-floor date) 6) 7)
)

(defun calcFunc-yearday (date)
  (let ((dt (math-date-to-dt date)))
    (math-day-number (car dt) (nth 1 dt) (nth 2 dt)))
)

(defun calcFunc-hour (date)
  (if (eq (car-safe date) 'hms)
      (nth 1 date)
    (or (nth 3 (math-date-to-dt date)) 0))
)

(defun calcFunc-minute (date)
  (if (eq (car-safe date) 'hms)
      (nth 2 date)
    (or (nth 4 (math-date-to-dt date)) 0))
)

(defun calcFunc-second (date)
  (if (eq (car-safe date) 'hms)
      (nth 3 date)
    (or (nth 5 (math-date-to-dt date)) 0))
)

(defun calcFunc-time (date)
  (let ((dt (math-date-to-dt date)))
    (if (nth 3 dt)
	(cons 'hms (nthcdr 3 dt))
      (list 'hms 0 0 0)))
)

(defun calcFunc-date (date &optional month day hour minute second)
  (and (math-messy-integerp month) (setq month (math-trunc month)))
  (and month (not (integerp month)) (math-reject-arg month 'fixnump))
  (and (math-messy-integerp day) (setq day (math-trunc day)))
  (and day (not (integerp day)) (math-reject-arg day 'fixnump))
  (if (and (eq (car-safe hour) 'hms) (not minute))
      (setq second (nth 3 hour)
	    minute (nth 2 hour)
	    hour (nth 1 hour)))
  (and (math-messy-integerp hour) (setq hour (math-trunc hour)))
  (and hour (not (integerp hour)) (math-reject-arg hour 'fixnump))
  (and (math-messy-integerp minute) (setq minute (math-trunc minute)))
  (and minute (not (integerp minute)) (math-reject-arg minute 'fixnump))
  (and (math-messy-integerp second) (setq second (math-trunc second)))
  (and second (not (math-realp second)) (math-reject-arg second 'realp))
  (if month
      (progn
	(and (math-messy-integerp date) (setq date (math-trunc date)))
	(and date (not (math-integerp date)) (math-reject-arg date 'integerp))
	(if day
	    (if hour
		(list 'date (math-dt-to-date (list date month day hour
						   (or minute 0)
						   (or second 0))))
	      (list 'date (math-dt-to-date (list date month day))))
	  (list 'date (math-dt-to-date (list (math-this-year) date month)))))
    (if (math-realp date)
	(list 'date date)
      (if (eq (car date) 'date)
	  (nth 1 date)
	(math-reject-arg date 'datep))))
)

(defun calcFunc-julian (date &optional zone)
  (if (math-realp date)
      (list 'date (if (math-integerp date)
		      (math-sub date '(bigpos 424 721 1))
		    (setq date (math-sub date '(float (bigpos 235 214 17) -1)))
		    (math-sub date (math-div (calcFunc-tzone zone date)
					     '(float 864 2)))))
    (if (eq (car date) 'date)
	(math-add (nth 1 date) (if (math-integerp (nth 1 date))
				   '(bigpos 424 721 1)
				 (math-add '(float (bigpos 235 214 17) -1)
					   (math-div (calcFunc-tzone zone date)
						     '(float 864 2)))))
      (math-reject-arg date 'datep)))
)

(defun calcFunc-unixtime (date &optional zone)
  (if (math-realp date)
      (progn
	(setq date (math-add 719164 (math-div date '(float 864 2))))
	(list 'date (math-sub date (math-div (calcFunc-tzone zone date)
					     '(float 864 2)))))
    (if (eq (car date) 'date)
	(math-add (nth 1 (math-date-parts (nth 1 date) 719164))
		  (calcFunc-tzone zone date))
      (math-reject-arg date 'datep)))
)

(defun calcFunc-tzone (&optional zone date)
  (if zone
      (cond ((math-realp zone)
	     (math-round (math-mul zone 3600)))
	    ((eq (car zone) 'hms)
	     (math-round (math-mul (math-from-hms zone 'deg) 3600)))
	    ((eq (car zone) '+)
	     (math-add (calcFunc-tzone (nth 1 zone) date)
		       (calcFunc-tzone (nth 2 zone) date)))
	    ((eq (car zone) '-)
	     (math-sub (calcFunc-tzone (nth 1 zone) date)
		       (calcFunc-tzone (nth 2 zone) date)))
	    ((eq (car zone) 'var)
	     (let ((name (upcase (symbol-name (nth 1 zone))))
		   found)
	       (if (setq found (assoc name math-tzone-names))
		   (calcFunc-tzone (math-add (nth 1 found)
					     (if (integerp (nth 2 found))
						 (nth 2 found)
					       (or
						(math-daylight-savings-adjust
						 date (car found))
						0)))
				   date)
		 (if (equal name "LOCAL")
		     (calcFunc-tzone nil date)
		   (math-reject-arg zone "*Unrecognized time zone name")))))
	    (t (math-reject-arg zone "*Expected a time zone")))
    (if (calc-var-value 'var-TimeZone)
	(calcFunc-tzone (calc-var-value 'var-TimeZone) date)
      (let ((p math-tzone-names)
	    (offset 0)
	    (tz '(var error var-error)))
	(save-excursion
	  (set-buffer (get-buffer-create " *Calc Temporary*"))
	  (erase-buffer)
	  (call-process "date" nil t)
	  (goto-char 1)
	  (let ((case-fold-search t))
	    (while (and p (not (search-forward (car (car p)) nil t)))
	      (setq p (cdr p))))
	  (if (looking-at "\\([-+][0-9]?[0-9]\\)\\([0-9][0-9]\\)?\\(\\'\\|[^0-9]\\)")
	      (setq offset (math-add
			    (string-to-int (buffer-substring
					    (match-beginning 1)
					    (match-end 1)))
			    (if (match-beginning 2)
				(math-div (string-to-int (buffer-substring
							  (match-beginning 2)
							  (match-end 2)))
					  60)
			      0)))))
	(if p
	    (progn
	      (setq p (car p))
	      ;; Try to convert to a generalized time zone.
	      (if (integerp (nth 2 p))
		  (let ((gen math-tzone-names))
		    (while (and gen
				(not (equal (nth 2 (car gen)) (car p)))
				(not (equal (nth 3 (car gen)) (car p)))
				(not (equal (nth 4 (car gen)) (car p)))
				(not (equal (nth 5 (car gen)) (car p))))
		      (setq gen (cdr gen)))
		    (and gen
			 (setq gen (car gen))
			 (equal (math-daylight-savings-adjust nil (car gen))
				(nth 2 p))
			 (setq p gen))))
	      (setq tz (math-add (list 'var
				       (intern (car p))
				       (intern (concat "var-" (car p))))
				 offset))))
	(kill-buffer " *Calc Temporary*")
	(setq var-TimeZone tz)
	(calc-refresh-evaltos 'var-TimeZone)
	(calcFunc-tzone tz date))))
)

;;; Note: Longer names must appear before shorter names which are
;;;       substrings of them.
(defvar math-tzone-names
  '( ( "MEGT" -1 "MET" "METDST" )                          ; Middle Europe
     ( "METDST" -1 -1 ) ( "MET" -1 0 )
     ( "MEGZ" -1 "MEZ" "MESZ" ) ( "MEZ" -1 0 ) ( "MESZ" -1 -1 )
     ( "WEGT" 0 "WET" "WETDST" )                           ; Western Europe
     ( "WETDST" 0 -1 ) ( "WET" 0 0 )
     ( "BGT" 0 "GMT" "BST" ) ( "GMT" 0 0 ) ( "BST" 0 -1 )  ; Britain
     ( "NGT" (float 35 -1) "NST" "NDT" )                   ; Newfoundland
     ( "NST" (float 35 -1) 0 ) ( "NDT" (float 35 -1) -1 )
     ( "AGT" 4 "AST" "ADT" ) ( "AST" 4 0 ) ( "ADT" 4 -1 )  ; Atlantic
     ( "EGT" 5 "EST" "EDT" ) ( "EST" 5 0 ) ( "EDT" 5 -1 )  ; Eastern
     ( "CGT" 6 "CST" "CDT" ) ( "CST" 6 0 ) ( "CDT" 6 -1 )  ; Central
     ( "MGT" 7 "MST" "MDT" ) ( "MST" 7 0 ) ( "MDT" 7 -1 )  ; Mountain
     ( "PGT" 8 "PST" "PDT" ) ( "PST" 8 0 ) ( "PDT" 8 -1 )  ; Pacific
     ( "YGT" 9 "YST" "YDT" ) ( "YST" 9 0 ) ( "YDT" 9 -1 )  ; Yukon
))


(defun math-daylight-savings-adjust (date zone &optional dt)
  (or date (setq date (nth 1 (calcFunc-now))))
  (let (bump)
    (if (eq (car-safe date) 'date)
	(setq bump 0
	      date (nth 1 date))
      (if (and date (math-realp date))
	  (let ((zadj (assoc zone math-tzone-names)))
	    (if zadj (setq bump -1
			   date (math-sub date (math-div (nth 1 zadj)
							 '(float 24 0))))))
	(math-reject-arg date 'datep)))
    (setq date (math-float date))
    (or dt (setq dt (math-date-to-dt date)))
    (and math-daylight-savings-hook
	 (funcall math-daylight-savings-hook date dt zone bump)))
)

(defun calcFunc-dsadj (date &optional zone)
  (if zone
      (or (eq (car-safe zone) 'var)
	  (math-reject-arg zone "*Time zone variable expected"))
    (setq zone (or (calc-var-value 'var-TimeZone)
		   (progn
		     (calcFunc-tzone)
		     (calc-var-value 'var-TimeZone)))))
  (setq zone (and (eq (car-safe zone) 'var)
		  (upcase (symbol-name (nth 1 zone)))))
  (let ((zadj (assoc zone math-tzone-names)))
    (or zadj (math-reject-arg zone "*Unrecognized time zone name"))
    (if (integerp (nth 2 zadj))
	(nth 2 zadj)
      (math-daylight-savings-adjust date zone)))
)

(defun calcFunc-tzconv (date z1 z2)
  (if (math-realp date)
      (nth 1 (calcFunc-tzconv (list 'date date) z1 z2))
    (calcFunc-unixtime (calcFunc-unixtime date z1) z2))
)

(defvar math-daylight-savings-hook 'math-std-daylight-savings)

(defun math-std-daylight-savings (date dt zone bump)
  "Standard North American daylight savings algorithm.
This implements the rules for the U.S. and Canada as of 1987.
Daylight savings begins on the first Sunday of April at 2 a.m.,
and ends on the last Sunday of October at 2 a.m."
  (cond ((< (nth 1 dt) 4) 0)
	((= (nth 1 dt) 4)
	 (let ((sunday (math-prev-weekday-in-month date dt 7 0)))
	   (cond ((< (nth 2 dt) sunday) 0)
		 ((= (nth 2 dt) sunday)
		  (if (>= (nth 3 dt) (+ 3 bump)) -1 0))
		 (t -1))))
	((< (nth 1 dt) 10) -1)
	((= (nth 1 dt) 10)
	 (let ((sunday (math-prev-weekday-in-month date dt 31 0)))
	   (cond ((< (nth 2 dt) sunday) -1)
		 ((= (nth 2 dt) sunday)
		  (if (>= (nth 3 dt) (+ 2 bump)) 0 -1))
		 (t 0))))
	(t 0))
)

;;; Compute the day (1-31) of the WDAY (0-6) on or preceding the given
;;; day of the given month.
(defun math-prev-weekday-in-month (date dt day wday)
  (or day (setq day (nth 2 dt)))
  (if (> day (math-days-in-month (car dt) (nth 1 dt)))
      (setq day (math-days-in-month (car dt) (nth 1 dt))))
  (let ((zeroth (math-sub (math-floor date) (nth 2 dt))))
    (math-sub (nth 1 (calcFunc-newweek (math-add zeroth day))) zeroth))
)

(defun calcFunc-pwday (date &optional day weekday)
  (if (eq (car-safe date) 'date)
      (setq date (nth 1 date)))
  (or (math-realp date)
      (math-reject-arg date 'datep))
  (if (math-messy-integerp day) (setq day (math-trunc day)))
  (or (integerp day) (math-reject-arg day 'fixnump))
  (if (= day 0) (setq day 31))
  (and (or (< day 7) (> day 31)) (math-reject-arg day 'range))
  (math-prev-weekday-in-month date (math-date-to-dt date) day (or weekday 0))
)


(defun calcFunc-newweek (date &optional weekday)
  (if (eq (car-safe date) 'date)
      (setq date (nth 1 date)))
  (or (math-realp date)
      (math-reject-arg date 'datep))
  (or weekday (setq weekday 0))
  (and (math-messy-integerp weekday) (setq weekday (math-trunc weekday)))
  (or (integerp weekday) (math-reject-arg weekday 'fixnump))
  (and (or (< weekday 0) (> weekday 6)) (math-reject-arg weekday 'range))
  (setq date (math-floor date))
  (list 'date (math-sub date (calcFunc-weekday (math-sub date weekday))))
)

(defun calcFunc-newmonth (date &optional day)
  (or day (setq day 1))
  (and (math-messy-integerp day) (setq day (math-trunc day)))
  (or (integerp day) (math-reject-arg day 'fixnump))
  (and (or (< day 0) (> day 31)) (math-reject-arg day 'range))
  (let ((dt (math-date-to-dt date)))
    (if (or (= day 0) (> day (math-days-in-month (car dt) (nth 1 dt))))
	(setq day (math-days-in-month (car dt) (nth 1 dt))))
    (and (eq (car dt) 1752) (= (nth 1 dt) 9)
	 (if (>= day 14) (setq day (- day 11))))
    (list 'date (math-add (math-dt-to-date (list (car dt) (nth 1 dt) 1))
			  (1- day))))
)

(defun calcFunc-newyear (date &optional day)
  (or day (setq day 1))
  (and (math-messy-integerp day) (setq day (math-trunc day)))
  (or (integerp day) (math-reject-arg day 'fixnump))
  (let ((dt (math-date-to-dt date)))
    (if (and (>= day 0) (<= day 366))
	(let ((max (if (eq (car dt) 1752) 355
		     (if (math-leap-year-p (car dt)) 366 365))))
	  (if (or (= day 0) (> day max)) (setq day max))
	  (list 'date (math-add (math-dt-to-date (list (car dt) 1 1))
				(1- day))))
      (if (and (>= day -12) (<= day -1))
	  (list 'date (math-dt-to-date (list (car dt) (- day) 1)))
	(math-reject-arg day 'range))))
)

(defun calcFunc-incmonth (date &optional step)
  (or step (setq step 1))
  (and (math-messy-integerp step) (setq step (math-trunc step)))
  (or (math-integerp step) (math-reject-arg step 'integerp))
  (let* ((dt (math-date-to-dt date))
	 (year (car dt))
	 (month (math-add (1- (nth 1 dt)) step))
	 (extra (calcFunc-idiv month 12))
	 (day (nth 2 dt)))
    (setq month (1+ (math-sub month (math-mul extra 12)))
	  year (math-add year extra)
	  day (min day (math-days-in-month year month)))
    (and (math-posp (car dt)) (not (math-posp year))
	 (setq year (math-sub year 1)))   ; did we go past the year zero?
    (and (math-negp (car dt)) (not (math-negp year))
	 (setq year (math-add year 1)))
    (list 'date (math-dt-to-date
		 (cons year (cons month (cons day (cdr (cdr (cdr dt)))))))))
)

(defun calcFunc-incyear (date &optional step)
  (calcFunc-incmonth date (math-mul (or step 1) 12))
)



(defun calcFunc-bsub (a b)
  (or (eq (car-safe a) 'date)
      (math-reject-arg a 'datep))
  (if (eq (car-safe b) 'date)
      (if (math-lessp (nth 1 a) (nth 1 b))
	  (math-neg (calcFunc-bsub b a))
	(math-setup-holidays b)
	(let* ((da (math-to-business-day a))
	       (db (math-to-business-day b)))
	  (math-add (math-sub (car da) (car db))
		    (if (and (cdr db) (not (cdr da))) 1 0))))
    (calcFunc-badd a (math-neg b)))
)

(defun calcFunc-badd (a b)
  (if (eq (car-safe b) 'date)
      (if (eq (car-safe a) 'date)
	  (math-reject-arg nil "*Illegal combination in date arithmetic")
	(calcFunc-badd b a))
    (if (eq (car-safe a) 'date)
	(if (Math-realp b)
	    (if (Math-zerop b)
		a
	      (let* ((d (math-to-business-day a))
		     (bb (math-add (car d)
				   (if (and (cdr d) (Math-posp b))
				       (math-sub b 1) b))))
		(or (math-from-business-day bb)
		    (calcFunc-badd a b))))
	  (if (eq (car-safe b) 'hms)
	      (let ((hours (nth 7 math-holidays-cache)))
		(setq b (math-div (math-from-hms b 'deg) 24))
		(if hours
		    (setq b (math-div b (cdr hours))))
		(calcFunc-badd a b))
	    (math-reject-arg nil "*Illegal combination in date arithmetic")))
      (math-reject-arg a 'datep)))
)

(defun calcFunc-holiday (a)
  (if (cdr (math-to-business-day a)) 1 0)
)


(setq math-holidays-cache nil)
(setq math-holidays-cache-tag t)


;;; Compute the number of business days since Jan 1, 1 AD.

(defun math-to-business-day (date &optional need-year)
  (if (eq (car-safe date) 'date)
      (setq date (nth 1 date)))
  (or (Math-realp date)
      (math-reject-arg date 'datep))
  (let* ((day (math-floor date))
	 (time (math-sub date day))
	 (dt (math-date-to-dt day))
	 (delta 0)
	 (holiday nil))
    (or (not need-year) (eq (car dt) need-year)
	(math-reject-arg (list 'date day) "*Generated holiday has wrong year"))
    (math-setup-holidays date)
    (let ((days (car math-holidays-cache)))
      (while (and (setq days (cdr days)) (< (car days) day))
	(setq delta (1+ delta)))
      (and days (= day (car days))
	   (setq holiday t)))
    (let* ((weekdays (nth 3 math-holidays-cache))
	   (weeks (1- (/ (+ day 6) 7)))
	   (wkday (- day 1 (* weeks 7))))
      (setq delta (+ delta (* weeks (length weekdays))))
      (while (and weekdays (< (car weekdays) wkday))
	(setq weekdays (cdr weekdays)
	      delta (1+ delta)))
      (and weekdays (eq wkday (car weekdays))
	   (setq holiday t)))
    (let ((hours (nth 7 math-holidays-cache)))
      (if hours
	  (progn
	    (setq time (math-div (math-sub time (car hours)) (cdr hours)))
	    (if (Math-lessp time 0) (setq time 0))
	    (or (Math-lessp time 1)
		(setq time
		      (math-sub 1
				(math-div 1 (math-mul 86400 (cdr hours)))))))))
    (cons (math-add (math-sub day delta) time) holiday))
)


;;; Compute the date a certain number of business days since Jan 1, 1 AD.
;;; If this returns NIL, holiday table was adjusted; redo calculation.

(defun math-from-business-day (num)
  (let* ((day (math-floor num))
	 (time (math-sub num day)))
    (or (integerp day)
	(math-reject-arg nil "*Date is outside valid range"))
    (math-setup-holidays)
    (let ((days (nth 1 math-holidays-cache))
	  (delta 0))
      (while (and (setq days (cdr days)) (< (car days) day))
	(setq delta (1+ delta)))
      (setq day (+ day delta)))
    (let* ((weekdays (nth 3 math-holidays-cache))
	   (bweek (- 7 (length weekdays)))
	   (weeks (1- (/ (+ day (1- bweek)) bweek)))
	   (wkday (- day 1 (* weeks bweek)))
	   (w 0))
      (setq day (+ day (* weeks (length weekdays))))
      (while (if (memq w weekdays)
		 (setq day (1+ day))
	       (> (setq wkday (1- wkday)) 0))
	(setq w (1+ w)))
      (let ((hours (nth 7 math-holidays-cache)))
	(if hours
	    (setq time (math-add (math-mul time (cdr hours)) (car hours)))))
      (and (not (math-setup-holidays day))
	   (list 'date (math-add day time)))))
)


(defun math-setup-holidays (&optional date)
  (or (eq (calc-var-value 'var-Holidays) math-holidays-cache-tag)
      (let ((h (calc-var-value 'var-Holidays))
	    (wdnames '( (sun . 0) (mon . 1) (tue . 2) (wed . 3)
			(thu . 4) (fri . 5) (sat . 6) ))
	    (days nil) (weekdays nil) (exprs nil) (limit nil) (hours nil))
	(or (math-vectorp h)
	    (math-reject-arg h "*Holidays variable must be a vector"))
	(while (setq h (cdr h))
	  (cond ((or (and (eq (car-safe (car h)) 'date)
			  (integerp (nth 1 (car h))))
		     (and (eq (car-safe (car h)) 'intv)
			  (eq (car-safe (nth 2 (car h))) 'date))
		     (eq (car-safe (car h)) 'vec))
		 (setq days (cons (car h) days)))
		((and (eq (car-safe (car h)) 'var)
		      (assq (nth 1 (car h)) wdnames))
		 (setq weekdays (cons (cdr (assq (nth 1 (car h)) wdnames))
				      weekdays)))
		((and (eq (car-safe (car h)) 'intv)
		      (eq (car-safe (nth 2 (car h))) 'hms)
		      (eq (car-safe (nth 3 (car h))) 'hms))
		 (if hours
		     (math-reject-arg
		      (car h) "*Only one hours interval allowed in Holidays"))
		 (setq hours (math-div (car h) '(hms 24 0 0)))
		 (if (or (Math-lessp (nth 2 hours) 0)
			 (Math-lessp 1 (nth 3 hours)))
		     (math-reject-arg
		      (car h) "*Hours interval out of range"))
		 (setq hours (cons (nth 2 hours)
				   (math-sub (nth 3 hours) (nth 2 hours))))
		 (if (Math-zerop (cdr hours))
		     (math-reject-arg
		      (car h) "*Degenerate hours interval")))
		((or (and (eq (car-safe (car h)) 'intv)
			  (Math-integerp (nth 2 (car h)))
			  (Math-integerp (nth 3 (car h))))
		     (and (integerp (car h))
			  (> (car h) 1900) (< (car h) 2100)))
		 (if limit
		     (math-reject-arg
		      (car h) "*Only one limit allowed in Holidays"))
		 (setq limit (calcFunc-vint (car h) '(intv 3 1 2737)))
		 (if (equal limit '(vec))
		     (math-reject-arg (car h) "*Limit is out of range")))
		((or (math-expr-contains (car h) '(var y var-y))
		     (math-expr-contains (car h) '(var m var-m)))
		 (setq exprs (cons (car h) exprs)))
		(t (math-reject-arg
		    (car h) "*Holidays must contain a vector of holidays"))))
	(if (= (length weekdays) 7)
	    (math-reject-arg nil "*Too many weekend days"))
	(setq math-holidays-cache (list (list -1)  ; 0: days list
					(list -1)  ; 1: inverse-days list
					nil        ; 2: exprs
					(sort weekdays '<)
					(or limit '(intv 3 1 2737))
					nil        ; 5: (lo.hi) expanded years
					(cons exprs days)
					hours)     ; 7: business hours
	      math-holidays-cache-tag (calc-var-value 'var-Holidays))))
  (if date
      (let ((year (calcFunc-year date))
	    (limits (nth 5 math-holidays-cache))
	    (done nil))
	(or (eq (calcFunc-in year (nth 4 math-holidays-cache)) 1)
	    (progn
	      (or (eq (car-safe date) 'date) (setq date (list 'date date)))
	      (math-reject-arg date "*Date is outside valid range")))
	(unwind-protect
	    (let ((days (nth 6 math-holidays-cache)))
	      (if days
		  (let ((year nil))   ; see below
		    (setcar (nthcdr 6 math-holidays-cache) nil)
		    (math-setup-add-holidays (cons 'vec (cdr days)))
		    (setcar (nthcdr 2 math-holidays-cache) (car days))))
	      (cond ((not (nth 2 math-holidays-cache))
		     (setq done t)
		     nil)
		    ((not limits)
		     (setcar (nthcdr 5 math-holidays-cache) (cons year year))
		     (math-setup-year-holidays year)
		     (setq done t))
		    ((< year (car limits))
		     (message "Computing holidays, %d .. %d"
			      year (1- (car limits)))
		     (calc-set-command-flag 'clear-message)
		     (while (< year (car limits))
		       (setcar limits (1- (car limits)))
		       (math-setup-year-holidays (car limits)))
		     (setq done t))
		    ((> year (cdr limits))
		     (message "Computing holidays, %d .. %d"
			      (1+ (cdr limits)) year)
		     (calc-set-command-flag 'clear-message)
		     (while (> year (cdr limits))
		       (setcdr limits (1+ (cdr limits)))
		       (math-setup-year-holidays (cdr limits)))
		     (setq done t))
		    (t
		     (setq done t)
		     nil)))
	  (or done (setq math-holidays-cache-tag t)))))
)

(defun math-setup-year-holidays (year)
  (let ((exprs (nth 2 math-holidays-cache)))
    (while exprs
      (let* ((var-y year)
	     (var-m nil)
	     (expr (math-evaluate-expr (car exprs))))
	(if (math-expr-contains expr '(var m var-m))
	    (let ((var-m 0))
	      (while (<= (setq var-m (1+ var-m)) 12)
		(math-setup-add-holidays (math-evaluate-expr expr))))
	  (math-setup-add-holidays expr)))
      (setq exprs (cdr exprs))))
)

(defun math-setup-add-holidays (days)   ; uses "year"
  (cond ((eq (car-safe days) 'vec)
	 (while (setq days (cdr days))
	   (math-setup-add-holidays (car days))))
	((eq (car-safe days) 'intv)
	 (let ((day (math-ceiling (nth 2 days))))
	   (or (eq (calcFunc-in day days) 1)
	       (setq day (math-add day 1)))
	   (while (eq (calcFunc-in day days) 1)
	     (math-setup-add-holidays day)
	     (setq day (math-add day 1)))))
	((eq (car-safe days) 'date)
	 (math-setup-add-holidays (nth 1 days)))
	((eq days 0))
	((integerp days)
	 (let ((b (math-to-business-day days year)))
	   (or (cdr b)   ; don't register holidays twice!
	       (let ((prev (car math-holidays-cache))
		     (iprev (nth 1 math-holidays-cache)))
		 (while (and (cdr prev) (< (nth 1 prev) days))
		   (setq prev (cdr prev) iprev (cdr iprev)))
		 (setcdr prev (cons days (cdr prev)))
		 (setcdr iprev (cons (car b) (cdr iprev)))
		 (while (setq iprev (cdr iprev))
		   (setcar iprev (1- (car iprev))))))))
	((Math-realp days)
	 (math-reject-arg (list 'date days) "*Invalid holiday value"))
	(t
	 (math-reject-arg days "*Holiday formula failed to evaluate")))
)




;;;; Error forms.

;;; Build a standard deviation form.  [X X X]
(defun math-make-sdev (x sigma)
  (if (memq (car-safe x) '(date mod sdev intv vec))
      (math-reject-arg x 'realp))
  (if (memq (car-safe sigma) '(date mod sdev intv vec))
      (math-reject-arg sigma 'realp))
  (if (or (Math-negp sigma) (memq (car-safe sigma) '(cplx polar)))
      (setq sigma (math-abs sigma)))
  (if (and (Math-zerop sigma) (Math-scalarp x))
      x
    (list 'sdev x sigma))
)
(defun calcFunc-sdev (x sigma)
  (math-make-sdev x sigma)
)



;;;; Modulo forms.

(defun math-normalize-mod (a)
  (let ((n (math-normalize (nth 1 a)))
	(m (math-normalize (nth 2 a))))
    (if (and (math-anglep n) (math-anglep m) (math-posp m))
	(math-make-mod n m)
      (math-normalize (list 'calcFunc-makemod n m))))
)

;;; Build a modulo form.  [N R R]
(defun math-make-mod (n m)
  (setq calc-previous-modulo m)
  (and n
       (cond ((not (Math-anglep m))
	      (math-reject-arg m 'anglep))
	     ((not (math-posp m))
	      (math-reject-arg m 'posp))
	     ((Math-anglep n)
	      (if (or (Math-negp n)
		      (not (Math-lessp n m)))
		  (list 'mod (math-mod n m) m)
		(list 'mod n m)))
	     ((memq (car n) '(+ - / vec neg))
	      (math-normalize
	       (cons (car n)
		     (mapcar (function (lambda (x) (math-make-mod x m)))
			     (cdr n)))))
	     ((and (eq (car n) '*) (Math-anglep (nth 1 n)))
	      (math-mul (math-make-mod (nth 1 n) m) (nth 2 n)))
	     ((memq (car n) '(* ^ var calcFunc-subscr))
	      (math-mul (math-make-mod 1 m) n))
	     (t (math-reject-arg n 'anglep))))
)
(defun calcFunc-makemod (n m)
  (math-make-mod n m)
)



;;;; Interval forms.

;;; Build an interval form.  [X S X X]
(defun math-make-intv (mask lo hi)
  (if (memq (car-safe lo) '(cplx polar mod sdev intv vec))
      (math-reject-arg lo 'realp))
  (if (memq (car-safe hi) '(cplx polar mod sdev intv vec))
      (math-reject-arg hi 'realp))
  (or (eq (eq (car-safe lo) 'date) (eq (car-safe hi) 'date))
      (math-reject-arg (if (eq (car-safe lo) 'date) hi lo) 'datep))
  (if (and (or (Math-realp lo) (eq (car lo) 'date))
	   (or (Math-realp hi) (eq (car hi) 'date)))
      (let ((cmp (math-compare lo hi)))
	(if (= cmp 0)
	    (if (= mask 3)
		lo
	      (list 'intv mask lo hi))
	  (if (> cmp 0)
	      (if (= mask 3)
		  (list 'intv 2 lo lo)
		(list 'intv mask lo lo))
	    (list 'intv mask lo hi))))
    (list 'intv mask lo hi))
)
(defun calcFunc-intv (mask lo hi)
  (if (math-messy-integerp mask) (setq mask (math-trunc mask)))
  (or (natnump mask) (math-reject-arg mask 'fixnatnump))
  (or (<= mask 3) (math-reject-arg mask 'range))
  (math-make-intv mask lo hi)
)

(defun math-sort-intv (mask lo hi)
  (if (Math-lessp hi lo)
      (math-make-intv (aref [0 2 1 3] mask) hi lo)
    (math-make-intv mask lo hi))
)




(defun math-combine-intervals (a am b bm c cm d dm)
  (let (res)
    (if (= (setq res (math-compare a c)) 1)
	(setq a c am cm)
      (if (= res 0)
	  (setq am (or am cm))))
    (if (= (setq res (math-compare b d)) -1)
	(setq b d bm dm)
      (if (= res 0)
	  (setq bm (or bm dm))))
    (math-make-intv (+ (if am 2 0) (if bm 1 0)) a b))
)


(defun math-div-mod (a b m)   ; [R R R R]  (Returns nil if no solution)
  (and (Math-integerp a) (Math-integerp b) (Math-integerp m)
       (let ((u1 1) (u3 b) (v1 0) (v3 m))
	 (while (not (eq v3 0))   ; See Knuth sec 4.5.2, exercise 15
	   (let* ((q (math-idivmod u3 v3))
		  (t1 (math-sub u1 (math-mul v1 (car q)))))
	     (setq u1 v1  u3 v3  v1 t1  v3 (cdr q))))
	 (let ((q (math-idivmod a u3)))
	   (and (eq (cdr q) 0)
		(math-mod (math-mul (car q) u1) m)))))
)

(defun math-mod-intv (a b)
  (let* ((q1 (math-floor (math-div (nth 2 a) b)))
	 (q2 (math-floor (math-div (nth 3 a) b)))
	 (m1 (math-sub (nth 2 a) (math-mul q1 b)))
	 (m2 (math-sub (nth 3 a) (math-mul q2 b))))
    (cond ((equal q1 q2)
	   (math-sort-intv (nth 1 a) m1 m2))
	  ((and (math-equal-int (math-sub q2 q1) 1)
		(math-zerop m2)
		(memq (nth 1 a) '(0 2)))
	   (math-make-intv (nth 1 a) m1 b))
	  (t
	   (math-make-intv 2 0 b))))
)


(defun math-read-angle-brackets ()
  (let* ((last (or (math-check-for-commas t) (length exp-str)))
	 (str (substring exp-str exp-pos last))
	 (res
	  (if (string-match "\\` *\\([a-zA-Z#][a-zA-Z0-9#]* *,? *\\)*:" str)
	      (let ((str1 (substring str 0 (1- (match-end 0))))
		    (str2 (substring str (match-end 0)))
		    (calc-hashes-used 0))
		(setq str1 (math-read-expr (concat "[" str1 "]")))
		(if (eq (car-safe str1) 'error)
		    str1
		  (setq str2 (math-read-expr str2))
		  (if (eq (car-safe str2) 'error)
		      str2
		    (append '(calcFunc-lambda) (cdr str1) (list str2)))))
	    (if (string-match "#" str)
		(let ((calc-hashes-used 0))
		  (and (setq str (math-read-expr str))
		       (if (eq (car-safe str) 'error)
			   str
			 (append '(calcFunc-lambda)
				 (calc-invent-args calc-hashes-used)
				 (list str)))))
	      (math-parse-date str)))))
    (if (stringp res)
	(throw 'syntax res))
    (if (eq (car-safe res) 'error)
	(throw 'syntax (nth 2 res)))
    (setq exp-pos (1+ last))
    (math-read-token)
    res)
)

