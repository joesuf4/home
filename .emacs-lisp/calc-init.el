;; -*-emacs-lisp-*-
;; arch-tag: efa17cbf-d9f1-47c2-820b-986a971322bb

(if (file-exists-p (concat "/usr/share/"
			   (symbol-name debian-emacs-flavor)
			   "/site-lisp/calc/calc.elc"))
    (progn
      (provide 'calc-init)
      (if (fboundp 'debian-pkg-add-load-path-item)
	  (setq load-path (debian-pkg-add-load-path-item (concat "/usr/share/"
						 (symbol-name 
						  debian-emacs-flavor)
						 "/site-lisp/calc")))
	(setq load-path (cons (concat "/usr/share/"
				      (symbol-name debian-emacs-flavor)
				      "/site-lisp/calc") load-path)))
      ;; Make sure that the uncompiled files are also in the
      ;; laod-path, though near the end.
      (setq load-path (nconc load-path 
			     (list "/usr/share/emacs/site-lisp/calc")))


(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
(autoload 'defmath		   "calc" nil t t)
(autoload 'calc			   "calc" "Calculator Mode" t)
(autoload 'quick-calc		   "calc" "Quick Calculator" t)
(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
(global-set-key "\e#" 'calc-dispatch)
      ))

