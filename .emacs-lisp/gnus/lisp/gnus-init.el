(if (file-exists-p (concat "/usr/share/"
			   (symbol-name debian-emacs-flavor)
			   "/site-lisp/gnus/gnus.elc"))
    (progn
      (provide 'gnus-init)
      (if (fboundp 'debian-pkg-add-load-path-item)
	  (setq load-path (debian-pkg-add-load-path-item (concat "/usr/share/"
						 (symbol-name 
						  debian-emacs-flavor)
						 "/site-lisp/gnus")))
	(setq load-path (cons (concat "/usr/share/"
				      (symbol-name debian-emacs-flavor)
				      "/site-lisp/gnus") load-path)))
      ;; Make sure that the uncompiled files are also in the
      ;; laod-path, though near the end.
      (setq load-path (nconc load-path 
			     (list "/usr/share/emacs/site-lisp/gnus/lisp")))

      ))

(cond ((string-match "XEmacs" emacs-version))
      ((string-match "Lucid"  emacs-version))
      ((and (boundp 'epoch::version)
	    (stringp (symbol-value 'epoch::version))))
       (t
	(setq smiley-data-directory "/usr/share/pixmaps/smilies"
		gnus-xmas-glyph-directory "/usr/share/pixmaps/gnus"
		)))


