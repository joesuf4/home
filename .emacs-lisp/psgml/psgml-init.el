					;-*-emacs-lisp-*-
;; arch-tag: 71313809-c3bd-4550-80da-e133ebb269ba

;; Fixes for Debian
(if (file-readable-p (concat "=F" "/psgml.elc"))
    (progn
      
      (if (fboundp 'debian-pkg-add-load-path-item)
	  (setq load-path (debian-pkg-add-load-path-item "=F"))
	(setq load-path (cons "=F" load-path)))

      ;; Make sure that the uncompiled files are also in the
      ;; laod-path, though near the end.
      (setq load-path (nconc load-path (list "/usr/share/emacs/site-lisp/psgml")))

      (setq sgml-ecat-files '("ECAT" "~/sgml/ECAT" "/usr/share/sgml/ECAT"
			      "/usr/local/share/sgml/ECAT"
			      "/usr/local/lib/sgml/ECAT" ))

      (or (assoc "\\.s?html?\\'" auto-mode-alist)
	  (setq auto-mode-alist (cons '("\\.s?html?\\'" . html-mode)
				      auto-mode-alist)))

      (setq auto-mode-alist (append '(("\\.xml$" . xml-mode)) auto-mode-alist))
      
      (setq
       sgml-data-directory "/usr/share/sgml/declaration/"
       sgml-xml-declaration "/usr/share/sgml/declaration/xml.dcl"
       sgml-display-char-list-filename "/usr/share/sgml/charsets/iso88591.map"
       sgml-public-map '("%S"  "/usr/share/sgml/%S" "/usr/share/sgml/%o/%c/%d"
			  "/usr/local/share/sgml/%o/%c/%d"
			  "/usr/local/lib/sgml/%o/%c/%d")
       sgml-system-path '("/usr/share/sgml" "/usr/share/sgml/cdtd" 
			  "/usr/local/share/sgml" "/usr/local/lib/sgml")
       )
      (eval-after-load "psgml"
	'(progn
	   (let ((program (if (file-exists-p "/usr/bin/onsgmls")
			      "onsgmls"
			    "nsgmls")))
	     (setq-default sgml-validate-command
			   (concat program " -e -g -s -u %s %s"))
	     (setq-default sgml-xml-validate-command
			   (concat program " -wxml -e -g -s -u %s %s")))
	   ))



;;;; Autoloads and hooks

      (autoload 'compile-internal "compile" "")
      (autoload 'html-mode "psgml-html" "HTML mode." t)
      (autoload 'reporter-submit-bug-report "reporter" nil)
      (autoload 'sgml-attrib-menu "psgml-edit" nil)
      (autoload 'sgml-backward-element "psgml-edit" nil)
      (autoload 'sgml-backward-up-element "psgml-edit" nil)
      (autoload 'sgml-beginning-of-element "psgml-edit" nil)
      (autoload 'sgml-change-element-name "psgml-edit" nil)
      (autoload 'sgml-charent-to-display-char "psgml-charent" nil)
      (autoload 'sgml-check-dtd-subset  "psgml-dtd")
      (autoload 'sgml-complete "psgml-edit" nil)
      (autoload 'sgml-custom-dtd "psgml-edit" nil)
      (autoload 'sgml-custom-markup "psgml-edit" nil)
      (autoload 'sgml-describe-element-type "psgml-info" nil)
      (autoload 'sgml-describe-entity "psgml-info" nil)
      (autoload 'sgml-display-char-to-charent "psgml-charent" nil)
      (autoload 'sgml-do-set-option "psgml-edit" nil)
      (autoload 'sgml-do-usemap-element  "psgml-dtd" nil)
      (autoload 'sgml-doctype-insert "psgml-edit" nil)
      (autoload 'sgml-down-element "psgml-edit" nil)
      (autoload 'sgml-edit-attrib-clear "psgml-edit" nil)
      (autoload 'sgml-edit-attrib-default "psgml-edit" nil)
      (autoload 'sgml-edit-attrib-field-end "psgml-edit" nil)
      (autoload 'sgml-edit-attrib-field-start "psgml-edit" nil)
      (autoload 'sgml-edit-attrib-finish "psgml-edit" nil)
      (autoload 'sgml-edit-attrib-next "psgml-edit" nil)
      (autoload 'sgml-edit-attributes "psgml-edit" nil)
      (autoload 'sgml-element-endable-p "psgml-edit" nil)
      (autoload 'sgml-element-menu "psgml-edit" nil)
      (autoload 'sgml-end-of-element "psgml-edit" nil)
      (autoload 'sgml-end-tag-menu "psgml-edit" nil)
      (autoload 'sgml-entities-menu "psgml-edit" nil)
      (autoload 'sgml-expand-all-shortrefs "psgml-edit" nil)
      (autoload 'sgml-expand-element "psgml-edit" nil)
      (autoload 'sgml-expand-entity-reference "psgml-edit" nil)
      (autoload 'sgml-file-options-menu "psgml-edit" nil)
      (autoload 'sgml-fill-element "psgml-edit" nil)
      (autoload 'sgml-fold-element "psgml-edit" nil)
      (autoload 'sgml-fold-region "psgml-edit" nil)
      (autoload 'sgml-fold-subelement "psgml-edit" nil)
      (autoload 'sgml-forward-element "psgml-edit" nil)
      (autoload 'sgml-general-dtd-info "psgml-info" nil)
      (autoload 'sgml-hide-attributes "psgml-edit" nil)
      (autoload 'sgml-hide-tags "psgml-edit" nil)
      (autoload 'sgml-indent-line "psgml-edit" nil)
      (autoload 'sgml-insert-attribute "psgml-edit" nil)
      (autoload 'sgml-insert-element "psgml-edit" nil)
      (autoload 'sgml-insert-end-tag "psgml-edit" nil)
      (autoload 'sgml-insert-tag "psgml-edit" nil)
      (autoload 'sgml-kill-element "psgml-edit" nil)
      (autoload 'sgml-kill-markup "psgml-edit" nil)
      (autoload 'sgml-list-attributes "psgml-info" nil)
      (autoload 'sgml-list-content-elements "psgml-info" nil)
      (autoload 'sgml-list-elements "psgml-info" nil)
      (autoload 'sgml-list-occur-in-elements "psgml-info" nil)
      (autoload 'sgml-list-terminals "psgml-info" nil)
      (autoload 'sgml-list-valid-tags "psgml-edit" nil)
      (autoload 'sgml-load-dtd "psgml-parse" nil)
      (autoload 'sgml-make-character-reference "psgml-edit" nil)
      (autoload 'sgml-mark-current-element "psgml-edit" nil)
      (autoload 'sgml-mark-element "psgml-edit" nil)
      (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )
      (autoload 'sgml-next-data-field "psgml-edit" nil)
      (autoload 'sgml-next-trouble-spot "psgml-edit" nil)
      (autoload 'sgml-normalize "psgml-edit" nil)
      (autoload 'sgml-normalize-element "psgml-edit" nil)
      (autoload 'sgml-parse-prolog "psgml-parse" nil)
      (autoload 'sgml-save-dtd "psgml-dtd" nil)
      (autoload 'sgml-show-attributes "psgml-edit" nil)
      (autoload 'sgml-show-context "psgml-edit" nil)
      (autoload 'sgml-show-or-clear-log "psgml-parse" nil)
      (autoload 'sgml-show-tags "psgml-edit" nil)
      (autoload 'sgml-split-element "psgml-edit" nil)
      (autoload 'sgml-start-tag-menu "psgml-edit" nil)
      (autoload 'sgml-tag-region "psgml-edit" nil)
      (autoload 'sgml-tag-region-menu "psgml-edit" nil)
      (autoload 'sgml-tags-menu "psgml-edit" nil)
      (autoload 'sgml-translate-model "psgml-dtd" "" nil)
      (autoload 'sgml-transpose-element "psgml-edit" nil)
      (autoload 'sgml-unfold-all "psgml-edit" nil)
      (autoload 'sgml-unfold-element "psgml-edit" nil)
      (autoload 'sgml-unfold-line "psgml-edit" nil)
      (autoload 'sgml-untag-element "psgml-edit" nil)
      (autoload 'sgml-up-element "psgml-edit" nil)
      (autoload 'sgml-user-options-menu "psgml-edit" nil)
      (autoload 'sgml-what-element "psgml-edit" nil)
      (autoload 'sgml-write-dtd  "psgml-dtd")
      (autoload 'xml-mode "psgml" nil t)
;;;(autoload 'sgml-xpointer "psgml-xpointer" nil t)


      (provide 'psgml-init)
      ))
