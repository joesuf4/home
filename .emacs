; .emacs config file
(global-set-key (kbd "C-l") 'redraw-display)
(setq load-path (cons "~/.emacs-lisp" load-path))
(setq exec-path (append exec-path '("~/bin")))
;(require 'w3m) ; use W3 browser
(setq semantic-load-turn-useful-things-on t)
(require 'cedet)
(require 'git)
(require 'doxymacs)
(require 'tls)
(require 'erc)

(load-library "semantic-load")
(semantic-load-enable-code-helpers)

;;;;;;;;;;;;;;;;;;;;
;; set up unicode
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;--------------------------------------------------
; erc + tls

(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"))

 ; M-x start-irc
 (defun start-irc ()
   "Connect to IRC."
   (interactive)
   (erc-tls :server "irc.freenode.net" :port 7000
        :nick "joes4" :password (password-read "Freenode Password: " "freenode") :full-name "Joe Schaefer")
   (setq erc-autojoin-channels-alist '(("freenode.net" "#asfinfra"))))

;--------------------------------------------------
; hygenics (semantic.cache, backup and autosave files)

(defun make-private-directory (directory-list)
  (let ((umask (default-file-modes)))
    (unwind-protect
        (progn
          (set-default-file-modes ?\700)
          (dolist (dir directory-list)
            (make-directory dir t)))
      (set-default-file-modes umask))))

(setq semanticdb-project-roots (list "~/src/apache"))
(setq temporary-file-directory (concat "/tmp/emacs-" (user-login-name) "/"))
(setq semanticdb-default-save-directory
      (concat temporary-file-directory "semantics/"))
(defvar autosave-dir (concat temporary-file-directory "autosaves/"))
(defvar backup-dir (concat temporary-file-directory "backups/"))

(make-private-directory (list semanticdb-default-save-directory
                              autosave-dir backup-dir))

;; Put autosave files (ie #foo#) in one place, *not* scattered all over the
;; file system! (The make-autosave-file-name function is invoked to determine
;; the filename of an autosave file.)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (interactive)
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))


;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(setq backup-directory-alist (list (cons "." backup-dir)))


;--------------------------------------------------
; tramp hygenics

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory autosave-dir)
;(setq tramp-backup-directory-alist (list (cons "." backup-dir)))

;--------------------------------------------------
; Mario Lang's indirect-region maps a region into its own buffer

(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)

(defun indirect-region (start end)
  "Edit the current region in another buffer.
   If the buffer-local variable `indirect-mode-name' is not set, prompt
   for mode name to choose for the indirect buffer interactively.
   Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (mapcar (lambda (e)
                               (list (symbol-name e)))
                             (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

;--------------------------------------------------

; (load-file "/usr/gnu/share/emacs/ilisp/ilisp.emacs")
;(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))

; remove silly toolbar & menu
(ignore-errors 
  (tool-bar-mode 0)
  (menu-bar-mode 0)
)
; jclark's nxml mode
(load "nxml-mode/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|xsd\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))

; Steve Reichoer's svn-status mode
(require 'psvn)

;Info path
;(setq Info-directory-list
;      '("/usr/gnu/info"
;	"/usr/info"))

(display-time) ; show time on mode line
(global-font-lock-mode t) ; turn on font-lock mode
(column-number-mode t) ; show column number

; enable auto-fill-mode and set width
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'shell-script-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq-default fill-column 79)
(add-hook 'message-setup-hook '(lambda ()
                                 (set-fill-column 72)
                                 (add-to-list 'message-syntax-checks
                                              '(sender . disabled))))
;; (add-hook 'message-send-hook 'ispell-message)
(setq message-cite-function 'message-cite-original-without-signature)

(setq transient-mark-mode t) ;comment out if bothersome

(setq outline-minor-mode-prefix "\C-c\C-o") ; outlining rebind
(setq tab-stop-list '(2 4 8 12 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
(setq-default indent-tabs-mode nil)


;__________________________________________________

; VM
;(autoload 'vm "vm" "Start VM on your primary inbox." t)
;(autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
;(autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
;(autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
;(autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
;(autoload 'vm-mail "vm" "Send a mail message using VM." t)
;(autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)

; more Mail STUFF
;(custom-set-variables
; '(mail-from-style (quote system-default))
; '(user-mail-address "joe@sunstarsys.com"))


;__________________________________________________

; GPG stuff (MailCrypt)
;(require 'mailcrypt-init) ; provides "mc-setversion"
;(mc-setversion "gpg") ; GnuPG privacy

;(autoload 'mc-install-write-mode "mailcrypt" nil t)
;(autoload 'mc-install-read-mode "mailcrypt" nil t)
;(add-hook 'mail-mode-hook 'mc-install-write-mode)

;     (add-hook 'rmail-show-message-hook 'mc-install-read-mode)
;     (add-hook 'rmail-summary-mode-hook 'mc-install-read-mode)

;     (add-hook 'vm-mode-hook 'mc-install-read-mode)
;     (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
;     (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
;     (add-hook 'vm-mail-mode-hook 'mc-install-write-mode)

(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
(add-hook 'message-mode-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)

;__________________________________________________
; GNUS
;(setq gnus-select-method '(nntp "newsgroups.bellsouth.net"))
(setq mail-user-agent 'gnus-user-agent)
;__________________________________________________

; first some TeX stuff...

; make sure AUCTeX package and friends load up
;(require 'tex-site)
;(setq TeX-parse-self t) ; Enable parse on load
;(setq TeX-auto-save t)  ; Enable parse on autosave
;; (setq TeX-auto-untabify nil); default (t) converts TAB's
			      ; into spaces before saving

;(setq-default TeX-master nil) ; Query for master file if it's not the
			      ; current file.
; Above adds the following to the end of a TeX-file to pass file info
; to AUC-TeX for future reference (see /usr/local/doc/auctex.dvi
; for details )-
;  % Local Variables:
;  % TeX-master: "master file"
;  % End:

; sectioning stuff
;(setq LaTeX-section-hook
;      '(LaTeX-(turn-on-auto-fill)

;section-heading
;	LaTeX-section-title
;	LaTeX-section-toc
;	LaTeX-section-section
;	LaTeX-section-label))

; Hooks to tex-specific files (containing key rebinds, mainly).  Removed
; tex-related ones- however lisp files and bibtex link remain.

;(add-hook 'bibtex-mode-hook
;	  '(lambda () (load "~/lib/emacs/bib")))
;(add-hook 'LaTeX-mode-hook
;	  '(lambda () (load "~/lib/emacs/LaTeX"))) ; caps on `LaTeX' due
					       ; to AUCTeX presence.

;some TeX additions (unhooked via AUCTeX requirement above) gray65
;(set-face-foreground 'font-lock-string-face "gray66")
;(set-face-foreground 'font-lock-warning-face "red")
;(set-face-foreground 'font-lock-comment-face "red")

;__________________________________________________
;set some more colors

;additional color hooks for Info-mode
(add-hook 'Info-mode-hook
	  '(lambda () (set-face-foreground 'info-node "orchid")))

(add-hook 'Info-mode-hook
	  '(lambda () (set-face-foreground 'info-xref "blue")))

(add-hook 'Info-mode-hook
	  '(lambda () (set-face-foreground 'highlight "blue")))

(add-hook 'Info-mode-hook
	  '(lambda () (set-face-background 'highlight "darkslategray")))


(add-hook 'font-lock-mode-hook
          '(lambda()
             (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
                 (doxymacs-font-lock))))

;__________________________________________________

; Now some C-stuff...

; allows normal <TAB> behavior in the middle of a line
(setq c-tab-always-indent nil)
(setq show-trailing-whitespace t)
(add-hook 'c-mode-common-hook (lambda ()
				(c-set-style "stroustrup")))
; ASF style
(add-hook 'c-mode-common-hook
          (function (lambda ()
			(c-set-offset 'inclass' ++)
			(c-set-offset 'defun-block-intro' ++)
			(c-set-offset 'statement-block-intro' ++)
			(c-set-offset 'substatement' ++)
			(c-set-offset 'brace-list-intro' ++)
			(c-set-offset 'statement-case-intro' ++)
			(c-set-offset 'inextern-lang' 0)
			)))



;--------------------------------------------------
;
; MMM mode for Mason
;(require 'mmm-mode)
;(mmm-add-find-file-hook)
;(add-to-list 'auto-mode-alist '("\\.pm\\'" . html-mode))
;(mmm-add-mode-ext-class 'cperl-mode "\\.pm\\'" 'embedded-css)
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (cons '("\\..*htm.*$" . html-mode) auto-mode-alist))

;(require 'js)
;(setq auto-mode-alist
;      (append '(("\\.js\\'" . js-mode))
;              auto-mode-alist))


;(autoload 'css-mode "css-mode")
;            (setq auto-mode-alist
;                 (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;__________________________________________________

; Perl -> cperl mode

(defun modify-alist (alist-symbol key value &optional search-cdr)
  (let ((alist (symbol-value alist-symbol)))
    (while alist
      (if (eq (if search-cdr
		  (cdr (car alist))
		(car (car alist))) key)
	  (setcdr (car alist) value)
	(setq alist (cdr alist))))))

(modify-alist 'interpreter-mode-alist 'perl-mode 'cperl-mode t)
(modify-alist 'auto-mode-alist        'perl-mode 'cperl-mode t)

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (define-key cperl-mode-map
	       "\C-c\C-d"
	       'cperl-perldoc-at-point)
	     (define-key cperl-mode-map
	       "\C-c\C-p"
	       'cperl-perldoc)
;	     (set-face-foreground 'cperl-array-face "goldenrod")
;	     (set-face-foreground 'cperl-hash-face "coral")
;	     (set-face-background 'cperl-array-face "white smoke")
;	     (set-face-background 'cperl-hash-face "white smoke")
             (cperl-set-style "PerlStyle")
             (setq cperl-close-paren-offset 0)
             (setq cperl-indent-parens-as-block t)
	     ))


;;--------------------------------------------------
;;--------------------------------------------------
;; compilation buffer stuff

(require 'compile)

(setq compilation-scroll-output t); let buffer scroll with output

;; Open the error_log in compile mode so it's easier to jump
;; to files with failing tests.

(setq auto-mode-alist
      (cons '("/error_log$" . compilation-mode) auto-mode-alist))

;; Include error message patterns for Apache::Test and Test::More
(dolist (item
         '(("\\[ +error\\] error running tests (please examine \\(.+\\))" 1 1)
           ("# +Failed test (\\(.+\\) at line +\\([0-9]+\\))" 1 2)
           ("# +Failed test \\([0-9]+\\) in \\(.+\\) at line \\([0-9]+\\)" 2 3)))
  (push item compilation-error-regexp-alist))


;; Related svn-status keybindings:
;;
;; C: configure
;; M: make
;; B: build (same as make)
;; T: test
;; I: install
;; G: grep

(define-key svn-status-mode-map (kbd "M") 'compile)
(define-key svn-status-mode-map (kbd "B") 'compile)

;--------------------------------------------------
;; grep over every file that svn can see.

(defvar svn-grep-find-command "grep -n -e ")
(defun svn-grep-find (command-args)
  (interactive
   (progn
     (list (read-from-minibuffer "Grep *svn-status* files (like this): "
				 svn-grep-find-command nil nil
				 'grep-find-history))))
  (let ((null-device nil))		; see grep
    (grep (concat "svn status -v | perl -nwal0 "
                  "-e 'print $F[-1] if @F and -f $F[-1]' | xargs -r0 -e "
                  command-args))))

(define-key svn-status-mode-map (kbd "G") 'svn-grep-find)


;--------------------------------------------------
(defvar test-command "make test")

;; XXX would love to launch gdb on Apache::Test segfault reports.
(defun test-compile (command)
   "M-x test-compile: Compile and run the project's test suite,
   basically duplicating `compile' and adding a simple
   post-processing regexp to preserve the *nix shell
   semantics of '\r'."

   (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Test command: "
                                   (eval test-command) nil nil
                                   '(compile-history . 1)))
     (list (eval test-command))))

  (unless (equal command (eval test-command))
    (setq test-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*test-" (downcase mmode) "*" ))))
    (compile-internal (concat command " | perl -ple 's/^.*\\r//, s/.[\\b]//sg'")
                      "No more errors")))

(define-key svn-status-mode-map (kbd "T") 'test-compile)
;--------------------------------------------------

(defvar configure-command "./configure")

(defun configure (command)
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Configure command: "
                                   (eval configure-command) nil nil
                                   '(compile-history . 1)))
     (list (eval configure-command))))

  (unless (equal command (eval configure-command))
    (setq configure-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*configure-" (downcase mmode) "*" ))))
    (compile-internal command "No more errors")))

(define-key svn-status-mode-map (kbd "C") 'configure)

;;--------------------------------------------------

(defvar install-command "make install")

(defun install (command)
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Install command: "
                                   (eval install-command) nil nil
                                   '(compile-history . 1)))
     (list (eval install-command))))

  (unless (equal command (eval install-command))
    (setq install-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*install-" (downcase mmode) "*" ))))
    (compile-internal command "No more errors")))

(define-key svn-status-mode-map (kbd "I") 'install)

;;--------------------------------------------------

;;; Kevin Rogers' compile-input.el
;;; http://groups.google.com/groups?selm=3F6A3405.8040609%40yahoo.com


(defvar comint-last-input-start)
(defvar comint-last-input-end)
(defvar comint-last-output-start)

(defun compile-enable-input ()
  "Enable user input to *compilation* buffers.
   To do so, add this function to compilation-mode-hook."
  ;; Emulate comint-mode:
  (require 'comint)
  (set (make-local-variable 'comint-input-ring)
       (make-ring comint-input-ring-size))
  (set (make-local-variable 'comint-last-input-start)
       (make-marker))
  (set-marker comint-last-input-start (point-min))
  (set (make-local-variable 'comint-last-input-end)
       (make-marker))
  (set-marker comint-last-input-end (point-min))
  (set (make-local-variable 'comint-last-output-start)
       (make-marker))
  (set-marker comint-last-output-start (point-max))
  (set (make-local-variable 'comint-accum-marker)
       (make-marker))
  (set-marker comint-accum-marker nil)
  (add-hook (make-local-variable 'pre-command-hook)
     'comint-preinput-scroll-to-bottom)
  ;; Move point to EOB on input:
  (set (make-local-variable 'comint-scroll-to-bottom-on-input)
       'this)
  ;; Restore useful bindings for input:
  (local-unset-key " ")   ; was scroll-up
  (local-unset-key "\^?")  ; was scroll-down
  ;; Add bindings to send input to compilation process:
  (local-set-key "\C-c\C-d" 'comint-send-eof)
  (local-set-key "\C-m" 'comint-send-input)
  ;; Enable input:
  (set (make-local-variable 'compile-disable-input) nil))

(add-hook 'compilation-mode-hook 'compile-enable-input)

;;--------------------------------------------------
;;--------------------------------------------------

;
;(require 'css-mode)
;(require 'javascript-mode)
;--------------------------------------------------
; JDE kit

;(setq load-path
;                      (nconc '(
;                               "/home/joe/.elisp/jde-2.1.5"
;                               )
;                               load-path))
;(require 'jde)

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(canlock-password "c6c1251bc1696898cd9a6859c6aef57382db43ab")
 '(ecb-options-version "2.27")
 '(message-send-mail-partially-limit 100000000)
 '(message-sendmail-f-is-evil t))

;;(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
;; '(mmm-default-submode-face ((t (:background "midnightblue")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

(server-start)
(ignore-errors
  (require 'battery)
  (unless (string-match "battery N/A" (battery))
    (display-battery)))

(put 'narrow-to-page 'disabled nil)
(require 'page-ext)
