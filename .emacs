; .emacs config file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'color-theme)
(color-theme-initialize)
(color-theme-pok-wog)


(require 'bpftrace-mode)
(require 'dired-x)
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
; hygenics (semantic.cache, backup and autosave files)

(defun make-private-directory (directory-list)
  (let ((umask (default-file-modes)))
    (unwind-protect
        (progn
          (set-default-file-modes ?\700)
          (dolist (dir directory-list)
            (make-directory dir t)))
      (set-default-file-modes umask))))

;(setq semanticdb-project-roots (list "~/src/apache"))
(setq temporary-file-directory (concat "/tmp/emacs-" (user-login-name) "/"))
;(setq semanticdb-default-save-directory
;      (concat temporary-file-directory "semantics/"))
(defvar autosave-dir (concat temporary-file-directory "autosaves/"))
(defvar backup-dir (concat temporary-file-directory "backups/"))

(make-private-directory (list ;semanticdb-default-save-directory
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
; Ideal for source files with regions from different coding languages
; since you only get one language mode per buffer.

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

; remove silly toolbar & menu
(ignore-errors
  (tool-bar-mode 0)
  (menu-bar-mode 0)
)
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




(global-set-key (kbd "C-l") 'redraw-display)
(setq transient-mark-mode t) ;comment out if bothersome

(setq outline-minor-mode-prefix "\C-c\C-o") ; outlining rebind
(setq tab-stop-list '(2 4 8 12 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
(setq-default indent-tabs-mode nil)



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


;(add-hook 'font-lock-mode-hook
;          '(lambda()
;             (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;                 (doxymacs-font-lock))))

;__________________________________________________

; Now some C-stuff...

; allows normal <TAB> behavior in the middle of a line
(setq c-tab-always-indent nil)
(setq show-trailing-whitespace t)
;(add-hook 'c-mode-common-hook (lambda ()
;				(c-set-style "stroustrup")))
; ASF style
;(add-hook 'c-mode-common-hook
;          (function (lambda ()
;			(c-set-offset 'inclass' ++)
;			(c-set-offset 'defun-block-intro' ++)
;			(c-set-offset 'statement-block-intro' ++)
;			(c-set-offset 'substatement' ++)
;			(c-set-offset 'brace-list-intro' ++)
;			(c-set-offset 'statement-case-intro' ++)
;			(c-set-offset 'inextern-lang' 0)
;			)))



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

(setq compilation-scroll-output t); let buffer scroll to bottom

;; Open the error_log in compile mode so it's easier to jump
;; to files with failing tests.

(setq auto-mode-alist
      (cons '("/error_log$" . compilation-mode) auto-mode-alist))

;; Include error message patterns for Apache::Test and Test::More
(dolist (item
         '(("\\[ +error\\] error running tests (please examine \\(.+\\))" 1 1)
           ("# at (\\([^:]+\\):\\([0-9]+\\)) test" 1 2)
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

;(define-key svn-status-mode-map (kbd "M") 'compile)
;(define-key svn-status-mode-map (kbd "B") 'compile)

;--------------------------------------------------
;; grep over pffxg.sh output

(defvar pffxg-command "pffxg.sh ")
(defun pffxg (command-args)
  (interactive
   (progn
     (list (read-from-minibuffer "Find filenames and grep them (like this): "
				 pffxg-command nil nil
				 'grep-find-history))))
  (let ((null-device nil))		; see grep
    (grep command-args)))


;--------------------------------------------------
(defvar ml1-make-command "make -j")

;; XXX would love to launch gdb on Apache::Test segfault reports.
(defun ml1-make (command)
   "M-x ml-make: Compile and run the project's test suite,
   basically duplicating `compile' and adding a simple
   post-processing regexp to preserve the *nix shell
   semantics of '\r'."

   (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "ML1 Make command: "
                                   (eval ml1-make-command) nil nil
                                   '(compile-history . 1)))
     (list (eval ml1-make-command))))

  (unless (equal command (eval ml1-make-command))
    (setq ml1-make-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*ml1-make-" (downcase mmode) "*" ))))
    (compilation-start (concat "unset JAVA_HOME ANDROID_JAVA_HOME \
         ANDROID_PRE_BUILD_PATHS ANDROID_JAVA_TOOLCHAIN \
         ANDROID_HOST_OUT_TESTCASES ANDROID_TARGET_OUT_TESTCASES; \
cd ~/builds-ssd/magicleap/ML1; . build/envsetup.sh; lunch phaedra-userdebug; export USE_CCACHE=1 CCACHE_DIR=~/builds-ssd/.ccache-ML1; ccache -M 32G;" command " | perl -ple 's/^.*\\r//, s/.[\\b]//sg'"))))
;--------------------------------------------------
(defvar ml19-make-command "make -j")

;; XXX would love to launch gdb on Apache::Test segfault reports.
(defun ml19-make (command)
   "M-x ml-make: Compile and run the project's test suite,
   basically duplicating `compile' and adding a simple
   post-processing regexp to preserve the *nix shell
   semantics of '\r'."

   (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "ML19 Make command: "
                                   (eval ml19-make-command) nil nil
                                   '(compile-history . 1)))
     (list (eval ml19-make-command))))

  (unless (equal command (eval ml19-make-command))
    (setq ml19-make-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*ml19-make-" (downcase mmode) "*" ))))
    (compilation-start (concat "cd ~/builds-ssd/magicleap/ML19; . build/envsetup.sh; lunch phaedrax-userdebug; export USE_CCACHE=1 CCACHE_DIR=~/builds-ssd/.ccache-ML19; ccache -M 32G;" command " | perl -ple 's/^.*\\r//, s/.[\\b]//sg'"))))
;--------------------------------------------------
(defvar ml2-make-command "make -j")

;; XXX would love to launch gdb on Apache::Test segfault reports.
(defun ml2-make (command)
   "M-x ml-make: Compile and run the project's test suite,
   basically duplicating `compile' and adding a simple
   post-processing regexp to preserve the *nix shell
   semantics of '\r'."

   (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "ML2 Make command: "
                                   (eval ml2-make-command) nil nil
                                   '(compile-history . 1)))
     (list (eval ml2-make-command))))

  (unless (equal command (eval ml2-make-command))
    (setq ml2-make-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*ml2-make-" (downcase mmode) "*" ))))
    (compilation-start (concat "cd ~/builds-f2fs/magicleap/ML2; . build/envsetup.sh; lunch acamas-userdebug; export USE_CCACHE=1 CCACHE_DIR=~/builds-f2fs/.ccache-ML2; ccache -M 32G;" command " | perl -ple 's/^.*\\r//, s/.[\\b]//sg'"))))

;--------------------------------------------------
(defvar ml2-mma-command "mma -j")

;; XXX would love to launch gdb on Apache::Test segfault reports.
(defun ml2-mma (command)
   "M-x ml-make: Compile and run the project's test suite,
   basically duplicating `compile' and adding a simple
   post-processing regexp to preserve the *nix shell
   semantics of '\r'."

   (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "ML2 MMA command: "
                                   (eval ml2-mma-command) nil nil
                                   '(compile-history . 1)))
     (list (eval ml2-mma-command))))

  (unless (equal command (eval ml2-mma-command))
    (setq ml2-mma-command command))

  (save-some-buffers (not compilation-ask-about-save) nil)

  (let ((compilation-buffer-name-function
         '(lambda (mmode) (concat "*ml2-mma-" (downcase mmode) "*" ))))
    (compilation-start (concat "cd ~/builds-f2fs/magicleap/ML2; . build/envsetup.sh; lunch acamas-userdebug; export USE_CCACHE=1 CCACHE_DIR=~/builds-f2fs/.ccache-ML2; ccache -M 32G; cd $OLDPWD;" command " | perl -ple 's/^.*\\r//, s/.[\\b]//sg'"))))

;(define-key svn-status-mode-map (kbd "T") 'test-compile)
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

;(define-key svn-status-mode-map (kbd "C") 'configure)

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

;(define-key svn-status-mode-map (kbd "I") 'install)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.27")
 '(flycheck-c/c++-clang-executable "clang-10")
 '(flycheck-clang-analyzer-executable "clang-10")
 '(lsp-enable-file-watchers nil)
 '(lsp-file-watch-threshold nil)
 '(message-send-mail-partially-limit 100000000)
 '(message-sendmail-f-is-evil t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (color-theme flycheck-clangcheck dired-git-info dap-mode lsp-treemacs helm-lsp company-lsp lsp-ui flycheck-clang-tidy ccls use-package flycheck-clang-analyzer lsp-mode))))

;;(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
;; '(mmm-default-submode-face ((t (:background "midnightblue")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(server-start)
(ignore-errors
  (require 'battery)
  (unless (string-match "battery N/A" (battery))
    (display-battery-mode)))

(put 'narrow-to-page 'disabled nil)
(require 'page-ext)

;; windows power-shell delete
(normal-erase-is-backspace-mode 1)

;(require 'zoom-frm)
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(defun transparency-90-hook (framenum)
  (set-frame-parameter framenum 'alpha 90))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(package-initialize)

(use-package lsp-mode
  :hook (c-mode-common . lsp)
  :commands lsp)
(require 'ccls)
(setq ccls-executable "/usr/local/bin/ccls")
;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;(use-package dap-mode)
;(use-package dap-gdb-lldb); to load the dap adapter for your language
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))
(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))
(require 'dired-git-info)
(add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable)
(add-hook 'after-make-frame-functions 'transparency-90-hook)
(setq ccls-initialization-options
      '(;:compilationDatabaseDirectory "out"
        ;:cache (:directory "out/.ccls-cache")
        :include (:maxPathSize 10000 :maxNum 1000000)
        :workspaceSymbol (:maxNum 1000000)
        :xref (:maxNum 2000000)
        ))
