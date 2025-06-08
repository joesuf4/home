; startup config file

(package-initialize)
(setq exec-path (append exec-path '("/usr/local/bin")))
(defun transparency-95-hook (framenum)
  (set-frame-parameter framenum 'alpha-background 95))

(setq color-theme-is-global nil)
(add-hook 'after-make-frame-functions 'transparency-95-hook)

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "n")
   (set-frame-parameter (selected-frame) 'alpha-background value))

(defun clear-background ()
  (interactive)
  (set-background-color ()))

  ;;--------------------------------------------------
;; set up unicode (bulletproof, from a different era)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;--------------------------------------------------
; hygenics (backup and autosave files)

(defun make-private-directory (directory-list)
  (let ((umask (default-file-modes)))
    (unwind-protect
        (progn
          (set-default-file-modes ?\700)
          (dolist (dir directory-list)
            (make-directory dir t)))
      (set-default-file-modes umask))))

(defvar autosave-dir (concat user-emacs-directory "autosaves/"))
(defvar backup-dir (concat user-emacs-directory "backups/"))

(make-private-directory (list autosave-dir backup-dir))

;; Put autosave files (ie #foo#) in one place.
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq auto-save-list-file-prefix autosave-dir)

;; lockfiles are an annoying mess on single-user systems
(setq create-lockfiles nil)

;; Put backup files (ie foo~) in one place too.
(setq backup-directory-alist (list (cons "." backup-dir)))

;--------------------------------------------------
; tramp hygenics

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory autosave-dir)
(setq tramp-backup-directory-alist (list (cons "." backup-dir)))

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

;;--------------------------------------------------
;; Now some whitespace-stuff...

(setq c-tab-always-indent nil) ; allows normal <TAB> behavior in the middle of a line
(setq show-trailing-whitespace t)

;;--------------------------------------------------
;; Perl -> cperl mode

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
             (cperl-set-style "CPerl")
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


;--------------------------------------------------
;; grep-buffer on pffxg.sh runs

(defvar pffxg-command "bash pffxg.sh ")
(defun pffxg (command-args)
  (interactive
   (progn
     (list (read-from-minibuffer "Find files (by name) and grep them: "
				 pffxg-command nil nil
				 'grep-find-history))))
  (let ((null-device nil))
    (grep command-args)))

;;--------------------------------------------------
;; Kevin Rogers' compile-input.el
;; http://groups.google.com/groups?selm=3F6A3405.8040609%40yahoo.com

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
;; Emacs' built-in customization engine sends its output here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function 'browse-url-chrome)
 '(browse-url-chrome-program "chrome.exe")
 '(custom-safe-themes
   '("09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93"
     "103b4cc7fb9063c614f0413828ae7c223dba14b8ce987746b6416de05d99dacf"
     "e7c33a10a4494a4f3ca8acfdf174cd83e91b4cb401246d78f406aec0d960f847"
     "70755427a482cf762b032172eda8e042e2aca58e9cb9542f251067142aacfa26"
     "011e5acf8327c380a9bae9e4281ec8509e3d1139ed136672635467e3c4ca09c7"
     "56e7eb6ba5dbe81bad46f22f6eb74682caf69dc652e57f1538ea52de93d44289" default))
 '(dired-use-ls-dired nil)
 '(diredfl-global-mode t nil (diredfl))
 '(exec-suffixes '(".exe" ".com" ".bat" ".cmd" ".btm" ".ps1" ""))
 '(flycheck-c/c++-clang-executable "clang")
 '(flycheck-clang-analyzer-executable "clang")
 '(lsp-csharp-server-path "~/bin/OmniSharp.bat")
 '(lsp-enable-file-watchers nil)
 '(lsp-file-watch-threshold nil)
 '(lsp-log-io t)
 '(markdown-command "marked-mermaid.js")
 '(message-send-mail-partially-limit 100000000)
 '(message-sendmail-f-is-evil t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")))
 '(package-selected-packages
   '(## auto-complete-clang auto-complete-clang-async auto-complete-distel avy-flycheck bpftrace-mode
        ccls color-theme-modern company-lsp csharp-mode dap-mode dired-git-info diredfl
        dockerfile-mode dtrace-script-mode editorconfig flycheck-clang-analyzer flycheck-clang-tidy
        flycheck-clangcheck flycheck-rust flymake flymake-hadolint flymake-yamllint forge ghub+
        git-commit-ts-mode go-mode helm-lsp kubectx-mode kubed lsp-docker lsp-jedi lsp-mode
        lsp-python-ms lsp-treemacs lsp-ui markdown-preview-mode mermaid-mode poly-ansible popup
        rust-mode shfmt terraform-doc terraform-mode use-package which-key yasnippet zenburn-theme))
 '(sh-basic-offset 2)
 '(shfmt-arguments '("-i" "2" "-ci"))
 '(shfmt-command "shfmt")
 '(visible-bell t)
 '(warning-suppress-log-types '(((unlock-file)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Cascadia Code NF Regular")))))

;;--------------------------------------------------
;; packaged LSP stuff

(use-package lsp-mode
  :ensure t
  :hook (c-mode-common . lsp)
  :custom
  (lsp-enable-snippet t)
  (lsp-keep-workspace-alive t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-enable-completion-at-point nil)

  :config
  (add-hook 'csharp-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'html-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'json-mode-hook #'lsp)
  (add-hook 'shell-mode-hook #'lsp)
  (add-hook 'css-mode-hook #'lsp)


  (setq company-minimum-prefix-length 1
        company-idle-delay 0.500) ;; default is 0.2
  :commands lsp)

;(add-to-list 'lsp-language-id-configuration '(asy-mode . "asymptote"))
;(lsp-register-client
; (make-lsp-client :new-connection (lsp-stdio-connection '("asy" "-lsp"))
;                  :activation-fn (lsp-activate-on "asymptote")
;                  :major-modes '(asy-mode)
;                  :server-id 'asyls))
(add-to-list 'auto-mode-alist '("\\.asy\\'" . asy-mode))
(add-hook 'prog-mode-hook #'lsp)
(global-auto-complete-mode t)
(which-key-mode)
;(helm-mode)
;(require 'helm-xref)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package flymake)
(use-package flymake-yamllint)
(use-package flymake-hadolint)
(use-package dap-mode)
(use-package dap-lldb)

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;--------------------------------------------------
;; modes for Brendan Gregg's brainchildren...
(require 'bpftrace-mode)
(require 'dtrace-script-mode)
(add-to-list 'auto-mode-alist '("\\.d\\'" . dtrace-script-mode))

(require 'shfmt)
;(add-hook 'sh-mode-hook 'shfmt-on-save-mode)
(add-hook 'yaml-mode-hook 'flymake-yamllint-setup)
(add-hook 'dockerfile-mode-hook 'flymake-hadolint-setup)
;;--------------------------------------------------
;; ccls: nice LSP app for emacs integration
(use-package ccls
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'ccls))
  (setq ccls-executable "ccls")
  (setq ccls-initialization-options
      '(;:compilationDatabaseDirectory "out"
        ;:cache (:directory "out/.ccls-cache")
        :include (:maxPathSize 10000 :maxNum 1000000)
        :workspaceSymbol (:maxNum 1000000)
        :xref (:maxNum 2000000)
        )))

;;--------------------------------------------------
;; dired-git-info mode - too lazy to deal with fancy git-mode packages
;(use-package forge)
(require 'dired-x)
(require 'diredfl)
(use-package dired-git-info
    :ensure t
    :bind (:map dired-mode-map
                (")" . dired-git-info-mode)
                ("M-s s" . forge-status)))

;;-perlcritic
;;(require 'flymake-perlcritic)

;;--------------------------------------------------
;; UI (frame) look and feel

(display-time) ; prints load avg too, which is more relevant than clock

(ignore-errors
  (tool-bar-mode 0)
  (menu-bar-mode 0))


(put 'narrow-to-page 'disabled nil)
(require 'page-ext)

;; windows power-shell delete
;(normal-erase-is-backspace-mode 1)

(global-font-lock-mode t)

(column-number-mode t)

; enable auto-fill-mode (electic cutoff+reindent of long lines)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'shell-script-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; markdown-preview
(require 'markdown-preview-mode)

(setq markdown-preview-stylesheets (list
				    "http://thomasf.github.io/solarized-css/solarized-light.min.css"
                                    "https://www.sunstarsys.com/editor.md/lib/codemirror/codemirror.min.css"
                                    "https://www.sunstarsys.com/editor.md/lib/codemirror/theme/solarized.css"
                                    "https://www.sunstarsys.com/css/mermaid.min.css"))

; still using 80-column terminals (at times)
(setq-default fill-column 100)

(add-hook 'markdown-mode-hook
          '(lambda ()
             (setq fill-column 'nil)))

(add-hook 'c-mode-common-hook
	  '(lambda ()
             (add-to-list 'c-offsets-alist '(substatement-open . 0))
             (c-set-style "stroustrup")
             (setq c-basic-offset 2)
	     ))

(global-set-key (kbd "C-l") 'redraw-display)
(setq transient-mark-mode t) ; no idea what this does any more

(setq outline-minor-mode-prefix "\C-c\C-o") ; outlining rebind
(setq tab-stop-list '(2 4 8 12 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
(setq-default indent-tabs-mode nil)

(defun rgmacs/copy-theme-pgtk (from-theme to-theme)
  "Copies all faces from from-theme to to-theme but restricts to TTY frames only."
  (dolist (entry (get from-theme 'theme-settings))
    (when (eq (car entry) 'theme-face)
      (let ((face (nth 1 entry))
            (face-specs (nth 3 entry))
            (new-specs))
        (dolist (face-spec face-specs)
          (let ((display (car face-spec))
                (rest (cdr face-spec)))
            (cond
             ((listp display)
              (progn
                (setq display (cl-copy-seq display))
                (add-to-list 'display '(type pgtk x-toolkit x))))
             ((eq display t)
              (setq display '((type pgtk x-toolkit x)))))
            (add-to-list 'new-specs (append `(,display) rest))))
        (custom-theme-set-faces to-theme `(,face ,new-specs))))))


(load-theme 'zenburn t t)
(deftheme zenburn-pgtk)

(rgmacs/copy-theme-pgtk 'zenburn 'zenburn-pgtk)
(enable-theme 'zenburn-pgtk)

; WSL fu
(defun delete-if-file ()
  (if (not (file-symlink-p buffer-file-name))
      (if (not (file-executable-p buffer-file-name))
          (delete-file buffer-file-name)
    )))

;; (add-hook 'before-save-hook #'delete-if-file)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; make fun
(setq compile-command "make -kj ")

;; python fun
(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(add-to-list 'lsp-enabled-clients 'ts-ls)
;; terraform
(require 'terraform-mode)
(require 'terraform-doc)

;;kubectx
(require 'kubectx-mode)

;;gnus
(setq gnus-select-method '(nnml ""))
