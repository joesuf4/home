;;; cwebm.el --- CWEB/WEB modified mode
;;
;; This file is replacement for the cweb.el from the Levi' CWEB.
;;
;; Copyright (C) 1987,1990,1993,2000 Silvio Levy and Donald E. Knuth
;; Copyright (c) 2002,2003,2004,2005 Max Vasin
;;
;; Permission is granted to make and distribute verbatim copies of
;; this document provided that the copyright notice and this
;; permission notice are preserved on all copies.
;;
;; Permission is granted to copy and distribute modified versions of
;; this document under the conditions for verbatim copying, provided
;; that the entire resulting derived work is given a different name
;; and distributed under the terms of a permission notice identical to
;; this one.

;;; Commentary:
;; 

;; Part 0: Differences from cweb.el:
;;
;; Key bindings:
;;   1. C-\ --> C-cwy
;;   2. M-\ --> C-cws
;;   3. C-z --> C-cwr
;; The first and second modifications are important for me because:
;;   1. C-\ switches keyboard layout (on my Linux box English <--> Russian).
;;   2. I've used to use M-\.
;; The third modification was made to unify key bindings.
;;
;; Base mode modified form plain-TeX to latex, since I use cweb-latex.
;;
;; Base keybings are now taken from the latex mode, instead of creaing
;; them from scratch.
;;
;; Key bindings for commands operating on module names pending list
;; made local.
;;
;; `TeX-validate-paragraph' function is now beeing defined in this package,
;; since it is defined in the file auc-old.el, which is provided for
;; compatability with AUC-TeX 6.x. `TeX-validate-paragraph' renamed to
;; `cwebm-validate-paragraph'.
;;
;; Package was renamed to cwebm.el
;; Suffix `cwebm-' added to function names, `TeX-' suffix chaged to `cwebm-'.
;;
;; Added customisation support.

;; Part (1): some WEB-oriented functions whose main purpose is
;; to maintain a stack of module names that are "pending" as you are writing
;; a program. When you first think of a module that needs to be written later,
;; put it into the pending list (by typing CTL-Z instead of @> after the
;; name). Later you can say CTL-\ to retrieve a pending name (and if
;; you want to cycle through the pending names, ESC-y after CTL-\ will
;; do it, just as ESC-y works after a yank).
;; After you've said CTL-\, the current region is the name just removed from
;; the pending list. If you change your mind, you can put it back again by
;; saying ESC-\. If you had put it into the pending list by mistake, you
;; can get rid of it by using the normal CTL-W operation (kill-region).
;; The following code binds the new commands to CTL-Z, CTL-\, and ESC-\
;; in all modes. You may prefer other bindings, of course.
;; CTL-Z is normally "suspend emacs", but it is best not used when emacs
;; has its own window as it usually does nowadays; if you need the
;; old CTL-Z, you might rather bind it to CTL-X CTL-Z.
;; CTL-\ is normally undefined.
;; ESC-\ is normally "delete space", but ESC-space DEL does that easily too.


;;; History:
;;  2005-09-26 Peter Galbraith <psg@debian.org>
;;   - Made almost checkdoc clean.

;;; Code:
;;(require 'tex)

(defgroup CWEBm nil
  "Major mode for editing CWEB and WEB programs"
  :prefix "cwebm-"
  :group 'languages)

(defcustom cwebm-validate-paragraph nil
  "Specifies whether to validate paragraph when you leave it."
  :type 'boolean
  :group 'CWEBm)

(defcustom cwebm-base-mode "latex-mode"
  "Specifies base mode for CWEBm."
  :type 'string
  :group 'CWEBm)

(defcustom cwebm-new-module-name-key-binding "\\C-cwr"
  "Specifies key binding for adding new module name into pending list."
  :type 'string
  :group 'CWEBm)

(defcustom cwebm-new-module-name-key-binding "\\C-cwr"
  "Specifies key binding for adding new module name into pending list."
  :type 'string
  :group 'CWEBm)

(defcustom cwebm-pop-module-name-key-binding "\\C-cwy"
  "Specifies key binding to remove first element from pending list.
Then insert it as current region."
  :type 'string
  :group 'CWEBm)

(defcustom cwebm-into-pending-list-key-binding "\\C-cws"
  "Specifies key binding to copy region into pending-list."
  :type 'string
  :group 'CWEBm)

(defcustom cwebm-into-pending-list-key-binding "\\C-cws"
  "Specifies key binding to copy region into pending-list."
  :type 'string
  :group 'CWEBm)

(defcustom cwebm-ditto-key-binding "\\M-\\\""
  "Specifies key binding to copy characters from the line above."
  :type 'string
  :group 'CWEBm)

(defun cwebm-process-key-binding (string)
  "Process a STRING representation of key binding."
  (read (concat "\"" string "\"")))

(defvar cwebm-pending-list nil
 "List of strings (usually WEB module names) still pending.")

(defun cwebm-into-pending-list (beg end)
 "Copy region between BEG and END into cwebm-pending-list."
 (interactive "r")
 (cwebm-indicate-region)
 (setq cwebm-pending-list (cons (buffer-substring beg end) cwebm-pending-list)))

(defun cwebm-new-module-name-pending ()
 "Insert @> to complete a module name, then put it into cwebm-pending-list."
 (interactive)
 (insert "@>")
 (push-mark)
 (if (search-backward "@<" nil t)
     (progn
       (exchange-point-and-mark)
       (cwebm-into-pending-list (point) (mark)))
   (message "There's no @< to begin the module name!")))

(defun cwebm-pop-pending-list (arg)
 "Remove first element of cwebm-pending-list and insert it as current region.
With argument ARG, put point at left; otherwise point will follow
the insertion.
Say \\[cwebm-new-yank-pop] to replace this by another element of the list.
Say \\[cwebm-into-pending-list] to put it back in the list."
 (interactive "*P")
 (if (consp cwebm-pending-list)
     (progn
       (push-mark (point))
       (insert (car cwebm-pending-list))
       (setq cwebm-pending-list (cdr cwebm-pending-list))
       (if arg
           (exchange-point-and-mark)))
   (message "Nothing is pending.")
   (setq this-command nil)))

(defun cwebm-new-yank-pop (arg)
 "If previous command was \\[cwebm-pop-pending-list], pop a different string;
otherwise do an ordinary Meta-y."
 (interactive "*p")
 (if (eq last-command 'cwebm-pop-pending-list)
     (let (xch)
       (setq xch (< (point) (mark)))
       (setq cwebm-pending-list (append cwebm-pending-list
                                 (list (buffer-substring (point) (mark)))))
       (delete-region (point) (mark))
       (setq this-command 'cwebm-pop-pending-list)
       (cwebm-pop-pending-list xch))
   (yank-pop arg)))
(global-set-key "\M-y" 'cwebm-new-yank-pop)

(defun cwebm-indicate-region ()
  "Bounce cursor to mark and back again."
  (let ((point-save (point)))
    (unwind-protect
        (progn (goto-char (mark))
               (sit-for 0 300)) ;; wait 300 milliseconds
      (goto-char point-save))))

; I prefer to change the standard copy-region command to the following,
; which gives me visual feedback about what I've copied to the kill ring:
(defun cwebm-indicate-and-copy-region (beg end)
  "Indicate current region, then copy it to the kill ring."
  (interactive "r")
  (cwebm-indicate-region)
  (copy-region-as-kill beg end))
(global-set-key "\M-w" 'cwebm-indicate-and-copy-region)

; Here's another convenient command, bound to the usually unused ESC-".
(defun cwebm-ditto (arg)
  "Copy ARG characters from the line above."
  (interactive "*p")
  (let (ch)
    (while (> arg 0)
      (setq temporary-goal-column (current-column))
      (save-excursion
        (previous-line 1)
        (setq ch (following-char)))
      (insert ch)
      (setq arg (1- arg)))))
(global-set-key (cwebm-process-key-binding cwebm-ditto-key-binding) 'cwebm-ditto)

;; OK, here's part (2): Changes to TeX mode.
; The WEB modes below are very much like TeX mode, but some improvements were
; desirable in TeX mode:
; I made newline act as it does in indented-text mode, since this
; works nicely for both TeX and WEB (Pascal or C code).
; I made RET check for unmatched delimiters if it ends a paragraph.
; Otherwise TeX mode remains as it was before.

(eval-after-load "tex"
  '(progn
     (define-key TeX-mode-map "\C-c\C-k" 'TeX-kill-job)
     (define-key TeX-mode-map "\C-c\C-l" 'TeX-recenter-output-buffer)
     (define-key TeX-mode-map "\C-c\C-p" 'TeX-print)
     (define-key TeX-mode-map "\e}" 'up-list)
     (define-key TeX-mode-map "\e{" 'TeX-insert-braces)
     (define-key TeX-mode-map "\C-c\C-r" 'TeX-region)
     (define-key TeX-mode-map "\C-c\C-b" 'TeX-buffer)
     (define-key TeX-mode-map "\r" 'cwebm-newline)
     (define-key TeX-mode-map "\t" 'LaTeX-indent-line)
     (setq TeX-mode-hook
           '(lambda ()
              (make-local-variable 'indent-line-function)
              (setq indent-line-function 'indent-relative-maybe)))))
     
(defun cwebm-newline (arg)
  "Handle a newline in cwebm mode.
If previous character is newline and no ARG, check for unbalanced braces
and/or dollar signs in previous paragraph.  If ARG is \\[universal-argument],
do a single newline; otherwise do ordinary newline."
 (interactive "*P")
 (if (and (eq (preceding-char) ?\n) (not arg) cwebm-validate-paragraph)
     (cwebm-check-paragraph)
   (if (listp arg)
       (newline)
     (newline arg))))

(defun cwebm-validate-paragraph (start end)
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (forward-sexp (- end start))
	  t))
    (error nil)))

(defun cwebm-check-paragraph ()
"Insert a newline following a newline, breaking a paragraph for TeX.
Check for mismatched delimiters in paragraph being terminated."
  (interactive)
  (if (cwebm-validate-paragraph
           (save-excursion
             (search-backward "\n\n" nil 'move)
             (point))
           (point))
      (insert ?\n)
    (insert ?\n)
    (error "Mismatched delimiters in that paragraph?")))

;; and now, part (3): WEB and CWEB modes.
; These are like plain TeX mode except that the automatic conversion of
; " to `` or '' is disabled. (Personally I never liked that feature anyway,
; since it's easy to get used to typing `` and ''. In WEB modes, the
; feature soon becomes intolerable, unless you never use string constants!)
; Another thing distinguishing WEB mode from TeX is ESC-p and ESC-n, to
; move to previous or next module. These keys are usually unbound, except
; when processing email.

(defun cwebm-forward-module (arg)
"Advance past next WEB module beginning; with ARG, repeat ARG times."
 (interactive "p")
 (cwebm-move-to-module arg))
(defun cwebm-backward-module (arg)
"Advance to previous WEB module beginning; with ARG, repeat ARG times."
 (interactive "p")
 (cwebm-move-to-module (- arg)))
(defun cwebm-move-to-module (arg)
 (while (> arg 0)
   (re-search-forward "@ \\|@\\*\\|@\n")
   (setq arg (1- arg)))
 (while (< arg 0)
   (re-search-backward "@ \\|@\\*\\|@\n")
   (setq arg (1+ arg))))

(defun cwebm-make-common-settings ()
  "Make settings common for both WEBm and CWEBm modes."
  ;; house-keeping first
  (kill-all-local-variables)

  ;; set base mode
  (funcall (intern cwebm-base-mode))

  ;; install local keybindings
  (local-set-key (cwebm-process-key-binding cwebm-new-module-name-key-binding) 'cwebm-new-module-name-pending)
  (local-set-key (cwebm-process-key-binding cwebm-pop-module-name-key-binding) 'cwebm-pop-pending-list)
  (local-set-key (cwebm-process-key-binding cwebm-into-pending-list-key-binding) 'cwebm-into-pending-list)
  (local-set-key "\M-n" 'cwebm-forward-module)
  (local-set-key "\M-p" 'cwebm-backward-module)
  (local-set-key "\"" 'self-insert-command)

  ;; set comments start and end
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-end ""))

;;;###autoload
(defun webm-mode ()
  "Major mode like TeX mode plus \\[forward-module] and \\[backward-module].
Used for relative module movement. The automatic \" feature is disabled."
  (interactive)

  ;; make settings common for both CWEBm and WEBm modes
  (cwebm-make-common-settings)

  ;; let emacs know who we are
  (setq mode-name "WEBm")
  (setq major-mode 'web-mode)

  ;; run a hook if it exists
  (run-hooks 'web-mode-hook))
(setq auto-mode-alist (cons '("\\.web$" . web-mode) auto-mode-alist))

;;;###autoload
(defun cwebm-mode ()
  "Major mode like LaTeX mode plus \\[forward-module] and \\[backward-module].
Used for relative module movement. The automatic \" feature is disabled."
  (interactive)

  ;; make settings common for both CWEBm and WEBm modes
  (cwebm-make-common-settings)

  ;; let emacs know who we are
  (setq mode-name "CWEBm")
  (setq major-mode 'cwebm-mode)

  ;; run a hook if it exists
  (run-hooks 'cwebm-mode-hook))

;;;###autoload(setq auto-mode-alist (cons '("\\.w$" . cwebm-mode) auto-mode-alist))
;;;###autoload(setq auto-mode-alist (cons '("\\.ch$" . cwebm-mode) auto-mode-alist))

(provide 'cwebm)

;;; cwebm.el ends here
