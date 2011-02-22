;; mc-remail2.el --- Type2/3 Remailer support for Mailcrypt

;; Copyright (C) 2003 Brian Warner <warner-mailcrypt@lothar.com>

;;{{{ Licensing

;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{ Load required packages

(require 'mailcrypt)

(eval-and-compile
  (autoload 'mc-find-headers-end "mc-toplev")
)

;;}}}

(defvar mc-mixmaster29-path "mixmaster"
  "*The mixmaster-2.9 executable. This uses a different API than the old
mix-2.0.3 'mixmaster' binary. If you use the old one, set mc-mixmaster-path
instead.")
(defvar mc-mixmaster29-ask t)

(defun mc-mixmaster-send (recipients start end &optional verbose)
  "Send the message in the current buffer via mixmaster. The buffer itself
  will be emptied by this process."

  (let (args ret recipient)

    ;; identify the (single) recipient. Multiple recipients are not allowed
    ;; mc-get-fields removes them
;    (setq recipients (mc-get-fields "To" main-header t))
    (if (> (length recipients) 1)
        (error "mixmaster only allows a single recipient"))
;    (setq recipient (mc-strip-address (cdr (car recipients))))
    (setq recipient (car recipients))

    ;; TODO: mixmaster allows multiple addresses. Need to find out if they
    ;; are comma-separated or if multiple '-t' options are used.

    ;; todo (maybe): mixmaster has support for posting to newsgroups

    ;; TODO: extract headers and pass to mixmaster though argv
    ;; --subject, --header 'Foo: text'

    ;; TODO: --nym uses a pre-configured nym (involves reply blocks)

    (setq args (list (format "--to=%s" recipient)
                     ;(format "--subject=%s" subject)
                     ))

    (setq command (format "%s %s" mc-mixmaster29-path args))

    (if mc-mixmaster29-ask
        (unless (y-or-n-p (format (concat
                                   "About to feed message through '%s'\n"
                                   "Really send this message? ")
                                  command))
          (error "Remailing aborted. See documentation for details.")))

    (goto-char start)

    (insert "Running mixmaster as follows:\n")
    (insert (format " %s\n\n" command))
    (insert "--- Command output begins ---\n")

    (setq start (point))

    (setq ret
          (apply 'call-process-region start end mc-mixmaster29-path t t t
                 args))
    (if (not (eq ret 0))
        (error "Mixmaster failed, rc %s" ret))

    (insert "--- Command output ends ---\n")
    (insert "\n\n")

    (insert (format "Message sent to '%s' through the Mixmaster network\n"
                    recipient))
    (insert "This buffer may now be deleted.\n")

    (message "Message sent through Mixmaster remailer network.")
))

(defun mc-remailer-scheme-mixmaster ()
  (list
   (cons 'encryption-func   'mc-mixmaster-send)
))



(defvar mc-mixminion-path "mixminion"
  "*The mixminion script")
(defvar mc-mixminion-ask t)

(defun mc-mixminion-send (recipients start end &optional verbose)
  "Send the message in the current buffer via mixminion. The buffer itself
  will be emptied by this process."

  (let (;(headers-end (mc-find-headers-end))
        ;(main-header (mc-find-main-header))
        args ret recipient)

    ;; identify the (single) recipient. Multiple recipients are not allowed
    ;; mc-get-fields removes them
;    (setq recipients (mc-get-fields "To" main-header t))
    (if (> (length recipients) 1)
        (error "mixminion only allows a single recipient"))
;    (setq recipient (mc-strip-address (cdr (car recipients))))
    (setq recipient (car recipients))

    ;; TODO: extract subject, pseudonym 'from', possibly In-Reply-To and
    ;; References headers, and pass to mixminion though argv

    (setq args (list "send"
                 "--to" recipient
                 ))

    (setq command (format "%s %s" mc-mixminion-path args))
    ;; formats weirdly, but good enough for now

    (if mc-mixminion-ask
        (unless (y-or-n-p (format (concat
                                   "About to feed message through '%s'\n"
                                   "Really send this message? ")
                                  command))
          (error "Remailing aborted. See documentation for details.")))

    (goto-char start)

    (insert "Running mixminion as follows:\n")
    (insert (format " %s\n\n" command))
    (insert "--- Command output begins ---\n")

    (setq start (point))

    (setq ret
          (apply 
           'call-process-region start end mc-mixminion-path
                               t ;; delete text that was sent
                               t ;; insert mixminion output in its place
                               t ;; refresh screen as output arrives
                               args
                               ))
    (if (not (eq ret 0))
        (error "Mixminion failed, rc %s" ret))

    (insert "--- Command output ends ---\n")
    (insert "\n\n")

    (insert (format "Message sent to '%s' through the Mixminion network\n"
                    recipient))
    (insert "This buffer may now be deleted.\n")

    (message "Message sent through Mixminion remailer network.")
))

(defun mc-remailer-scheme-mixminion ()
  (list
   (cons 'encryption-func   'mc-mixminion-send)
))
