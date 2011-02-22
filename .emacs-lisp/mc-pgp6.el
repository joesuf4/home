;; mc-pgp.el, PGP support for Mailcrypt
;; Copyright (C) 1995  Jin Choi <jin@atype.com>
;;                     Patrick LoPresti <patl@lcs.mit.edu>
;; Copyright (C) 2000  Derek Atkins <warlord@MIT.EDU>

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
(require 'mailcrypt)

(defvar mc-pgp65-user-id (user-login-name)
  "*PGP ID of your default identity.")
(defvar mc-pgp65-path "pgp6" "*The PGP executable.")
(defvar mc-pgp65-display-snarf-output nil
  "*If t, pop up the PGP output window when snarfing keys.")
(defvar mc-pgp65-always-fetch nil
  "*If t, always fetch missing keys. If nil, prompt user. If 'never,
never fetch keys, and don't ask.")
(defvar mc-pgp65-alternate-keyring nil
  "*Public keyring to use instead of default.")
(defvar mc-pgp65-comment
  (format "Processed by Mailcrypt %s, an Emacs/PGP interface" mc-version)
  "*Comment field to appear in ASCII armor output.  If nil, let PGP
use its default.")

(defconst mc-pgp65-msg-begin-line "^-----BEGIN PGP MESSAGE-----\r?$"
  "Text for start of PGP message delimiter.")
(defconst mc-pgp65-msg-end-line "^-----END PGP MESSAGE-----\r?$"
  "Text for end of PGP message delimiter.")
(defconst mc-pgp65-signed-begin-line "^-----BEGIN PGP SIGNED MESSAGE-----\r?$"
  "Text for start of PGP signed messages.")
(defconst mc-pgp65-signed-end-line "^-----END PGP SIGNATURE-----\r?$"
  "Text for end of PGP signed messages.")
(defconst mc-pgp65-key-begin-line "^-----BEGIN PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for start of PGP public key.")
(defconst mc-pgp65-key-end-line "^-----END PGP PUBLIC KEY BLOCK-----\r?$"
  "Text for end of PGP public key.")
(defconst mc-pgp65-error-re "^\\(ERROR:\\|WARNING:\\).*"
  "Regular expression matching an error from PGP")
(defconst mc-pgp65-sigok-re "^.*Good signature.*"
  "Regular expression matching a PGP signature validation message")
(defconst mc-pgp65-newkey-re "^\\([0-9]+\\) +matching keys? found"
  "Regular expression matching a PGP key snarf message")
(defconst mc-pgp65-nokey-re
  "Cannot find the public key matching userid '\\(.+\\)'$"
  "Regular expression matching a PGP missing-key messsage")
(defconst mc-pgp65-key-expected-re
  "Key matching expected Key ID \\(\\S +\\) not found")

(defvar mc-pgp65-key-cache nil
  "Association list mapping PGP IDs to canonical \"keys\".  A \"key\"
is a pair (USER-ID . KEY-ID) which identifies the canonical IDs of the
PGP ID.")

(defun mc-pgp65-lookup-key (str)
  ;; Look up the string STR in the user's secret key ring.  Return a
  ;; pair of strings (USER-ID . KEY-ID) which uniquely identifies the
  ;; matching key, or nil if no key matches.
  (if (equal str "***** CONVENTIONAL *****") nil
    (let ((result (cdr-safe (assoc str mc-pgp65-key-cache)))
	  (key-regexp
	   "^\\(\\(pub\\|sec\\)\\s +[^/]+/\\(\\S *\\)\\s +\\S +\\s +\\(.*\\)\\)$")
	  (revoke-regexp "REVOKED")
	  (dsk-regexp "*** DEFAULT SIGNING KEY ***")
	  (real-id-regexp "^ +\\(.*\\)$")
	  (obuf (current-buffer))
	  buffer key-start key-end)
      (if (null result)
	  (unwind-protect
	      (progn
		(setq buffer (generate-new-buffer " *mailcrypt temp"))
		(call-process mc-pgp65-path nil buffer nil
			      "+language=en" "+compat=on" "-kv" str)
		(set-buffer buffer)
		(goto-char (point-min))
		(while (and (null result)
			    (re-search-forward key-regexp nil t))
		    (progn
		      (setq result
			    (cons (buffer-substring-no-properties
				   (match-beginning 4) (match-end 4))
				  (concat
				   "0x"
				   (buffer-substring-no-properties
				    (match-beginning 3) (match-end 3)))))
		      (setq key-start (match-beginning 1))
		      (setq key-end (match-end 1))
		      (save-restriction
			    (narrow-to-region key-start key-end)
			    (goto-char (point-min))
			    (cond
			     ((re-search-forward revoke-regexp nil t)
;			      (message "Key Revoked")
			      (setq result nil))
			     ((re-search-forward dsk-regexp nil t)
;			      (message "DSK Detected")
			      (widen)
			      (if (re-search-forward real-id-regexp nil t)
				  (setcar result
					(buffer-substring-no-properties
					 (match-beginning 1) (match-end 1)))
				(mc-message "\aError: Invalid keyring. UserID not found"))
;			      (message (format "%s" result))
			      ))
			    
			    (if (not (null result))
				(setq mc-pgp65-key-cache 
				      (cons (cons str result)
					    mc-pgp65-key-cache)))))))
	    (if buffer (kill-buffer buffer))
	    (set-buffer obuf)))
      (if (null result) nil )             ; We don't mind a missing "secring"
      result)))

(defun mc-pgp65-generic-parser (result)
  (let (start)
    (goto-char (point-min))
    (cond ((not (eq result 0))
	   (prog1
	       nil
	     (if (mc-message "^\aError: +Bad pass phrase\\.$" (current-buffer))
		 (mc-deactivate-passwd t)
	       (mc-message mc-pgp65-error-re (current-buffer)
			   (format "PGP exited with status %d" result)))))
	  ((re-search-forward mc-pgp65-nokey-re nil t)
	   nil)
	  (t
	   (and
	    (goto-char (point-min))
	    (re-search-forward "-----BEGIN PGP.*-----$" nil t)
	    (setq start (match-beginning 0))
	    (goto-char (point-max))
	    (re-search-backward "^-----END PGP.*-----\n" nil t)
	    (cons start (match-end 0)))))))

(defun mc-pgp65-encrypt-region (recipients start end &optional id sign)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	;; Crock.  Rewrite someday.
	(mc-pgp-always-sign mc-pgp-always-sign)
	(obuf (current-buffer))
	action msg args key passwd result pgp-id)
    (setq args (list "+encrypttoself=off +verbose=1" "+batchmode"
		     "+language=en" "-fat"))
    (setq action (if recipients "Encrypting" "Armoring"))
    (setq msg (format "%s..." action))  ; May get overridden below
    (if recipients (setq args (append (list "-e") args (list "+force"))))
    (if mc-pgp65-comment
	(setq args (cons (format "+comment=%s" mc-pgp65-comment) args)))
    (if mc-pgp65-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp65-alternate-keyring)))))
    (if (and (not (eq mc-pgp-always-sign 'never))
	     (or mc-pgp-always-sign sign (y-or-n-p "Sign the message? ")))
	(progn
	  (setq mc-pgp-always-sign t)
	  (setq key (mc-pgp65-lookup-key (or id mc-pgp65-user-id)))
	  (if (not key) (error "No key available for signing."))
	  (setq passwd
		(mc-activate-passwd
		 (cdr key)
		 (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
	  (setq args
		(nconc args (list "-s" "-u" (cdr key))))
	  (setenv "PGPPASSFD" "0")
	  (setq msg (format "%s+signing as %s ..." action (car key))))
      (setq mc-pgp-always-sign 'never))

    (or key
	(setq key (mc-pgp65-lookup-key mc-pgp65-user-id)))

    (if (and recipients mc-encrypt-for-me)
	(setq recipients (cons (cdr key) recipients)))

    (setq args (append args recipients))
    
    (message "%s" msg)
    (setq result (mc-process-region start end passwd mc-pgp65-path args
				    'mc-pgp65-generic-parser buffer))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (re-search-forward mc-pgp65-nokey-re nil t)
	  (progn
	    (if result (error "This should never happen."))
	    (setq pgp-id (buffer-substring-no-properties
			  (match-beginning 1) (match-end 1)))
	    (if (and (not (eq mc-pgp65-always-fetch 'never))
		     (or mc-pgp65-always-fetch
			 (y-or-n-p
			  (format "Key for '%s' not found; try to fetch? "
				  pgp-id))))
		(progn
		  (mc-pgp65-fetch-key (cons pgp-id nil))
		  (set-buffer obuf)
		  (mc-pgp65-encrypt-region recipients start end id))
	      (mc-message mc-pgp65-nokey-re buffer)
	      nil))
	(if (not result)
	    nil
	  (message "%s Done." msg)
	  t)))))

(defun mc-pgp65-decrypt-parser (result)
  (goto-char (point-min))
  (cond ((eq result 0)
	 ;; Valid signature
	 (re-search-forward "^Signature made.*\n")
	 (if (looking-at
	      "\a\nWARNING:  Because this public key.*\n.*\n.*\n")
	     (goto-char (match-end 0)))
	 (cons (point) (point-max)))
	((eq result 1)
	 (re-search-forward
	  "\\(\\(^File is conven.*\\)?Just a moment\\.+\\)\\|\\(^\\.\\)")
	 (if (eq (match-beginning 2) (match-end 2))
	     (if (looking-at
		  "\nFile has signature.*\\(\n\a.*\n\\)*\nWARNING:.*\n")
		 (goto-char (match-end 0)))
	   (if (looking-at "Pass phrase appears good\\. \\.")
	       (goto-char (match-end 0))))
	 (cons (point) (point-max)))
	(t nil)))

(defun mc-pgp65-decrypt-region (start end &optional id)
  ;; returns a pair (SUCCEEDED . VERIFIED) where SUCCEEDED is t if
  ;; the decryption succeeded and verified is t if there was a valid signature
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	args key new-key passwd result pgp-id)
    (undo-boundary)
    (setq key (mc-pgp65-lookup-key (or id mc-pgp65-user-id)))
;    (if (not key) (error "No key available for decrypting."))
    (setq
     passwd
     (if key
	 (mc-activate-passwd (cdr key)
			     (and id
				  (format "PGP passphrase for %s (%s): "
					  (car key) (cdr key))))
       (mc-activate-passwd id "PGP passphrase for conventional decryption: ")))
    (if passwd
	(setenv "PGPPASSFD" "0"))
    (setq args '("+verbose=1" "+batchmode" "+language=en" "-f"))
    (if mc-pgp65-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp65-alternate-keyring)))))
    (message "Decrypting...")
    (setq result
	  (mc-process-region
	   start end passwd mc-pgp65-path args 'mc-pgp65-decrypt-parser buffer))
    (cond
     (result
      (message "Decrypting... Done.")
      ;; If verification failed due to missing key, offer to fetch it.
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(if (re-search-forward mc-pgp65-key-expected-re nil t)
	    (setq pgp-id (concat "0x" (buffer-substring-no-properties
				       (match-beginning 1)
				       (match-end 1))))))
      (if (and pgp-id
	       (not (eq mc-pgp65-always-fetch 'never))
	       (or mc-pgp65-always-fetch
		   (y-or-n-p
		    (format "Key %s not found; attempt to fetch? " pgp-id)))
	       (mc-pgp65-fetch-key (cons nil pgp-id)))
	  (progn
	    (undo-start)
	    (undo-more 1)
	    (mc-pgp65-decrypt-region start end id))
	(mc-message mc-pgp65-key-expected-re buffer)
	(cons t (eq result 0))))
     ;; Decryption failed; maybe we need to use a different user-id
     ((save-excursion
	(and
	 (set-buffer buffer)
	 (goto-char (point-min))
	 (re-search-forward
	  "^Key for user ID:.*\n.*Key ID 0x\\([0-9A-F]+\\)" nil t)
	 (setq new-key
	       (mc-pgp65-lookup-key
		(concat "0x" (buffer-substring-no-properties
			      (match-beginning 1)
			      (match-end 1)))))
	 (not (and id (equal key new-key)))))
      (message (format "Decrypting with id %s" new-key))
      (mc-pgp65-decrypt-region start end (cdr new-key)))
     ;; Or maybe it is conventionally encrypted
     ((save-excursion
	(and
	 (set-buffer buffer)
	 (goto-char (point-min))
	 (re-search-forward "^File is conventionally encrypted" nil t)))
      (if (null key) (mc-deactivate-passwd t))
      (mc-pgp65-decrypt-region start end "***** CONVENTIONAL *****"))
     ;; Or maybe this is the wrong PGP version
     ((save-excursion
	(and
	 (set-buffer buffer)
	 (goto-char (point-min))
	 (re-search-forward "Unsupported packet format" nil t)))
      (mc-message mc-pgp65-error-re buffer "Not encrypted for PGP 2.6"))
     (t
      (mc-display-buffer buffer)
      (if (mc-message "^\aError: +Bad pass phrase\\.$" buffer)
	  (mc-deactivate-passwd t)
	(mc-message mc-pgp65-error-re buffer "Error decrypting buffer"))
      (cons nil nil)))))

(defun mc-pgp65-sign-region (start end &optional id unclear)
  (let ((process-environment process-environment)
	(buffer (get-buffer-create mc-buffer-name))
	passwd args key)
    (setq key (mc-pgp65-lookup-key (or id mc-pgp65-user-id)))
    (if (not key) (error "No key available for signing."))
    (setq passwd
	  (mc-activate-passwd
	   (cdr key)
	   (format "PGP passphrase for %s (%s): " (car key) (cdr key))))
    (setenv "PGPPASSFD" "0")
    (setq args
	  (list
	   "-fast" "+verbose=1" "+language=en"
	    (format "+clearsig=%s" (if unclear "off" "on"))
	    "+batchmode" "-u" (cdr key)))
    (if mc-pgp65-comment
	(setq args (cons (format "+comment=%s" mc-pgp65-comment) args)))
    (message "Signing as %s ..." (car key))
    (if (mc-process-region start end passwd mc-pgp65-path args
			   'mc-pgp65-generic-parser buffer)
	(progn
	  (message "Signing as %s ... Done." (car key))
	  t)
      nil)))

(defun mc-pgp65-verify-parser (result)
  (cond ((eq result 0)
	 (mc-message mc-pgp65-sigok-re (current-buffer) "Good signature")
	 t)
	((eq result 1)
	 (mc-message mc-pgp65-error-re (current-buffer) "Bad signature")
	 nil)
	(t
	 (mc-message mc-pgp65-error-re (current-buffer)
		     (format "PGP exited with status %d" result))
	 nil)))

(defun mc-pgp65-verify-region (start end &optional no-fetch)
  (let ((buffer (get-buffer-create mc-buffer-name))
	(obuf (current-buffer))
	args pgp-id)
    (setq args '("+verbose=1" "+batchmode" "+language=en" "-f"))
    (if mc-pgp65-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp65-alternate-keyring)))))
    (message "Verifying...")
    (if (mc-process-region
	 start end nil mc-pgp65-path args 'mc-pgp65-verify-parser buffer)
	t
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(if (and
	     (not no-fetch)
	     (re-search-forward mc-pgp65-key-expected-re nil t)
	     (setq pgp-id
		   (concat "0x" (buffer-substring-no-properties
				 (match-beginning 1)
				 (match-end 1))))
	     (not (eq mc-pgp65-always-fetch 'never))
	     (or mc-pgp65-always-fetch
		 (y-or-n-p
		  (format "Key %s not found; attempt to fetch? " pgp-id)))
	     (mc-pgp65-fetch-key (cons nil pgp-id))
	     (set-buffer obuf))
	    (mc-pgp65-verify-region start end t)
	  (mc-message mc-pgp65-error-re buffer)
	  nil)))))

(defun mc-pgp65-insert-public-key (&optional id)
  (let ((buffer (get-buffer-create mc-buffer-name))
	args)
    (setq id (or id mc-pgp65-user-id))
    (setq args (list "+verbose=1" "+batchmode" "+language=en" "-kxaf" id))
    (if mc-pgp65-comment
	(setq args (cons (format "+comment=%s" mc-pgp65-comment) args)))
    (if mc-pgp65-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp65-alternate-keyring)))))

    (if (mc-process-region (point) (point) nil mc-pgp65-path
			   args 'mc-pgp65-generic-parser buffer)
	(progn
	  (mc-message "Key for user ID: .*" buffer)
	  t))))

(defun mc-pgp65-snarf-parser (result)
  (eq result 0))

(defun mc-pgp65-snarf-keys (start end)
  ;; Returns number of keys found.
  (let ((buffer (get-buffer-create mc-buffer-name)) tmpstr args)
    (setq args '("+verbose=1" "+batchmode" "+language=en" "-kaf"))
    (if mc-pgp65-alternate-keyring
	(setq args (append args (list (format "+pubring=%s"
					      mc-pgp65-alternate-keyring)))))
    (message "Snarfing...")
    (if (mc-process-region start end nil mc-pgp65-path args
			   'mc-pgp65-snarf-parser buffer)
	(save-excursion
	  (set-buffer buffer)
	  (goto-char (point-min))
	  (if (re-search-forward mc-pgp65-newkey-re nil t)
	      (progn
		(if mc-pgp65-display-snarf-output (mc-display-buffer buffer))
		(setq tmpstr (buffer-substring-no-properties
			      (match-beginning 1) 
			      (match-end 1)))
		(if (equal tmpstr "No")
		    0
		  (car (read-from-string tmpstr))))))
      (mc-display-buffer buffer)
      (mc-message mc-pgp65-error-re buffer "Error snarfing PGP keys")
      0)))

(defun mc-scheme-pgp65 ()
  (list
   (cons 'encryption-func 		'mc-pgp65-encrypt-region)
   (cons 'decryption-func		'mc-pgp65-decrypt-region)
   (cons 'signing-func			'mc-pgp65-sign-region)
   (cons 'verification-func 		'mc-pgp65-verify-region)
   (cons 'key-insertion-func 		'mc-pgp65-insert-public-key)
   (cons 'snarf-func			'mc-pgp65-snarf-keys)
   (cons 'key-fetch-func                mc-pgp65-fetch-key)
   (cons 'msg-begin-line 		mc-pgp65-msg-begin-line)
   (cons 'msg-end-line 			mc-pgp65-msg-end-line)
   (cons 'signed-begin-line 		mc-pgp65-signed-begin-line)
   (cons 'signed-end-line 		mc-pgp65-signed-end-line)
   (cons 'key-begin-line 		mc-pgp65-key-begin-line)
   (cons 'key-end-line 			mc-pgp65-key-end-line)
   (cons 'user-id			mc-pgp65-user-id)))

;;{{{ Key fetching

(defvar mc-pgp65-always-fetch nil
  "*If t, always attempt to fetch missing keys, or never fetch if
'never.")

(defvar mc-pgp65-keyserver-url-template
  "/pks/lookup?op=get&search=%s"
  "The URL to pass to the keyserver.")

(defvar mc-pgp65-keyserver-address "pgp.ai.mit.edu"
  "Host name of keyserver.")

(defvar mc-pgp65-keyserver-port 11371
  "Port on which the keyserver's HTTP daemon lives.")

(defvar mc-pgp65-fetch-timeout 20
  "*Timeout, in seconds, for any particular key fetch operation.")

(defvar mc-pgp65-fetch-keyring-list nil
  "*List of strings which are filenames of public keyrings to search
when fetching keys.")

(defsubst mc-pgp65-buffer-get-key (buf)
  "Return the first key block in BUF as a string, or nil if none found."
  (save-excursion
    (let (start)
      (set-buffer buf)
      (goto-char (point-min))
      (and (re-search-forward mc-pgp65-key-begin-line nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward mc-pgp65-key-end-line nil t)
	   (buffer-substring-no-properties start (match-end 0))))))

(defun mc-pgp65-fetch-from-keyrings (id)
  (let ((keyring-list mc-pgp65-fetch-keyring-list)
	buf proc key)
    (unwind-protect
	(progn
	  (message "Fetching %s from keyrings..." (or (cdr id) (car id)))
	  (while (and (not key) keyring-list)
	    (setq buf (generate-new-buffer " *mailcrypt temp*"))
	    (setq proc
		  (start-process "*PGP*" buf mc-pgp65-path "-kxaf"
				 "+verbose=0" "+batchmode"
				 (format "+pubring=%s" (car keyring-list))
				 (or (cdr id) (car id))))
	    ;; Because PGPPASSFD might be set
	    (process-send-string proc "\r\n")
	    (while (eq 'run (process-status proc))
	      (accept-process-output proc 5))
	    (setq key (mc-pgp65-buffer-get-key buf))
	    (setq keyring-list (cdr keyring-list)))
	  key)
      (if buf (kill-buffer buf))
      (if (and proc (eq 'run (process-status proc)))
	  (interrupt-process proc)))))

(defun mc-pgp65-fetch-from-http (id)
  (let (buf connection)
    (unwind-protect
	(progn
	  (message "Fetching %s via HTTP to %s..."
		   (or (cdr id) (car id)) mc-pgp65-keyserver-address)
	  (setq buf (generate-new-buffer " *mailcrypt temp*"))
	  (setq connection
		(open-network-stream "*key fetch*" buf mc-pgp65-keyserver-address
				     mc-pgp65-keyserver-port))
	  (process-send-string
	   connection
	   (concat "GET " (format mc-pgp65-keyserver-url-template
				  (or (cdr id) (car id))) "\r\n"))
	  (while (and (eq 'open (process-status connection))
		      (accept-process-output connection mc-pgp65-fetch-timeout)))
	  (mc-pgp65-buffer-get-key buf))
      (if buf (kill-buffer buf))
      (if connection (delete-process connection)))))

(defun mc-pgp65-fetch-from-finger (id)
  (let (buf connection user host)
    (unwind-protect
	(and (car id)
	     (string-match "^\\(.+\\)@\\([^@]+\\)$" (car id))
	     (progn
	       (message "Trying finger %s..." (car id))
	       (setq user (substring (car id)
				     (match-beginning 1) (match-end 1)))
	       (setq host (substring (car id)
				     (match-beginning 2) (match-end 2)))
	       (setq buf (generate-new-buffer " *mailcrypt temp*"))
	       (condition-case nil
		   (progn
		     (setq connection
			   (open-network-stream "*key fetch*" buf host 79))
		     (process-send-string connection
					  (concat "/W " user "\r\n"))
		     (while
			 (and (eq 'open (process-status connection))
			      (accept-process-output connection
						     mc-pgp65-fetch-timeout)))
		     (mc-pgp65-buffer-get-key buf))
		 (error nil))))
      (if buf (kill-buffer buf))
      (if connection (delete-process connection)))))

(defvar mc-pgp65-fetch-methods '(mc-pgp65-fetch-from-keyrings
			       mc-pgp65-fetch-from-finger
			       mc-pgp65-fetch-from-http)
  "List of methods to try when attempting to fetch a key.  Each
element is a function to call with an ID as argument.  See the
documentation for the function mc-pgp65-fetch-key for a description of
the ID.")

(defun mc-pgp65-fetch-key (&optional id)
  "Attempt to fetch a key for addition to PGP keyring.  Interactively,
prompt for string matching key to fetch.

Non-interactively, ID must be a pair.  The CAR must be a bare Email
address and the CDR a keyID (with \"0x\" prefix).  Either, but not
both, may be nil.

Return t if we think we were successful; nil otherwise.  Note that nil
is not necessarily an error, since we may have merely fired off an Email
request for the key."
  (interactive)
  (let ((methods mc-pgp65-fetch-methods)
	(process-connection-type nil) key proc buf args)
    (if (null id)
	(setq id (cons (read-string "Fetch key for: ") nil)))
    (while (and (not key) methods)
      (setq key (funcall (car methods) id))
      (setq methods (cdr methods)))
    (if (not (stringp key))
	(progn
	  (message "Key not found.")
	  nil)
      ;; Maybe I'll do this right someday.
      (unwind-protect
	  (save-window-excursion
	    (setq buf (generate-new-buffer " *PGP Key Info*"))
	    (pop-to-buffer buf)
	    (if (< (window-height) (/ (frame-height) 2))
		(enlarge-window (- (/ (frame-height) 2)
				   (window-height))))
	    (setq args '("-f" "+verbose=0" "+batchmode"))
	    (if mc-pgp65-alternate-keyring
		(setq args
		      (append args (list (format "+pubring=%s"
						 mc-pgp65-alternate-keyring)))))

	    (setq proc (apply 'start-process "*PGP*" buf mc-pgp65-path args))
	    ;; Because PGPPASSFD might be set
	    (process-send-string proc "\r\n")
	    (process-send-string proc key)
	    (process-send-string proc "\r\n")
	    (process-send-eof proc)
	    (set-buffer buf)
	    (while (eq 'run (process-status proc))
	      (accept-process-output proc 5)
	      (goto-char (point-min)))
	    (if (y-or-n-p "Add this key to keyring? ")
		(progn
		  (setq args (append args '("-ka")))
		  (setq proc
			(apply 'start-process "*PGP*" buf mc-pgp65-path args))
		  ;; Because PGPPASSFD might be set
		  (process-send-string proc "\r\n")
		  (process-send-string proc key)
		  (process-send-string proc "\r\n")
		  (process-send-eof proc)
		  (while (eq 'run (process-status proc))
		    (accept-process-output proc 5))
		  t)))
	(if buf (kill-buffer buf))))))

;;}}}
