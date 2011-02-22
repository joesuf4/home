;;; dict.el --- Emacs interface to dict client
;;
;; $Id: dict.el,v 1.5 2005/09/28 01:16:23 psg Exp $
;;

;; Copyright (c) 2002, 2003 Max Vasin
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary
;;
;; Emacs DICT Client is an Emacs wrapper around `dict' command to provide
;; an easy and comfortable (from my point of view) access to the dictd server
;; from the Emacs.  The package was written and tested under GNU Emacs 21 only
;; but I hope it should work under other Emacs versions as well.
;;
;; The package provides several key bindings, which are customisation variables,
;; so you can change them easily without recompiling the package:
;;     1. `C-c d d' for running dict with options defined by customisation
;;        variables described below.
;;     2. `C-c d r' for running dict on region as a single word.
;;     3. `C-c d m' for running dict on every word in the region.
;;     4. `C-c d s' for running dict to perform search on the given server.
;;
;; Descriptions of all customisation variables are given below in their
;; definitions, and of cause you can find them in the customisation buffer
;; (External->Emacs Dict Client).
;;
;; I hope you find the program usefull.  And also I would like to know your
;; opinion about the program, improvement suggestions and of cause bug reports.
;; Mail them to <max-appolo@mail.ru>

;;; History:
;;
;;    $Log: dict.el,v $
;;    Revision 1.5  2005/09/28 01:16:23  psg
;;    dict.el: `current-word' can return nil", thanks to Jorgen Schaefer for the
;;    report and patch. (Closes: #301293).
;;
;;    Revision 1.4  2003/10/06 14:01:21  psg
;;    New upstream version, fixes initial enabling via defcustom.
;;
;;    Revision 1.33  2003/10/06 09:32:14  max
;;    `:require 'dict' have been added to all defcustoms that have `:set' commands.
;;    (Thanks to Peter S. Galbraith for bug report.)
;;
;;    Revision 1.32  2003/09/30 15:52:40  max
;;    dict-generate-options: added "--pager -" option to
;;    force pager non-usage.
;;    dict-always-quote-terms: new customisation variable.
;;    dict-quote: new function.
;;    dict-get-answer: changed to quote terms (conditionally).
;;
;;    Revision 1.31  2003/09/29 11:39:39  max
;;    dict-enable-key-bindings: set function updates key bindings.
;;    dict-set-key-binding: forces `dict-enable-key-bindings' be t.
;;
;;    Revision 1.30  2003/09/29 11:27:54  max
;;    1. Changed according to Emacs Coding Conventions (need more?).
;;    2. Changed to be `checkdoc' clean.
;;
;;    Revision 1.29  2003/07/14 09:23:35  max
;;    dict-buffer-coding-system: New customisation variable.
;;    dict-get-answer: Changed to use `dict-buffer-coding-system'
;;
;;    Revision 1.28  2003/07/03 09:06:42  max
;;    dict-mode, dict-mode-set-key-binding,
;;    dict-mode-update-key-bindings: New function.
;;    dict-mode-key-binding, dict-mode-region-key-binding,
;;    dict-multiple-key-binding, dict-on-server-key-binding: New customisation variable.
;;
;;    Revision 1.27  2003/04/15 18:19:53  max
;;    Names made coherent.
;;
;;    Revision 1.26  2003/01/01 13:11:24  max
;;    dict-get-answer: Made to replace newlines and multiple
;;    space with one space.
;;
;;    Revision 1.25  2002/11/21 16:36:11  max
;;    - dict-set-key-binding: New function.
;;    - dict-update-key-bindings: New function.
;;    - Key binding customisation variable set
;;      function chaged to dict-set-key-binding.
;;
;;    Revision 1.24  2002/10/10 10:33:04  max
;;    Added functions to display revision number.
;;
;;    Revision 1.23  2002/10/01 15:05:08  max
;;    dict-server: Changed customisation type.
;;    Added a brief package description.
;;
;;    Revision 1.22  2002/09/30 14:44:12  max
;;    dict-on-server: New function and key binding for it.
;;    dict-get-answer: Changed to run dict asynchronously.
;;
;;    Revision 1.21  2002/09/25 13:34:20  max
;;    Functions dict-databases and dict-strategies removed.
;;
;;    Revision 1.20  2002/09/25 13:30:05  max
;;    Added forward declarations of dict-database and dict-strategy customisation
;;    variables to preserve their relative position in the customisation buffer.
;;    Later they are redefined to use generated sets of values.
;;
;;    Revision 1.19  2002/09/25 13:18:38  max
;;    Changed customisation types for dict-strategy and dict-database.
;;
;;    Revision 1.18  2002/09/23 16:47:12  max
;;    Type of dict-server and dict-database customisation variables
;;    changed to the list of strings.
;;
;;    Revision 1.17  2002/09/22 15:24:22  max
;;    dict-get-answer: Corrected to clear buffer before running dict
;;    and to set point in that buffer to the beginning.
;;
;;    Revision 1.16  2002/09/22 10:36:42  max
;;    - dict-process-key-binding: New function.
;;    - Key bindings changed to work with dict-process-key-binding.
;;    - Call to global-set-key changed to use dict-process-key-binding.
;;
;;    Revision 1.15  2002/09/22 06:44:27  max
;;    - dict-get-answer: New function.
;;    - dict: Changed to use dict-get-answer.
;;    - Now the answer for the dict is stored in the buffer
;;      named *DICT <word>*, where <word> is dict parameters.
;;
;;    Revision 1.14  2002/09/15 07:26:08  max
;;    - dict-region function takes two params
;;    - added dict-multiple function and bindings for it
;;
;;    Revision 1.13  2002/09/13 15:28:52  max
;;    Added simple dict-region function
;;    and a key binding for it.
;;
;;    Revision 1.12  2002/09/03 16:33:39  max
;;    Group name changed to `Emacs Dict client'
;;
;;    Revision 1.11  2002/09/02 12:53:22  max
;;    Customisation type string changed to sexp.
;;    Customisation group moved to the external supergroup.
;;
;;    Revision 1.10  2002/09/02 09:14:04  max
;;    Functions names prefix changed to dict.
;;    Customisation group changed to dict.
;;    Added customisation variables for key bindings.
;;
;;    Revision 1.9  2002/09/02 08:46:06  max
;;    Name changed to emacs-dict-client.el
;;
;;    Revision 1.8  2002/08/30 07:18:53  max
;;    Added documentation to functions
;;
;;    Revision 1.7  2002/08/25 12:54:25  max
;;    Package now provides itself.
;;
;;    Revision 1.6  2002/08/25 12:41:41  max
;;    Added key bindings for running edict and wordinspect.
;;
;;    Revision 1.5  2002/08/25 11:14:25  max
;;    - Added customisation support.
;;    - Removed dependency of man module.
;;
;;    Revision 1.4  2002/08/19 10:32:02  max
;;    edict: Added default value for word.
;;    edict-region: Removed.
;;
;;    Revision 1.3  2002/08/19 10:10:07  max
;;    Log added
;;

;;; Code:
(defgroup Dict nil
  "Browse DICT dictionaries."
  :prefix "dict-"
  :group 'external)

;;;;
;;;; Definitions of dict client parameter variables
;;;;

(defcustom dict-server ""
  "Specifies the hostname for the  DICT  server.
Server/port combinationscan be specified in the configuration file.
If no servers are specified the default behavior is to try dict.org,
alt0.dict.org, alt1.dict.org, and alt2.dict.org, in that order.  If IP
lookup for a server expands to a list of IP addresses (as dict.org
does currently), then each IP will be tried in the order listed."
  :type 'string
  :group 'Dict)

;; Forward declarations
(defcustom dict-database nil
  "Foo."
  :type 'string
  :group 'Dict)

(defcustom dict-strategy nil
  "Bar."
  :type 'string
  :group 'Dict)

(defcustom dict-service ""
  "Specifies the port or service for connections.
The default is 2628, as specified in the DICT Protocol
RFC.  Server/port combinations can be specified in the configuration
file."
  :type 'string
  :group 'Dict)

(defcustom dict-match nil
  "Instead of printing a definition, perform a match using the specified strategy."
  :type 'boolean
  :group 'Dict)

(defcustom dict-nocorrect ""
  "Disable spelling correction.
Usually, if a definition is requested and the word cannot be found,
spelling correction is requested from the server, and a list of
possible words are provided.  This option disables the generation of
this list."
  :type 'string
  :group 'Dict)

(defcustom dict-config-file ""
  "Specify the configuration file.
The default is to try ~/.dictrc and /etc/dict.conf, using the first
file that exists.  If a specific configuration file is specified, then
the defaults will not be tried."
  :type 'string
  :group 'Dict)

(defcustom dict-noauth nil
  "Disable authentication (i.e., don't send an AUTH command)."
  :type 'boolean
  :group 'Dict)

(defcustom dict-user ""
  "Specifies the username for authentication."
  :type 'string
  :group 'Dict)

(defcustom dict-key ""
  "Specifies the shared secret for authentication."
  :type 'string
  :group 'Dict)

(defcustom dict-verbose nil
  "Be verbose."
  :type 'boolean
  :group 'Dict)

(defcustom dict-raw nil
  "Be very verbose: show the raw client/server interaction."
  :type 'boolean
  :group 'Dict)

(defcustom dict-pipesize 256
  "Specify the buffer size for pipelineing commands.
The default is 256, which should be sufficient for general tasks and
be below the MTU for most transport media.  Larger values may provide
faster or slower throughput, depending on MTU.  If the buffer is too
small, requests will be serialized.  Values less than 0 and greater
than one million are silently changed to something more reasonable."
  :type 'integer
  :group 'Dict)

(defcustom dict-original-server ""
  "Specifies original server name for the `dict-on-server' function."
  :type 'string
  :group 'Dict)

(defcustom dict-client ""
  "Specifies additional text to be sent using the CLIENT command."
  :type 'string
  :group 'Dict)

(defcustom dict-always-quote-terms nil
  "If t dict will always quote terms."
  :type 'boolean
  :group 'Dict)

;;;;
;;;; Key binding customisation variables and helper functions
;;;;

(defun dict-set-key-binding (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defun dict-set-enable-key-bindings (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defun dict-mode-set-key-binding (key value)
  "Stub for setting KEY binding to VALUE."
  (set-default key value))

(defcustom dict-enable-key-bindings nil
  "Enables key bindings for dict.el commands."
  :type 'boolean
  :group 'Dict
  :set 'dict-set-enable-key-bindings
  :require 'dict)

(defcustom dict-key-binding "\\C-cdd"
  "Specifies a key binding to run dict and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-region-key-binding "\\C-cdr"
  "Specifies a key binding to run dict on the region and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-multiple-key-binding "\\C-cdm"
  "Run dict on region.
Specifies a key binding to run dict on every word from the region
and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defcustom dict-on-server-key-binding "\\C-cds"
  "Run dict on server.
Specifies a key binding to run dict to search word on the given
server and display the results in the Emacs buffer."
  :type 'string
  :group 'Dict
  :set 'dict-set-key-binding
  :require 'dict)

(defgroup Dict-Mode nil
  "DICT-mode key bindings"
  :tag "DICT-mode"
  :prefix "dict-mode-"
  :group 'Dict)

(defcustom dict-mode-key-binding "d"
  "Specifies a key binding to run dict and display the results in the Emacs buffer."
  :tag "DICT"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-mode-region-key-binding "r"
  "Specifies a key binding to run dict on the region and display the results in the Emacs buffer."
  :tag "DICT region"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-multiple-key-binding "m"
  "Run dict on every word in region.
Specifies a key binding to run dict on every word from the region and
display the results in the Emacs buffer."
  :tag "DICT multiple"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-on-server-key-binding "s"
  "Specifies a key binding to run dict to search word on the given server and display the results in the Emacs buffer."
  :tag "DICT on server"
  :type 'string
  :group 'Dict-Mode
  :set 'dict-mode-set-key-binding
  :require 'dict)

(defcustom dict-buffer-coding-system nil
  "Specifies coding system to use in dict buffer."
  :tag "Input coding system for DICT buffer"
  :type 'sexp
  :group 'Dict-Mode)

;;;;
;;;; Service functions
;;;;

(defun dict-get-databases ()
  "Get a list of available databases."
  (let ((dbs (shell-command-to-string "dict -D | tail -n $(expr $(dict -D | wc -l) - 1) | cut -f 3 -d ' '")))
    (sort (read (concat "(" dbs ")")) 'string<)))

(defun dict-get-stategies ()
  "Get a list of strategies."
  (let ((strats (shell-command-to-string "dict -S | tail -n $(expr $(dict -S | wc -l) - 1) | cut -f 3 -d ' '")))
    (sort (read (concat "(" strats ")")) 'string<)))

(defun dict-generate-consts (values)
  "Generate a list of constants for customisation types of VALUES."
  (if (null values)
      nil
    (cons `(const ,(car values)) (dict-generate-consts (cdr values)))))

(defcustom dict-strategy nil
  "Specify a matching strategy.
By default, the server default match strategy is used.  This is
usually \"exact\" for definitions, and some form of
spelling-correction strategy for matches (\".\" fromthe DICT
protocol). The available strategies are dependent on the server
implemenation."
  :type `(choice ,@(dict-generate-consts (dict-get-stategies)) (const :tag "default" nil))
  :group 'Dict)

(defcustom dict-database nil
  "Specifies a specific database to search.
The default is to search all databases (a * from the DICT
protocol).  Note that a ! in the DICT protocol means to search all of
the databases until a match is found, and then stop searching."
  :type  `(set ,@(dict-generate-consts (dict-get-databases)))
  :group 'Dict) ;"

(defun dict-generate-options-list (prefix seq)
  "Generate a list of options of the form `PREFIX SEQ[0] PREFIX SEQ[1] ...'."
  (if (null seq)
      ""
      (concat prefix (car seq) (dict-generate-options-list prefix (cdr seq)))))

(defun dict-mkseq (string)
  "Make a list from a STRING."
  (read (concat "(\"" string "\")")))

(defsubst dict-nes (string)
  "T if STRING is not empty."
  (not (string= string "")))

(defun dict-generate-options ()
  "Generate dict's command line options based on the parameter variables' values."
  (concat
   (if (dict-nes dict-server) (dict-generate-options-list " --host " (dict-mkseq dict-server)) "")
   (if (dict-nes dict-service) (concat " --port " dict-service) "")
   (if dict-database (dict-generate-options-list " --database " (dict-mkseq dict-database)) "")
   (if dict-match " --match" "")
   (if dict-strategy (concat " --strategy " dict-strategy) "")
   (if dict-nocorrect " --nocorrect " "")
   (if (dict-nes dict-config-file)  (concat " --config " dict-config-file) "")
   (if dict-noauth " --noauth" "")
   (if (dict-nes dict-user) (concat " --user " dict-user) "")
   (if (dict-nes dict-key) (concat " --key " dict-key) "")
   (if dict-verbose " --verbose" "")
   (if dict-raw " --raw" "")
   (if (not (= dict-pipesize 256)) (concat " --pipesize " (number-to-string dict-pipesize)) "")
   (if (dict-nes dict-client) (concat " --client " dict-client) "")
   " --pager -"
   " "))

(defun dict-newline-to-space (string)
  "Replace newline with space in STRING."
  (let ((result (make-string (length string) ?x)))
    (dotimes (i (length string) 1)
      (aset result i (if (char-equal (aref string i) ?\n) ?\  (aref string i))))
    result))

(defun dict-reduce-spaces (string)
  "Replace multiple sequencial whitespaces in STRING with one whitespace."
  (if (not (string-match "[ \t][ \t]+" string))
      string
    (dict-reduce-spaces (replace-match " " t "\\&" string nil))))

(defun dict-normalise-request (request)
  "Replace newlines and multiple spaces with one space in the REQUEST."
  (dict-reduce-spaces (dict-newline-to-space request)))

(defun dict-quote (word)
  "Quote WORD if necessary."
  (if dict-always-quote-terms
      (if (or (and (eq (aref word 0) ?\")
		   (eq (aref word (- (length word) 1)) ?\"))
	      (and (eq (aref word 0) ?\')
		   (eq (aref word (- (length word) 1)) ?\')))
	  word
	(concat "'" word "'"))
    word))

(defun dict-get-answer (what)
  "Recieve the answer for WHAT from the dict and insert in ther buffer."
  (let* ((word (dict-normalise-request what))
	 (buffer-name (concat "*DICT " word "*"))
	 (buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name)))
	 (coding-system-for-read dict-buffer-coding-system)
	 (coding-system-for-write dict-buffer-coding-system))
    (save-excursion
      (set-buffer buffer)
      (kill-region (point-min) (point-max))
      (dict-mode))
    (message "Invoking dict %s in the background" word)
    (set-process-sentinel
     (start-process "dict" buffer "sh" "-c" (format "dict %s %s" (dict-generate-options) (dict-quote word)))
     'dict-bgproc-sentinel)))

(defun dict-bgproc-sentinel (process msg)
  "Dict background PROCESS sentinel (handling MSG)."
  (let ((buffer (process-buffer process)))
    (cond
     ((string= "finished\n" msg)
      (save-excursion (set-buffer buffer)
		      (goto-char (point-min))
		      (display-buffer buffer)))
     ((string-match "exited abnormally with code" msg)
      (message msg)))))

(defsubst dict-default-dict-entry ()
  "Make a guess at a default dict entry.
This guess is based on the text surrounding the cursor."
  (let ((word (or (current-word)
                  "")))
    (if (string-match "[._]+$" word)
        (substring word 0 (match-beginning 0))
      word)))

;;;;
;;;; Lookup functions
;;;;

(defun dict (word)
  "Lookup a WORD in the dictionary."
  (interactive (list (let* ((default-entry (dict-default-dict-entry))
	     (input (read-string
		     (format "Dict entry%s: "
			     (if (string= default-entry "")
				 ""
			       (format " (default %s)" default-entry))))))
	(if (string= input "")
	    (if (string= default-entry "")
		(error "No dict args given") default-entry) input))))
  (dict-get-answer word))

(defun dict-region (from to)
  "Lookup a region (FROM, TO) in the dictionary."
  (interactive (list (region-beginning)
		     (region-end)))
  (dict (concat "\"" (buffer-substring-no-properties from to) "\"")))

(defun dict-multiple (from to)
  "Lookup every word from the region (FROM, TO) in the dictionary."
  (interactive (list (region-beginning)
		     (region-end)))
  (dict (buffer-substring-no-properties from to)))

(defun dict-on-server (word server)
  "Lookup a WORD in the dictionary on the given SERVER."
  (interactive (list
		(let* ((default-entry (dict-default-dict-entry))
		       (input (read-string
			       (format "Dict entry%s: "
				       (if (string= default-entry "")
					   ""
					 (format " (default %s)" default-entry))))))
		  (if (string= input "")
		      (if (string= default-entry "")
			  (error "No dict args given") default-entry) input))
		(read-string "DICT server: " nil)))
  (if (not (string= server ""))
      (let ((dict-server server))
	(dict word))
    (dict word)))

(defun dict-set-key-binding (key value)
  "Set KEY binding customisation variable to VALUE."
  (let ((result (set-default key value)))
    (set-default 'dict-enable-key-bindings t)
    (dict-update-key-bindings)
    result))

(defun dict-set-enable-key-bindings (key value)
  "Set KEY to VALUE and update dict key bindings."
  (let ((result (set-default key value)))
    (dict-update-key-bindings)
    result))

(defun dict-process-key-binding (string)
  "Process a STRING representing a key binding to allow easy key binding customisation."
  (read (concat "\"" string "\"")))

(defvar dict-mode-keymap (make-sparse-keymap))

(defun dict-mode-set-key-binding (key value)
  "Set KEY binding customisation variable to VALUE (for DICT-mode)."
  (let ((result (set-default key value)))
    (dict-mode-update-key-bindings)
    result))

(defun dict-mode()
  (interactive)
  (use-local-map dict-mode-keymap)
  (setq mode-name "DICT")
  (setq major-mode 'dict-mode))

(defun dict-update-key-bindings ()
  "Update dict key bindings."
  (when dict-enable-key-bindings
    ;; Setup global key binding `C-c d d' for running dict...
    (global-set-key (dict-process-key-binding dict-key-binding) 'dict)
    ;; ... `C-c d r' for running dict on the region...
    (global-set-key (dict-process-key-binding dict-region-key-binding) 'dict-region)
    ;; ... `C-c d m' for running dict on every word in the region...
    (global-set-key (dict-process-key-binding dict-multiple-key-binding) 'dict-multiple)
    ;; ... `C-c d s' for running dict to perform search on the given server...
    (global-set-key (dict-process-key-binding dict-on-server-key-binding) 'dict-on-server)))

(defun dict-mode-update-key-bindings ()
  "Update dict key bindings for DICT-mode."
  ;; Setup DICT-mode key binding `d' for running dict...
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-key-binding) 'dict)
  ;; ... `r' for running dict on the region...
  (define-key dict-mode-keymap (dict-process-key-binding dict-mode-region-key-binding) 'dict-region))

;;;;
;;;; Informational functions
;;;;

(defun dict-version ()
  "Display dict version information."
  (interactive)
  (shell-command "dict --version"))

(defconst dict-version
  "$Revision: 1.5 $"
  "Version number for 'dict' package.")

(defun dict-version-number ()
  "Return 'dict' version number."
  (string-match "[0123456789.]+" dict-version)
  (match-string 0 dict-version))

(defun dict-display-version ()
  "Display 'dict' version."
  (interactive)
  (message "Emacs DICT client version <%s>." (dict-version-number)))

(dict-update-key-bindings)
(dict-mode-update-key-bindings)

; LocalWords:  dict dictd wordinspect appolo ru

(provide 'dict)

;;; dict.el ends here
