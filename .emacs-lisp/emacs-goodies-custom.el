;;; emacs-goodies-custom.el --- Automatically harvested defgroups
;;
;;  Peter S Galbraith <psg@debian.org>
;;  License of copied code applies to this combined work (GPL V2)
;;
;;; Code:

(defgroup apache-mode nil
  "Major mode for editing Apache configuration files."
  :group 'programming
  :link '(custom-manual "(emacs-goodies-el)apache-mode")
  :load 'apache-mode
;;:require 'apache-mode
  :group 'emacs-goodies-el)

(defgroup ascii nil
  "ASCII code display"
  :link '(emacs-library-link :tag "Source Lisp File" "ascii.el")
  :prefix "ascii-"
  :group 'data
  :link '(custom-manual "(emacs-goodies-el)ascii")
  :load 'ascii
  :group 'emacs-goodies-el)

;; bar-cursor
(defgroup bar-cursor nil
  "switch block cursor to a bar."
  :link '(custom-manual "(emacs-goodies-el)bar-cursor")
  :group 'convenience
  :load 'bar-cursor
;;:require 'bar-cursor
  :group 'emacs-goodies-el)

;; boxquote
(defgroup boxquote nil
  "Mark regions of text with a half-box."
  :group  'editing
  :prefix "boxquote-"
  :link '(custom-manual "(emacs-goodies-el)boxquote")
  :load 'boxquote
;;:require 'boxquote
  :group 'emacs-goodies-el)

;; browse-kill-ring
(defgroup browse-kill-ring nil
  "A package for browsing and inserting the items in `kill-ring'."
  :link '(url-link "http://web.verbum.org/~walters")
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)browse-kill-ring")
  :load 'browse-kill-ring
;;:require 'browse-kill-ring
  :group 'emacs-goodies-el)

;; color-theme
(defgroup color-theme nil
  "Color Themes for Emacs.
A color theme consists of frame parameter settings, variable settings,
and face definitions."
  :version "20.6"
  :group 'faces
  :link '(custom-manual "(emacs-goodies-el)color-theme")
  :load 'color-theme
  :group 'emacs-goodies-el)

;; csv-mode
(defgroup CSV nil
  "Major mode for editing files of comma-separated value type."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)csv-mode")
  :load 'csv-mode
  :group 'emacs-goodies-el)

;; ctypes
(defgroup ctypes nil
  "Enhanced Font lock support for custom defined types."
  :group 'programming
  :link '(custom-manual "(emacs-goodies-el)ctypes")
  :load 'ctypes
  :group 'emacs-goodies-el)

;; cua
(defgroup CUA-mode nil
  "Emulate CUA key bindings including C-x and C-c."
  :prefix "CUA-mode"
  :group 'editing-basics
  :group 'convenience
  :group 'emulations
  :link '(emacs-commentary-link :tag "Commentary" "cua.el")
  :link '(emacs-library-link :tag "Lisp File" "cua.el")
  :link '(custom-manual "(emacs-goodies-el)cua")
  :load 'cua
  :group 'emacs-goodies-el)

;; cwebm
(defgroup CWEBm nil
  "Major mode for editing CWEB and WEB programs"
  :prefix "cwebm-"
  :group 'languages
  :link '(custom-manual "(emacs-goodies-el)cwebm")
  :load 'cwebm
  :group 'emacs-goodies-el)

;; df
(defgroup df nil
  "Display space left on partitions in the mode-line."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)df")
  :load 'df
;;:require 'df
  :group 'emacs-goodies-el)

;; dict
(defgroup Dict nil
  "Browse DICT dictionaries."
  :prefix "dict-"
  :group 'external
  :link '(custom-manual "(emacs-goodies-el)dict")
  :load 'dict
;;:require 'dict
  :group 'emacs-goodies-el)

;; diminish
(defgroup diminish nil
  "Diminished modes are minor modes with no modeline display."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)diminish")
  :load 'diminish
;;:require 'diminish
  :group 'emacs-goodies-el)

;; egocentric
(defgroup egocentric nil
  "Highlight your name in arbitrary buffers."
  :group 'files
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)egocentric")
  :load 'egocentric
;;:require 'egocentric
  :group 'emacs-goodies-el)

;; ff-paths
(defgroup ff-paths nil
  "Find file using paths."
  :group 'ffap
  :group 'matching
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)ff-paths")
  :load 'ff-paths
;;:require 'ff-paths
  :group 'emacs-goodies-el)

;; filladapt
(defgroup filladapt nil
  "Enhanced filling"
  :group 'fill
  :link '(custom-manual "(emacs-goodies-el)filladapt")
  :load 'filladapt
;;:require 'filladapt
  :group 'emacs-goodies-el)

;; floatbg
(defgroup floatbg nil
  "Slowly modify background color by moving through an HSV color model."
  :tag "Floating Background"
  :group 'frames
  :prefix "floatbg-"
  :link '(custom-manual "(emacs-goodies-el)floatbg")
  :load 'floatbg
;;:require 'floatbg
  :group 'emacs-goodies-el)

;; folding
(defgroup folding nil
  "Managing buffers with Folds."
  :group 'tools
  :link '(custom-manual "(emacs-goodies-el)folding")
  :load 'folding
;;:require 'folding
  :group 'emacs-goodies-el)

;; framepop
(defgroup framepop nil
  "Display temporary buffers in a dedicated frame."
  :group 'frames
  :link '(custom-manual "(emacs-goodies-el)framepop")
  :load 'framepop
;;:require 'framepop
  :group 'emacs-goodies-el)

;; highlight-beyond-fill-column
(defgroup highlight-beyond-fill-column nil
  "Fontify beyond the fill-column."
  :group 'fill
  :link '(custom-manual "(emacs-goodies-el)highlight-beyond-fill-column")
  :load 'highlight-beyond-fill-column
;;:require 'highlight-beyond-fill-column
  :group 'emacs-goodies-el)

;; highlight-completion
(defgroup highlight-completion nil
  "Highlight completion mode: display completion as highlighted text."
  :tag "Highlight completion"
  :prefix "hc"
  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/hlc.html")
  :group 'abbrev
  :link '(custom-manual "(emacs-goodies-el)highlight-completion")
  :load 'highlight-completion
;;:require 'highlight-completion
  :group 'emacs-goodies-el)

;; highlight-current-line
(defgroup highlight-current-line nil
  "Highlight line where the cursor is."
  :load 'highlight-current-line
  :group 'faces
  :link '(custom-manual "(emacs-goodies-el)highlight-current-line")
  :load 'highlight-current-line
;;:require 'highlight-current-line
  :group 'emacs-goodies-el)

;; htmlize
(defgroup htmlize nil
  "HTMLize font-locked buffers."
  :group 'hypermedia
  :link '(custom-manual "(emacs-goodies-el)htmlize")
  :load 'htmlize
;;:require 'htmlize
  :group 'emacs-goodies-el)

;; ibuffer
(when (not (featurep 'xemacs))
  (defgroup ibuffer nil
    "An advanced replacement for `buffer-menu'.

Ibuffer allows you to operate on buffers in a manner much like Dired.
Operations include sorting, marking by regular expression, and
selectable views (limits)."
    :link '(url-link "http://web.verbum.org/~walters")
    :group 'convenience
    :link '(custom-manual "(emacs-goodies-el)ibuffer")
    :load 'ibuffer
  ;;:require 'ibuffer
    :group 'emacs-goodies-el))

;; ido.el
(defgroup ido nil
  "Switch between files using substrings."
  :group 'extensions
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "ido.el")
  :link '(emacs-library-link :tag "Lisp File" "ido.el")
  :link '(custom-manual "(emacs-goodies-el)ido")
  :load 'ido
  :group 'emacs-goodies-el)

;; initsplit
(defgroup initsplit nil
  "Code to split customizations into different files."
  :group 'initialization
;;:link '(custom-manual "(emacs-goodies-el)initsplit")
  :load 'initsplit
;;:require 'initsplit
  :group 'emacs-goodies-el)

(defgroup joc-toggle-buffer nil
  "toggle-buffer package customization"
  :group 'tools
  :link '(custom-manual "(emacs-goodies-el)joc-toggle-buffer")
  :load 'joc-toggle-buffer
  :group 'emacs-goodies-el)

(defgroup joc-toggle-case nil
  "joc-toggle-case package customization"
  :group 'tools
  :link '(custom-manual "(emacs-goodies-el)joc-toggle-case")
  :load 'joc-toggle-case
  :group 'emacs-goodies-el)

;; keywiz
(defgroup keywiz nil
  "Emacs key sequence quiz."
  :version "21.2"
  :group 'games
  :group 'keyboard
  :link '(emacs-commentary-link "keywiz.el")
  :link '(custom-manual "(emacs-goodies-el)keywiz")
  :load 'keywiz
;;:require 'keywiz
  :group 'emacs-goodies-el)

;; lcomp
(defgroup lcomp nil
  "list-completion hacks."
  :group 'completion
  :link '(custom-manual "(emacs-goodies-el)lcomp")
  :load 'lcomp
  :group 'emacs-goodies-el)

;; maplev
(defgroup maplev nil
  "Major mode for editing Maple source in Emacs"
  :group 'languages
  :link '(custom-manual "(emacs-goodies-el)maplev")
  :load 'maplev
  :group 'emacs-goodies-el)

;; matlab
(defgroup matlab nil
  "Matlab mode."
  :prefix "matlab-"
  :group 'languages
  :link '(custom-manual "(emacs-goodies-el)matlab")
  :load 'matlab
  :group 'emacs-goodies-el)

;; minibuffer-complete-cycle
(defgroup minibuffer-complete-cycle nil
  "Cycle through the *Completions* buffer."
  :group 'completion
  :link '(custom-manual "(emacs-goodies-el)minibuffer-complete-cycle")
  :load 'minibuffer-complete-cycle
;;:require 'minibuffer-complete-cycle
  :group 'emacs-goodies-el)

;; mutt-alias
(defgroup mutt-alias nil
  "Lookup mutt mail aliases."
  :group  'mail
  :prefix "mutt-alias-"
  :link '(custom-manual "(emacs-goodies-el)mutt-alias")
  :load 'mutt-alias
;;:require 'mutt-alias
  :group 'emacs-goodies-el)

;; muttrc-mode
(defgroup muttrc nil
  "Muttrc editing commands for Emacs."
  :group 'files
  :prefix "muttrc-"
  :link '(custom-manual "(emacs-goodies-el)muttrc-mode")
  :load 'muttrc-mode
;;:require 'muttrc-mode
  :group 'emacs-goodies-el)

;; newsticker
(defgroup newsticker nil
  "Newsticker settings. Some of these changes do not have an immediate
effect.  Therefore it is recommended to `newsticker-stop' and then
`newsticker-start' again after you have changed any of these settings."
  :link '(custom-manual "(emacs-goodies-el)newsticker")
  :load 'newsticker
;;:require 'newsticker
  :group 'emacs-goodies-el)

;; nuke-trailing-whitespace
(defgroup nuke-trailing-whitespace nil
  "Strip trailing whitespace from buffers."
  :group 'editing
  :link '(custom-manual "(emacs-goodies-el)nuke-trailing-whitespace")
  :load 'nuke-trailing-whitespace
;;:require 'nuke-trailing-whitespace
  :group 'emacs-goodies-el)

;; pack-windows
(defgroup pack-windows nil
  "Resize all windows to display as much info as possible."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)pack-windows")
  :load 'pack-windows
  :group 'emacs-goodies-el)

;; perldoc
(defgroup perldoc nil
  "Show help for Perl functions, builtins, and modules."
  :group  'help
  :link '(custom-manual "(emacs-goodies-el)perldoc")
  :load 'perldoc
;;:require 'perldoc
  :group 'emacs-goodies-el)

;; projects
(defgroup projects nil
  "Project-based buffer name management."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)projects")
  :load 'projects
;;:require 'projects
  :group 'emacs-goodies-el)

;; protbuf
(defgroup protect-buffer nil
  "Protect buffers from accidental killing."
  :group 'killing
  :link '(custom-manual "(emacs-goodies-el)protbuf")
  :load 'protbuf
;;:require 'protbuf
  :group 'emacs-goodies-el)

;; rfcview
(defgroup rfcview nil
  "View IETF RFC files with formatting."
  :group  'hypermedia
  :prefix "rfcview-"
  :link '(custom-manual "(emacs-goodies-el)rfcview")
  :load 'rfcview
  :group 'emacs-goodies-el)

;; session
(defgroup session nil
  "Use variables, registers and buffer places across sessions."
  :group 'data
  :link '(emacs-commentary-link "session.el")
  :link '(url-link "http://emacs-session.sourceforge.net/")
  :prefix "session-"
  :link '(custom-manual "(emacs-goodies-el)session")
  :load 'session
;;:require 'session
  :group 'emacs-goodies-el)

;; setnu
(defgroup setnu nil
  "vi-style line number mode for Emacs."
  :link '(custom-manual "(emacs-goodies-el)setnu")
  :load 'setnu
  :group 'emacs-goodies-el)

;; shell-command
(defgroup shell-command nil
  "Enable Tab completions for `shell-command' and related commands."
  :group 'shell
  :link '(custom-manual "(emacs-goodies-el)shell-command")
  :load 'shell-command
  :group 'emacs-goodies-el)

(defgroup slang nil
  "Major mode for editing slang code."
  :prefix "slang-"
  :group 'languages
  :link '(custom-manual "(emacs-goodies-el)slang-mode")
  :load 'slang-mode
  :group 'emacs-goodies-el)

(defgroup silly-mail nil
  "Generate bozotic mail headers."
  :group 'mail
  :group 'mh
  :group 'sendmail
  :link '(custom-manual "(emacs-goodies-el)silly-mail")
  :load 'silly-mail
  :group 'emacs-goodies-el)

(defgroup tabbar nil
  "Display a tab bar in the header line."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)tabbar")
  :load 'tabbar
  :group 'emacs-goodies-el)

;; table
(defgroup table nil
  "Text based table manipulation utilities.
See `table-insert' for examples about how to use."
  :tag "Table"
  :prefix "table-"
  :group 'editing
  :group 'wp
  :group 'paragraphs
  :group 'fill
  :link '(custom-manual "(emacs-goodies-el)table")
  :load 'table
;;:require 'table
  :group 'emacs-goodies-el)

;; tail
(defgroup tail nil
  "Tail files or commands into Emacs buffers."
  :prefix "tail-"
  :group 'environment
  :link '(custom-manual "(emacs-goodies-el)tail")
  :load 'tail
;;:require 'tail
  :group 'emacs-goodies-el)

;; tc
(defgroup tc nil "Insert cited text in a nice manner"
;;:link '(custom-manual "(emacs-goodies-el)tc")
  :load 'tc
;;:require 'tc
  :group 'emacs-goodies-el)

;; thinks
(defgroup thinks nil
  "Insert text in a think bubble."
  :group  'editing
  :prefix "thinks-"
  :link '(custom-manual "(emacs-goodies-el)thinks")
  :load 'thinks
;;:require 'thinks
  :group 'emacs-goodies-el)

;;tlc
(defgroup tlc nil
  "Major mode for editing tlc files."
  :group 'languages
  :link '(custom-manual "(emacs-goodies-el)tlc")
  :load 'tlc
  :group 'emacs-goodies-el)

;; todoo
(when (not (featurep 'xemacs))
  (defgroup todoo nil
    "Maintain a list of todo items."
    :group 'calendar
    :link '(custom-manual "(emacs-goodies-el)todoo")
    :load 'todoo
  ;;:require 'todoo
    :group 'emacs-goodies-el))

;; toggle-option
(defgroup toggle-option nil
  "Convenience library for toggling commonly toggled variables/functions."
  :group 'convenience
  :link '(custom-manual "(emacs-goodies-el)toggle-option")
  :load 'toggle-option
;;:require 'toggle-option
  :group 'emacs-goodies-el)

;; wdired
(defgroup wdired nil
  "Mode to rename files by editing their names in dired buffers."
  :group 'dired
  :link '(custom-manual "(emacs-goodies-el)wdired")
  :load 'wdired
;;:require 'wdired
  :group 'emacs-goodies-el)

;; xrdb-mode
(defgroup xrdb nil
  "Support for editing X resource database files"
  :group 'languages
  :link '(custom-manual "(emacs-goodies-el)xrdb-mode")
  :load 'xrdb-mode
;;:require 'xrdb-mode
  :group 'emacs-goodies-el)

(provide 'emacs-goodies-custom)
