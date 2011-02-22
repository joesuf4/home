;;; emacs-goodies-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:

;;;### (autoloads (all) "all" "all.el" (16259 18879))
;;; Generated autoloads from all.el

(autoload (quote all) "all" "\
Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*All*'.
Any changes made in that buffer will be propagated to this buffer." t nil)

;;;***

;;;### (autoloads (apache-mode) "apache-mode" "apache-mode.el" (17102
;;;;;;  30750))
;;; Generated autoloads from apache-mode.el

(autoload (quote apache-mode) "apache-mode" "\
Major mode for editing Apache configuration files." t nil)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;;;***

;;;### (autoloads (ascii-off ascii-on ascii-display ascii-customize)
;;;;;;  "ascii" "ascii.el" (16313 9500))
;;; Generated autoloads from ascii.el

(autoload (quote ascii-customize) "ascii" "\
Customize ASCII options." t nil)

(autoload (quote ascii-display) "ascii" "\
Toggle ASCII code display.

If ARG is null, toggle ASCII code display.
If ARG is a number and is greater than zero, turn on display; otherwise, turn
off display.
If ARG is anything else, turn on display." t nil)

(autoload (quote ascii-on) "ascii" "\
Turn on ASCII code display." t nil)

(autoload (quote ascii-off) "ascii" "\
Turn off ASCII code display." t nil)

;;;***

;;;### (autoloads (bar-cursor-change bar-cursor-mode) "bar-cursor"
;;;;;;  "bar-cursor.el" (17246 57581))
;;; Generated autoloads from bar-cursor.el

(autoload (quote bar-cursor-mode) "bar-cursor" "\
Toggle use of variable `bar-cursor-mode'.
This quasi-minor mode changes cursor to a bar cursor in insert mode,
and a block cursor in overwrite mode.  It may only be turned on and
off globally, not on a per-buffer basis (hence the quasi- designation).

Optional ARG turns mode on if ARG is a positive integer." t nil)

(autoload (quote bar-cursor-change) "bar-cursor" "\
Enable or disable advice based on value of variable `bar-cursor-mode'." nil nil)

;;;***

;;;### (autoloads (boxquote-unbox boxquote-unbox-region boxquote-fill-paragraph
;;;;;;  boxquote-kill boxquote-narrow-to-boxquote-content boxquote-narrow-to-boxquote
;;;;;;  boxquote-text boxquote-where-is boxquote-shell-command boxquote-describe-key
;;;;;;  boxquote-describe-variable boxquote-describe-function boxquote-boxquote
;;;;;;  boxquote-paragraph boxquote-defun boxquote-yank boxquote-kill-ring-save
;;;;;;  boxquote-insert-file boxquote-buffer boxquote-region boxquote-title)
;;;;;;  "boxquote" "boxquote.el" (17245 26633))
;;; Generated autoloads from boxquote.el

(autoload (quote boxquote-title) "boxquote" "\
Set the title of the current boxquote to TITLE.

If TITLE is an empty string the title is removed. Note that the title will
be formatted using `boxquote-title-format'." t nil)

(autoload (quote boxquote-region) "boxquote" "\
Draw a box around the left hand side of a region bounding START and END." t nil)

(autoload (quote boxquote-buffer) "boxquote" "\
Apply `boxquote-region' to a whole buffer." t nil)

(autoload (quote boxquote-insert-file) "boxquote" "\
Insert the contents of a file, boxed with `boxquote-region'.

If `boxquote-title-files' is non-nil the boxquote will be given a title that
is the result applying `boxquote-file-title-funciton' to FILENAME." t nil)

(autoload (quote boxquote-kill-ring-save) "boxquote" "\
Like `kill-ring-save' but remembers a title if possible.

The title is acquired by calling `boxquote-kill-ring-save-title'. The title
will be used by `boxquote-yank'." t nil)

(autoload (quote boxquote-yank) "boxquote" "\
Do a `yank' and box it in with `boxquote-region'.

If the yanked entry was placed on the kill ring with
`boxquote-kill-ring-save' the resulting boxquote will be titled with
whatever `boxquote-kill-ring-save-title' returned at the time." t nil)

(autoload (quote boxquote-defun) "boxquote" "\
Apply `boxquote-region' the current defun." t nil)

(autoload (quote boxquote-paragraph) "boxquote" "\
Apply `boxquote-region' to the current paragraph." t nil)

(autoload (quote boxquote-boxquote) "boxquote" "\
Apply `boxquote-region' to the current boxquote." t nil)

(autoload (quote boxquote-describe-function) "boxquote" "\
Call `describe-function' and boxquote the output into the current buffer." t nil)

(autoload (quote boxquote-describe-variable) "boxquote" "\
Call `describe-variable' and boxquote the output into the current buffer." t nil)

(autoload (quote boxquote-describe-key) "boxquote" "\
Call `describe-key' and boxquote the output into the current buffer.

If the call to this command is prefixed with \\[universal-argument] you will also be
prompted for a buffer. The key defintion used will be taken from that buffer." t nil)

(autoload (quote boxquote-shell-command) "boxquote" "\
Call `shell-command' with COMMAND and boxquote the output." t nil)

(autoload (quote boxquote-where-is) "boxquote" "\
Call `where-is' with DEFINITION and boxquote the result." t nil)

(autoload (quote boxquote-text) "boxquote" "\
Insert TEXT, boxquoted." t nil)

(autoload (quote boxquote-narrow-to-boxquote) "boxquote" "\
Narrow the buffer to the current boxquote." t nil)

(autoload (quote boxquote-narrow-to-boxquote-content) "boxquote" "\
Narrow the buffer to the content of the current boxquote." t nil)

(autoload (quote boxquote-kill) "boxquote" "\
Kill the boxquote and its contents." t nil)

(autoload (quote boxquote-fill-paragraph) "boxquote" "\
Perform a `fill-paragraph' inside a boxquote." t nil)

(autoload (quote boxquote-unbox-region) "boxquote" "\
Remove a box created with `boxquote-region'." t nil)

(autoload (quote boxquote-unbox) "boxquote" "\
Remove the boxquote that contains `point'." t nil)

;;;***

;;;### (autoloads (browse-huge-tar-copy-file-at-point browse-huge-tar-view-file-at-point
;;;;;;  browse-huge-tar-file) "browse-huge-tar" "browse-huge-tar.el"
;;;;;;  (16313 9500))
;;; Generated autoloads from browse-huge-tar.el

(autoload (quote browse-huge-tar-file) "browse-huge-tar" "\
Create a buffer containing a listing of FILENAME as a tar file." t nil)

(autoload (quote browse-huge-tar-view-file-at-point) "browse-huge-tar" "\
Extract the file at the point into a buffer for viewing." t nil)

(autoload (quote browse-huge-tar-copy-file-at-point) "browse-huge-tar" "\
Extract the file at the point and copy to a local file OUTFILE.
This requires the value of `shell-file-name' to support redirection using \">\"." t nil)

;;;***

;;;### (autoloads (browse-kill-ring browse-kill-ring-default-keybindings)
;;;;;;  "browse-kill-ring" "browse-kill-ring.el" (17246 57581))
;;; Generated autoloads from browse-kill-ring.el

(autoload (quote browse-kill-ring-default-keybindings) "browse-kill-ring" "\
Set up M-y (`yank-pop') so that it can invoke `browse-kill-ring'.
Normally, if M-y was not preceeded by C-y, then it has no useful
behavior.  This function sets things up so that M-y will invoke
`browse-kill-ring'." t nil)

(autoload (quote browse-kill-ring) "browse-kill-ring" "\
Display items in the `kill-ring' in another buffer." t nil)

;;;***

;;;### (autoloads (cfengine-mode) "cfengine" "cfengine.el" (17213
;;;;;;  19161))
;;; Generated autoloads from cfengine.el

(autoload (quote cfengine-mode) "cfengine" "\
Cfengine mode is the major mode for editing Cfengine code.

Bindings are as follows:

 Indent line                                          '\\[cfengine-tab]'
 Indent line, insert newline and indent the new line. '\\[newline-and-indent]'

Comments are handled using standard Emacs conventions, including:
 Start a comment                                      '\\[indent-for-comment]'
 Comment region                                       '\\[comment-region]'
 Uncomment region                                     '\\[cfengine-uncomment-region]'
 Continue comment on next line                        '\\[indent-new-comment-line]'
" t nil)

;;;***

;;;### (autoloads (color-theme-select) "color-theme" "color-theme.el"
;;;;;;  (17246 57581))
;;; Generated autoloads from color-theme.el

(autoload (quote color-theme-select) "color-theme" "\
Displays a special buffer for selecting and installing a color theme.
With optional prefix ARG, this buffer will include color theme libraries
as well.  A color theme library is in itself not complete, it must be
used as part of another color theme to be useful.  Thus, color theme
libraries are mainly useful for color theme authors." t nil)

;;;***

;;;### (autoloads (ctypes-read-file ctypes-auto-parse-mode ctypes-file
;;;;;;  ctypes-dir ctypes-tags ctypes-all-buffers ctypes-buffer ctypes-define-type-in-mode
;;;;;;  ctypes-define-type) "ctypes" "ctypes.el" (17246 57581))
;;; Generated autoloads from ctypes.el

(autoload (quote ctypes-define-type) "ctypes" "\
Add a new TYPE to current major mode and inform font-lock.

When preceded by C-u the display is not updated.

Return non-nil if the type was not known before." t nil)

(autoload (quote ctypes-define-type-in-mode) "ctypes" "\
Add TYPE to major mode MODE and inform font-lock.

When preceded by C-u the display is not updated.

\(This function is designed for interactive use, please call
`ctypes-define-type' from Lisp programs.)" t nil)

(autoload (quote ctypes-buffer) "ctypes" "\
Search for types in buffer, inform font-lock if any is found.

When preceded by C-u the action is not performed.

Return non-nil if new types are found." t nil)

(autoload (quote ctypes-all-buffers) "ctypes" "\
Search for types in all buffers, inform font-lock about all discoveries.

When preceded by C-u the display is not updated.

Return non-nil if new types are found." t nil)

(autoload (quote ctypes-tags) "ctypes" "\
Search for types in files in the visited TAGS table.
Should no tags table be visited, the user will be prompted for a new.

When preceded by C-u the display is not updated.

Return non-nil if new types are found." t nil)

(autoload (quote ctypes-dir) "ctypes" "\
Search for types in files in a directory hierarchy.

See variable `ctypes-dir-read-file' for a description of which files
are opened during scanning, and how you can change the behavior.

When preceded by C-u the display is not updated.

Return non-nil if new types are found." t nil)

(autoload (quote ctypes-file) "ctypes" "\
Search for types in file FILE.
Should FILE not be loaded it is read into a temporary buffer.

Return mode of file, if new types was found." t nil)

(autoload (quote ctypes-auto-parse-mode) "ctypes" "\
Toggle CTypes auto parse mode; search all new buffers for types.
With arg, turn types Auto Mode on if and only if arg is positive.

This a global minor mode, it does not have a private keymap, nor does
it add itself to the mode line.

Place the following in your startup file to enable this feature in
future sessions:

    (require 'ctypes)
    (ctypes-auto-parse-mode 1)

When activated, the functions in the hook `ctypes-auto-parse-mode-hook'
is called with no args." t nil)

(autoload (quote ctypes-read-file) "ctypes" "\
Load types previously saved with `ctypes-write-file'.
The name of the file is given by the optional argument FILE.
Should no file name be given the value of the variable `ctypes-file-name'
is used.

Please note that the types read will be added to the current types.

When preceded by C-u the display is not updated.

The third argument, NO-ERROR, determines whether or not we should
raise an error if there should be any problem loading the file.

Should the fourth argument, QUIETLY, be non-nil no messages are
generated when the file is loaded.

Return non-nil if new types are found." t nil)

;;;***

;;;### (autoloads (CUA-keypad-mode CUA-mode-on CUA-mode CUA-mode-bindings
;;;;;;  CUA-movement-key CUA-exchange-point-and-mark CUA-mode) "cua"
;;;;;;  "cua.el" (17212 38105))
;;; Generated autoloads from cua.el

(defvar CUA-mode nil "\
Non-nil means that CUA emulation mode is enabled.
In CUA mode, shifted movement keys highlight and extend the region.
When a region is highlighted, the binding of the C-x and C-c keys are
temporarily changed to work as Motif, MAC or MS-Windows cut and paste.
Also, insertion commands first delete the region and then insert.
This mode enables Transient Mark mode and it provides a superset of the
PC Selection Mode and Delete Selection Modes.

Setting this variable directly does not take effect;
use either \\[customize] or the function `CUA-mode'.")

(custom-add-to-group (quote CUA-mode) (quote CUA-mode) (quote custom-variable))

(custom-add-load (quote CUA-mode) (quote cua))

(autoload (quote CUA-exchange-point-and-mark) "cua" "\
Exchanges point and mark, but don't activate the mark.
Activates the mark if a prefix argument is given." t nil)

(autoload (quote CUA-movement-key) "cua" "\
Like `global-set-key' but also binds shifted KEY to COMMAND.
KEY should be a simple symbol or character, like home or ?\\C-e,
or a list like (control home)." nil nil)

(autoload (quote CUA-mode-bindings) "cua" "\
Define even more compatibility bindings.
Optional argument BIND identifies what bindings to add." nil nil)

(autoload (quote CUA-mode) "cua" "\
Toggle CUA keybinding mode.
When ON, C-x and C-c will cut and copy the selection if the selection
is active (i.e. the region is highlighted), and typed text replaces
the active selection. When OFF, typed text is just inserted at point.
If non-nil, the optional second argument EXTRA specifies additional
key bindings as defined by CUA-mode-bindings.
The following key bindings are made unless optional third argument 
NOBIND is non-nil:
 C-z  is undo
 C-v  is yank
 C-x C-x is CUA-exchange-point-and-mark which doesn't enable the mark
 C-space   starts/cancels the normal region 
 S-C-space sets/cancels the global marker
 S-return  starts a rectangular region, if repeated toggles between
          rectangle and normal region." t nil)

(autoload (quote CUA-mode-on) "cua" nil t nil)

(autoload (quote CUA-keypad-mode) "cua" "\
Set keypad bindings in function-key-map according to MODE.
If optional second argument NUMLOCK is non-nil, the NumLock On bindings
are changed. Otherwise, the NumLock Off binding are changed.

 Mode      Binding
 -------------------------------------------------------------
 'prefix   Command prefix argument, i.e.  M-0 .. M-9 and M--
 'S-cursor Bind shifted keypad keys to the shifted cursor movement keys.
 'cursor   Bind keypad keys to the cursor movement keys.
 'numeric  Plain numeric, i.e. 0 .. 9 and .  (or DECIMAL arg)
 'none     Removes all bindings for keypad keys in function-key-map.

If mode is 'numeric and the optional third argument DECIMAL is non-nil,
the decimal key on the keypad i<s mapped to DECIMAL instead of [.]." nil nil)

;;;***

;;;### (autoloads (cwebm-mode webm-mode) "cwebm" "cwebm.el" (17221
;;;;;;  28087))
;;; Generated autoloads from cwebm.el

(autoload (quote webm-mode) "cwebm" "\
Major mode like TeX mode plus \\[forward-module] and \\[backward-module].
Used for relative module movement. The automatic \" feature is disabled." t nil)

(autoload (quote cwebm-mode) "cwebm" "\
Major mode like LaTeX mode plus \\[forward-module] and \\[backward-module].
Used for relative module movement. The automatic \" feature is disabled." t nil)
(setq auto-mode-alist (cons '("\\.w$" . cwebm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ch$" . cwebm-mode) auto-mode-alist))

;;;***

;;;### (autoloads (dedicated-mode) "dedicated" "dedicated.el" (17246
;;;;;;  57581))
;;; Generated autoloads from dedicated.el

(autoload (quote dedicated-mode) "dedicated" "\
Toggle dedicated minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise." t nil)

;;;***

;;;### (autoloads (df) "df" "df.el" (16259 18879))
;;; Generated autoloads from df.el

(autoload (quote df) "df" "\
Enables display of space left on any PARTITION in mode-lines.
This display updates automatically every `df-refresh' seconds." t nil)

;;;***

;;;### (autoloads (diminished-modes diminish-undo diminish) "diminish"
;;;;;;  "diminish.el" (17246 57581))
;;; Generated autoloads from diminish.el

(autoload (quote diminish) "diminish" "\
Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish 'jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have abbrev-mode enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space.

If ANNOTATE-FLAG is nil or omitted, the normal case in interactive use, then
the variable `diminished-minor-modes' will be modified to reflect the change." t nil)

(autoload (quote diminish-undo) "diminish" "\
Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked M-x diminish).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo 'diminished-modes).

If ANNOTATE-FLAG is nil or omitted, the normal case in interactive use, then
the variable `diminished-minor-modes' will be modified to reflect the change." t nil)

(autoload (quote diminished-modes) "diminish" "\
Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor." t nil)

;;;***

;;;### (autoloads (edit-env) "edit-env" "edit-env.el" (17246 57581))
;;; Generated autoloads from edit-env.el

(autoload (quote edit-env) "edit-env" "\
Display, edit, delete and add environment variables." t nil)

;;;***

;;;### (autoloads (egocentric-update-regexp-list egocentric-mode-off
;;;;;;  egocentric-mode-on egocentric-mode) "egocentric" "egocentric.el"
;;;;;;  (16259 18879))
;;; Generated autoloads from egocentric.el

(autoload (quote egocentric-mode) "egocentric" "\
Toggle egocentric mode.
Optional argument ARG is an optional boolean controling whether egocentric-mode should be turned on/off." t nil)

(autoload (quote egocentric-mode-on) "egocentric" "\
Turn Egocentric mode on." t nil)

(autoload (quote egocentric-mode-off) "egocentric" "\
Turn Egocentric mode off." t nil)

(autoload (quote egocentric-update-regexp-list) "egocentric" "\
Update ``egocentric-regexp-list'' from $USER and $NAME variables." t nil)

;;;***

;;;### (autoloads (ff-paths-install) "ff-paths" "ff-paths.el" (17194
;;;;;;  6223))
;;; Generated autoloads from ff-paths.el

(autoload (quote ff-paths-install) "ff-paths" "\
Install ff-paths as a `find-file-not-found-hooks' and to ffap package." nil nil)

;;;***

;;;### (autoloads (floatbg-mode) "floatbg" "floatbg.el" (16259 18880))
;;; Generated autoloads from floatbg.el

(autoload (quote floatbg-mode) "floatbg" "\
Toggle floatbg mode" t nil)

;;;***

;;;### (autoloads (folding-mode turn-on-folding-mode turn-off-folding-mode
;;;;;;  folding-mode-add-find-file-hook folding-keep-hooked folding-install-hooks
;;;;;;  folding-uninstall-hooks folding-mode-hook-no-regexp folding-mode-string
;;;;;;  folding-inside-mode-name folding-default-mouse-keys-function
;;;;;;  folding-default-keys-function) "folding" "folding.el" (16806
;;;;;;  38694))
;;; Generated autoloads from folding.el

(defvar folding-mode nil "\
When Non nil, Folding mode is active in the current buffer.")

(defvar folding-default-keys-function (quote folding-bind-default-keys) "\
*Function or list of functions used to define keys for Folding mode.
Possible values are:
  folding-bind-default-key
        The standard keymap.

  `folding-bind-backward-compatible-keys'
        Keys used by older versions of Folding mode. This function
        does not conform to Emacs 19.29 style conversions concerning
        key bindings. The prefix key is C - c

  `folding-bind-outline-compatible-keys'
        Define keys compatible with Outline mode.

  `folding-bind-foldout-compatible-keys'
        Define some extra keys compatible with Foldout.

All except `folding-bind-backward-compatible-keys' used the value of
the variable `folding-mode-prefix-key' as prefix the key.
The default is C - c @")

(defvar folding-default-mouse-keys-function (quote folding-bind-default-mouse) "\
*Function to bind default mouse keys to `folding-mode-map'.")

(defvar folding-inside-mode-name "Fld" "\
*Mode line addition to show inside levels of 'fold' .")

(defvar folding-mode-string "Fld" "\
*The minor mode string displayed when mode is on.")

(defvar folding-mode-hook-no-regexp "RMAIL" "\
*Regexp which disable automatic folding mode turn on for certain files.")

(defvar folding-mode-marks-alist nil "\
List of (major-mode . fold mark) default combinations to use.
When Folding mode is started, the major mode is checked, and if there
are fold marks for that major mode stored in `folding-mode-marks-alist',
those marks are used by default. If none are found, the default values
of \"{{{ \" and \"}}}\" are used.

Use function  `folding-add-to-marks-list' to add more fold marks. The function
also explains the alist use in details.

Use function `folding-set-local-variables' if you change the current mode's
folding marks during the session.")

(autoload (quote folding-uninstall-hooks) "folding" "\
Remove hooks set by folding." nil nil)

(autoload (quote folding-install-hooks) "folding" "\
Install folding hooks." nil nil)

(autoload (quote folding-keep-hooked) "folding" "\
Make sure hooks are in their places." nil nil)

(autoload (quote folding-mode-add-find-file-hook) "folding" "\
Append `folding-mode-find-file-hook' to the list `find-file-hooks'.

This has the effect that afterwards, when a folded file is visited, if
appropriate Emacs local variable entries are recognized at the end of
the file, Folding mode is started automatically.

If `inhibit-local-variables' is non-nil, this will not happen regardless
of the setting of `find-file-hooks'.

To declare a file to be folded, put `folded-file: t' in the file's
local variables. eg., at the end of a C source file, put:

/*
Local variables:
folded-file: t
*/

The local variables can be inside a fold." t nil)

(autoload (quote turn-off-folding-mode) "folding" "\
Turn off folding." nil nil)

(autoload (quote turn-on-folding-mode) "folding" "\
Turn on folding." nil nil)

(autoload (quote folding-mode) "folding" "\
A folding-editor-like minor mode. ARG INTER.

These are the basic commands that Folding mode provides:

\\{folding-mode-map}

Keys starting with `folding-mode-prefix-key'

\\{folding-mode-prefix-map}

     folding-convert-buffer-for-printing:
     `\\[folding-convert-buffer-for-printing]'
     Makes a ready-to-print, formatted, unfolded copy in another buffer.

     Read the documentation for the above functions for more information.

Overview

    Folds are a way of hierarchically organizing the text in a file, so
    that the text can be viewed and edited at different levels. It is
    similar to Outline mode in that parts of the text can be hidden from
    view. A fold is a region of text, surrounded by special \"fold marks\",
    which act like brackets, grouping the text. Fold mark pairs can be
    nested, and they can have titles. When a fold is folded, the text is
    hidden from view, except for the first line, which acts like a title
    for the fold.

    Folding mode is a minor mode, designed to cooperate with many other
    major modes, so that many types of text can be folded while they are
    being edited (eg., plain text, program source code, Texinfo, etc.).

Folding-mode function

    If Folding mode is not called interactively (`(interactive-p)' is nil),
    and it is called with two or less arguments, all of which are nil, then
    the point will not be altered if `folding-folding-on-startup' is set
    and `folding-whole-buffer' is called. This is generally not a good
    thing, as it can leave the point inside a hidden region of a fold, but
    it is required if the local variables set \"mode: folding\" when the
    file is first read (see `hack-local-variables').

    Not that you should ever want to, but to call Folding mode from a
    program with the default behavior (toggling the mode), call it with
    something like `(folding-mode nil t)'.

Fold marks

    For most types of folded file, lines representing folds have \"{{{\"
    near the beginning. To enter a fold, move the point to the folded line
    and type `\\[folding-shift-in]'. You should no longer be able to see
    the rest of the file, just the contents of the fold, which you couldn't
    see before. You can use `\\[folding-shift-out]' to leave a fold, and
    you can enter and exit folds to move around the structure of the file.

    All of the text is present in a folded file all of the time. It is just
    hidden. Folded text shows up as a line (the top fold mark) with \"...\"
    at the end. If you are in a fold, the mode line displays \"inside n
    folds Narrow\", and because the buffer is narrowed you can't see outside
    of the current fold's text.

    By arranging sections of a large file in folds, and maybe subsections
    in sub-folds, you can move around a file quickly and easily, and only
    have to scroll through a couple of pages at a time. If you pick the
    titles for the folds carefully, they can be a useful form of
    documentation, and make moving though the file a lot easier. In
    general, searching through a folded file for a particular item is much
    easier than without folds.

Managing folds

    To make a new fold, set the mark at one end of the text you want in the
    new fold, and move the point to the other end. Then type
    `\\[folding-fold-region]'. The text you selected will be made into a
    fold, and the fold will be entered. If you just want a new, empty fold,
    set the mark where you want the fold, and then create a new fold there
    without moving the point. Don't worry if the point is in the middle of
    a line of text, `folding-fold-region' will not break text in the middle
    of a line. After making a fold, the fold is entered and the point is
    positioned ready to enter a title for the fold. Do not delete the fold
    marks, which are usually something like \"{{{\" and \"}}}\". There may
    also be a bit of fold mark which goes after the fold title.

    If the fold markers get messed up, or you just want to see the whole
    unfolded file, use `\\[folding-open-buffer]' to unfolded the whole
    file, so you can see all the text and all the marks. This is useful for
    checking/correcting unbalanced fold markers, and for searching for
    things. Use `\\[folding-whole-file]' to fold the buffer again.

    `folding-shift-out' will attempt to tidy the current fold just before
    exiting it. It will remove any extra blank lines at the top and bottom,
    (outside the fold marks). It will then ensure that fold marks exists,
    and if they are not, will add them (after asking). Finally, the number
    of blank lines between the fold marks and the contents of the fold is
    set to 1 (by default).

Folding package customizations

    If the fold marks are not set on entry to Folding mode, they are set to
    a default for current major mode, as defined by
    `folding-mode-marks-alist' or to \"{{{ \" and \"}}}\" if none are
    specified.

    To bind different commands to keys in Folding mode, set the bindings in
    the keymap `folding-mode-map'.

    The hooks `folding-mode-hook' and `<major-mode-name>-folding-hook' are
    called before folding the buffer and applying the key bindings in
    `folding-mode-map'. This is a good hook to set extra or different key
    bindings in `folding-mode-map'. Note that key bindings in
    `folding-mode-map' are only examined just after calling these hooks;
    new bindings in those maps only take effect when Folding mode is being
    started. The hook `folding-load-hook' is called when Folding mode is
    loaded into Emacs.

Mouse behavior

    If you want folding to detect point of actual mouse click, please see
    variable `folding-mouse-yank-at-p'.

    To customise the mouse actions, look at `folding-behave-table'." t nil)

;;;***

;;;### (autoloads (framepop-display-buffer framepop-enable framepop-disable)
;;;;;;  "framepop" "framepop.el" (16269 22307))
;;; Generated autoloads from framepop.el

(autoload (quote framepop-disable) "framepop" "\
Disable automatic pop-up temporary windows." t nil)

(autoload (quote framepop-enable) "framepop" "\
Enable automatic pop-up temporary windows." t nil)

(autoload (quote framepop-display-buffer) "framepop" "\
Display-buffer for FramePop.
Displays BUF in a separate frame -- the FramePop frame.
BUF bay be a buffer or a buffer name.

You can display a buffer in the FramePop frame with \\[framepop-display-buffer].

Several commands are available for manipulating the FramePop frame after
typing the keymap prefix (default F2).

\\{framepop-map}
" t nil)

;;;***

;;;### (autoloads (highlight-beyond-fill-column) "highlight-beyond-fill-column"
;;;;;;  "highlight-beyond-fill-column.el" (17246 57581))
;;; Generated autoloads from highlight-beyond-fill-column.el

(autoload (quote highlight-beyond-fill-column) "highlight-beyond-fill-column" "\
Setup this buffer to highlight beyond the `fill-column'." t nil)

;;;***

;;;### (autoloads (highlight-current-line-minor-mode) "highlight-current-line"
;;;;;;  "highlight-current-line.el" (16259 18881))
;;; Generated autoloads from highlight-current-line.el

(autoload (quote highlight-current-line-minor-mode) "highlight-current-line" "\
Toggle highlight-current-line minor mode.
With ARG, turn minor mode on if ARG is positive, off otherwise.
You can customize the face of the highlighted line and whether the entire
line is hightlighted by customizing the group highlight-current-line." t nil)

;;;***

;;;### (autoloads (home-end-end home-end-home) "home-end" "home-end.el"
;;;;;;  (17246 57581))
;;; Generated autoloads from home-end.el

(autoload (quote home-end-home) "home-end" "\
Go to beginning of line/window/buffer.
First hitting key goes to beginning of line, second in a row goes to
beginning of window, third in a row goes to beginning of buffer." t nil)

(autoload (quote home-end-end) "home-end" "\
Go to end of line/window/buffer.
First hitting key goes to end of line, second in a row goes to end
of window, third in a row goes to end of buffer." t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize" "htmlize.el" (16285
;;;;;;  9287))
;;; Generated autoloads from htmlize.el

(autoload (quote htmlize-buffer) "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses." t nil)

(autoload (quote htmlize-region) "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details." t nil)

(autoload (quote htmlize-file) "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name." t nil)

(autoload (quote htmlize-many-files) "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file." t nil)

(autoload (quote htmlize-many-files-dired) "htmlize" "\
HTMLize dired-marked files." t nil)

;;;***

;;;### (autoloads (ibuffer) "ibuffer" "ibuffer.el" (16259 18881))
;;; Generated autoloads from ibuffer.el

(defsubst ibuffer-and-update (&optional other-window-p) "\
Like `ibuffer', but update the list of buffers too.
With optional prefix argument, use another window." (interactive "P") (ibuffer other-window-p nil nil t))

(defsubst ibuffer-and-update-other-window nil "\
Like `ibuffer-and-update', but use another window." (interactive) (ibuffer-and-update t))

(autoload (quote ibuffer) "ibuffer" "\
Begin using `ibuffer' to edit a list of buffers.
Type 'h' after entering ibuffer for more information.

Optional argument OTHER-WINDOW-P says to use another window.
Optional argument NAME specifies the name of the buffer; it defaults
to \"*Ibuffer*\".
Optional argument QUALIFIERS is an initial set of limiting qualifiers
to use; see `ibuffer-limiting-qualifiers'." t nil)

;;;***

;;;### (autoloads (ido-read-directory-name ido-read-file-name ido-dired
;;;;;;  ido-insert-file ido-write-file ido-find-file-other-frame
;;;;;;  ido-display-file ido-find-file-read-only-other-frame ido-find-file-read-only-other-window
;;;;;;  ido-find-file-read-only ido-find-alternate-file ido-find-file-other-window
;;;;;;  ido-find-file ido-find-file-in-dir ido-switch-buffer-other-frame
;;;;;;  ido-insert-buffer ido-kill-buffer ido-display-buffer ido-switch-buffer-other-window
;;;;;;  ido-switch-buffer ido-read-buffer ido-mode ido-enabled) "ido"
;;;;;;  "ido.el" (17213 20908))
;;; Generated autoloads from ido.el

(defvar ido-enabled nil "\
Determines for which functional group (buffer and files) ido behavior
should be enabled. The following values are possible:
- 'buffer: Turn only on ido buffer behavior (switching, killing,
  displaying...) 
- 'file: Turn only on ido file behavior (finding, writing, inserting...)
- 'both: Turn on ido buffer and file behavior.
- nil: Turn off any ido switching.

Setting this variable directly does not take effect;
use either \\[customize] or the function `ido-mode'.")

(custom-add-to-group (quote ido) (quote ido-enabled) (quote custom-variable))

(custom-add-load (quote ido-enabled) (quote ido))

(autoload (quote ido-mode) "ido" "\
Toggle ido speed-ups on or off.
With ARG, turn ido speed-up on if arg is positive, off otherwise.
If second argument NOBIND is non-nil, no keys are rebound; otherwise,
turning on ido-mode will modify the default keybindings for the 
find-file and switch-to-buffer families of commands to the ido
versions of these functions.
However, if second arg equals 'files, bind only for files, or if it 
equals 'buffers, bind only for buffers.
This function also adds a hook to the minibuffer." t nil)

(autoload (quote ido-read-buffer) "ido" "\
Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.  
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected.
If INITIAL is non-nil, it specifies the initial input string." nil nil)

(autoload (quote ido-switch-buffer) "ido" "\
Switch to another buffer.
The buffer is displayed according to `ido-default-buffer-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.

As you type in a string, all of the buffers matching the string are
displayed if substring-matching is used (default). Look at
`ido-enable-prefix' and `ido-toggle-prefix'. When you have found the
buffer you want, it can then be selected. As you type, most keys have their
normal keybindings, except for the following: \\<ido-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[ido-select-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[ido-edit-input] Edit input string.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of buffer names.
\\[ido-completion-help] Show list of matching buffers in separate window.
\\[ido-enter-find-file] Drop into ido-find-file.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list.
\\[ido-toggle-ignore] Toggle ignoring buffers listed in `ido-ignore-buffers'." t nil)

(autoload (quote ido-switch-buffer-other-window) "ido" "\
Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'." t nil)

(autoload (quote ido-display-buffer) "ido" "\
Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'." t nil)

(autoload (quote ido-kill-buffer) "ido" "\
Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'." t nil)

(autoload (quote ido-insert-buffer) "ido" "\
Insert contents of a buffer in current buffer after point.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'." t nil)

(autoload (quote ido-switch-buffer-other-frame) "ido" "\
Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'." t nil)

(autoload (quote ido-find-file-in-dir) "ido" "\
Switch to another file starting from DIR." t nil)

(autoload (quote ido-find-file) "ido" "\
Edit file with name obtained via minibuffer.
The file is displayed according to `ido-default-file-method' -- the
default is to show it in the same window, unless it is already
visible in another frame.

The file name is selected interactively by typing a substring. As you type
in a string, all of the filenames matching the string are displayed if
substring-matching is used (default). Look at `ido-enable-prefix' and
`ido-toggle-prefix'. When you have found the filename you want, it can
then be selected. As you type, most keys have their normal keybindings,
except for the following: \\<ido-mode-map>

RET Select the file at the front of the list of matches.  If the
list is empty, possibly prompt to create new file.

\\[ido-select-text] Select the current prompt as the buffer or file.
If no buffer or file is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all files.  If there is only one match, select that file.
If there is no common suffix, show a list of all matching files
in a separate window.
\\[ido-edit-input] Edit input string (including path).
\\[ido-prev-work-directory] or \\[ido-next-work-directory] go to previous/next directory in work directory history.
\\[ido-merge-work-directories] search for file in the work directory history.
\\[ido-forget-work-directory] removes current directory from the work directory history.
\\[ido-prev-work-file] or \\[ido-next-work-file] cycle through the work file history.
\\[ido-wide-find-file] and \\[ido-wide-find-dir] prompts and uses find to locate files or directories.
\\[ido-make-directory] prompts for a directory to create in current directory.
\\[ido-fallback-command] Fallback to non-ido version of current command.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-prefix] Toggle between substring and prefix matching.
\\[ido-toggle-case] Toggle case-sensitive searching of file names.
\\[ido-toggle-vc] Toggle version control for this file.
\\[ido-toggle-literal] Toggle literal reading of this file.
\\[ido-completion-help] Show list of matching files in separate window.
\\[ido-toggle-ignore] Toggle ignoring files listed in `ido-ignore-files'." t nil)

(autoload (quote ido-find-file-other-window) "ido" "\
Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-find-alternate-file) "ido" "\
Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-find-file-read-only) "ido" "\
Edit file read-only with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-find-file-read-only-other-window) "ido" "\
Edit file read-only in other window with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-find-file-read-only-other-frame) "ido" "\
Edit file read-only in other frame with name obtained via minibuffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-display-file) "ido" "\
Display a file in another window but don't select it.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-find-file-other-frame) "ido" "\
Switch to another file and show it in another frame.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-write-file) "ido" "\
Write current buffer to a file.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-insert-file) "ido" "\
Insert contents of file in current buffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-dired) "ido" "\
Call dired the ido way.
The directory is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'." t nil)

(autoload (quote ido-read-file-name) "ido" "\
Read file name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters." nil nil)

(autoload (quote ido-read-directory-name) "ido" "\
Read directory name, prompting with PROMPT and completing in directory DIR.
See `read-file-name' for additional parameters." nil nil)

;;;***

;;;### (autoloads (joc-toggle-buffer) "joc-toggle-buffer" "joc-toggle-buffer.el"
;;;;;;  (17246 57581))
;;; Generated autoloads from joc-toggle-buffer.el

(autoload (quote joc-toggle-buffer) "joc-toggle-buffer" "\
Switch to previous active buffer." t nil)

;;;***

;;;### (autoloads (joc-toggle-case-by-region joc-toggle-case-by-word-backwards
;;;;;;  joc-toggle-case-by-word joc-toggle-case-backwards joc-toggle-case)
;;;;;;  "joc-toggle-case" "joc-toggle-case.el" (17246 57581))
;;; Generated autoloads from joc-toggle-case.el

(autoload (quote joc-toggle-case) "joc-toggle-case" "\
Toggle the case of the character under point.
If called with a PREFIX argument, it toggles that many
characters (see joc-toggle-case-stop-at-eol).  If the prefix is
negative, the case of the character before point is toggled, and
if called with a prefix argument, N characters before point will
have their case toggled (see also joc-toggle-case-backwards)." t nil)

(autoload (quote joc-toggle-case-backwards) "joc-toggle-case" "\
Convenience function to toggle case of character preceeding point.
This is the same as calling joc-toggle-case with a negative
prefix (and is in fact implemented that way)." t nil)

(autoload (quote joc-toggle-case-by-word) "joc-toggle-case" "\
Similar to joc-toggle-case except that the count (supplied by
the prefix argument) is of the number of words, not letters, to
be toggled.  It will start from point and move to the end of
the first word at a minimum, and then take whole words from
there.  If called with a negative prefix, then from point to
beginning of current word will have their case toggled, going
backwards for N words (see also
joc-toggle-case-by-word-backwards).  Note that the
joc-toggle-case-stop-at-eol setting will be honored." t nil)

(autoload (quote joc-toggle-case-by-word-backwards) "joc-toggle-case" "\
Convenience function to toggle case by word, backwards.
This is the same as calling joc-toggle-case-by-word with a
negative prefix (and is in fact implemented that way)." t nil)

(autoload (quote joc-toggle-case-by-region) "joc-toggle-case" "\
Toggles the case of all characters in the current region." t nil)

;;;***

;;;### (autoloads (keydef) "keydef" "keydef.el" (16259 18882))
;;; Generated autoloads from keydef.el

(autoload (quote keydef) "keydef" "\
Define the key sequence SEQ, written in kbd form, to run CMD.
CMD is automatically wrapped in an anonymous interactive function if it
is Emacs Lisp code rather than a command name. SEQ may also have the form
\(MODE SEQ) where the car is a mode name; for example

  (keydef (latex \"C-c %\") comment-region)

means to define the given key in latex-mode-map. And this will work even
if latex-mode is not loaded yet, provided that it is possible to deduce
the file that it will be loaded from, either from the autoload info or
by searching for a matching file name in the Emacs load path.

For best results, the \"mode name\" that you use here should yield the
proper foo-mode-map symbol when \"-mode-map\" is appended; although
this will normally match the mode name as given in the mode line,
Shell-script is one example I can think of where it doesn't---the map is
named sh-mode-map. The common cases that I know about, including
shell-script-mode and latex-mode, are handled as exceptions through the
variable mode-map-alist. But for other cases you will need to look up
the name of the mode-map that goes with the given mode." nil (quote macro))

;;;***

;;;### (autoloads (lcomp-activate-advices) "lcomp" "lcomp.el" (17246
;;;;;;  57581))
;;; Generated autoloads from lcomp.el

(autoload (quote lcomp-activate-advices) "lcomp" "\
Activate lcomp advices if ON is non-nil, disable otherwise." t nil)

;;;***

;;;### (autoloads (marker-visit-truncate-mark-ring marker-visit-next
;;;;;;  marker-visit-prev) "marker-visit" "marker-visit.el" (17246
;;;;;;  57581))
;;; Generated autoloads from marker-visit.el

(autoload (quote marker-visit-prev) "marker-visit" "\
From point, visit the nearest mark earlier in the buffer." t nil)

(autoload (quote marker-visit-next) "marker-visit" "\
From point, visit the nearest mark later in the buffer." t nil)

(autoload (quote marker-visit-truncate-mark-ring) "marker-visit" "\
Truncate the `mark-ring'." t nil)

;;;***

;;;### (autoloads (matlab-shell matlab-mode) "matlab" "matlab.el"
;;;;;;  (17238 24754))
;;; Generated autoloads from matlab.el

(autoload (quote matlab-mode) "matlab" "\
MATLAB-mode is a major mode for editing MATLAB dot-m files.
\\<matlab-mode-map>
Convenient editing commands are:
 \\[matlab-comment-region]   - Comment/Uncomment out a region of code.
 \\[matlab-fill-comment-line] - Fill the current comment line.
 \\[matlab-fill-region] - Fill code and comments in region.
 \\[matlab-fill-paragraph]     - Refill the current command or comment.
 \\[matlab-complete-symbol]   - Symbol completion of matlab symbolsbased on the local syntax.
 \\[matlat-indent-sexp] - Indent syntactic block of code.

Convenient navigation commands are:
 \\[matlab-beginning-of-command]   - Move to the beginning of a command.
 \\[matlab-end-of-command]   - Move to the end of a command.
 \\[matlab-beginning-of-defun] - Move to the beginning of a function.
 \\[matlab-end-of-defun] - Move do the end of a function.
 \\[matlab-forward-sexp] - Move forward over a syntactic block of code.
 \\[matlab-backward-sexp] - Move backwards over a syntactic block of code.

Convenient template insertion commands:
 \\[tempo-template-matlab-function] - Insert a function definition.
 \\[tempo-template-matlab-if] - Insert an IF END block.
 \\[tempo-template-matlab-for] - Insert a FOR END block.
 \\[tempo-template-matlab-switch] - Insert a SWITCH END statement.
 \\[matlab-insert-next-case] - Insert the next CASE condition in a SWITCH.
 \\[matlab-insert-end-block] - Insert a matched END statement.  With optional ARG, reindent.
 \\[matlab-stringify-region] - Convert plaintext in region to a string with correctly quoted chars.

Variables:
  `matlab-indent-level'		Level to indent blocks.
  `matlab-cont-level'		Level to indent continuation lines.
  `matlab-cont-requires-ellipsis' Does your MATLAB support implied elipsis.
  `matlab-case-level'		Level to unindent case statements.
  `matlab-indent-past-arg1-functions'
                                Regexp of functions to indent past the first
                                  argument on continuation lines.
  `matlab-maximum-indents'      List of maximum indents during lineups.
  `matlab-comment-column'       Goal column for on-line comments.
  `fill-column'			Column used in auto-fill.
  `matlab-indent-function-body' If non-nil, indents body of MATLAB functions.
  `matlab-functions-have-end'	If non-nil, MATLAB functions terminate with end.
  `matlab-return-function'	Customize RET handling with this function.
  `matlab-auto-fill'            Non-nil, do auto-fill at startup.
  `matlab-fill-code'            Non-nil, auto-fill code.
  `matlab-fill-strings'         Non-nil, auto-fill strings.
  `matlab-verify-on-save-flag'  Non-nil, enable code checks on save.
  `matlab-highlight-block-match-flag'
                                Enable matching block begin/end keywords.
  `matlab-vers-on-startup'	If t, show version on start-up.
  `matlab-handle-simulink'      If t, enable simulink keyword highlighting.

All Key Bindings:
\\{matlab-mode-map}" t nil)

(autoload (quote matlab-shell) "matlab" "\
Create a buffer with MATLAB running as a subprocess.

MATLAB shell cannot work on the MS Windows platform because MATLAB is not
a console application." t nil)

;;;***

;;;### (autoloads (muttrc-mode) "muttrc-mode" "muttrc-mode.el" (17246
;;;;;;  57581))
;;; Generated autoloads from muttrc-mode.el

(autoload (quote muttrc-mode) "muttrc-mode" "\
Major mode for editing Muttrc files.
This function ends by invoking the function(s) `muttrc-mode-hook'.

\\{muttrc-mode-map}
" t nil)

;;;***

;;;### (autoloads (newsticker-show-news newsticker-start) "newsticker"
;;;;;;  "newsticker.el" (17246 57581))
;;; Generated autoloads from newsticker.el

(autoload (quote newsticker-start) "newsticker" "\
Start the newsticker.
Start the timers for display and retrieval.  If the newsticker, i.e. the
timers, are running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.
Run `newsticker-start-hook' if newsticker was not running already." t nil)

(autoload (quote newsticker-show-news) "newsticker" "\
Switch to newsticker buffer.  You may want to bind this to a key." t nil)

;;;***

;;;### (autoloads (nuke-trailing-whitespace) "nuke-trailing-whitespace"
;;;;;;  "nuke-trailing-whitespace.el" (17246 57581))
;;; Generated autoloads from nuke-trailing-whitespace.el

(autoload (quote nuke-trailing-whitespace) "nuke-trailing-whitespace" "\
Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on `write-file-hooks'.

Unless called interactively, this function uses
`nuke-trailing-whitespace-p' to determine how to behave.
However, even if this variable is t, this function will query for
replacement if the buffer is read-only." t nil)

;;;***

;;;### (autoloads (obfuscate-url) "obfusurl" "obfusurl.el" (16259
;;;;;;  18882))
;;; Generated autoloads from obfusurl.el

(autoload (quote obfuscate-url) "obfusurl" "\
Obfuscate an URL under `point'.

This might be useful if you're writing out an URL for someone but the URL
itself is a spoiler. The URL will still work but it won't be readable (by
most mortals anyway)." t nil)

;;;***

;;;### (autoloads (pack-windows) "pack-windows" "pack-windows.el"
;;;;;;  (16313 9500))
;;; Generated autoloads from pack-windows.el

(autoload (quote pack-windows) "pack-windows" "\
Resize all windows vertically to display as much information as possible.

Only windows that are on the left edge of the frame are taken into
account. The vertical space available in the frame is first divided
among all these windows. Then any window requireing less lines than it
got to display its whole buffer is shrinked, and the freed space is
divided equally among all the other windows.

If some vertical space remains afterwards, it is given in totality to
the currently selected window.

Do not shrink any window to less than `window-min-height'.

Shrink windows iteratively, performing at most `pack-windows-max-iteration'
iterations. The number of iterations really performed will be
displayed in the echo area if `pack-windows-verbose' is non-nil." t nil)

;;;***

;;;### (autoloads (perldoc-perl-hook perldoc-at-point perldoc) "perldoc"
;;;;;;  "perldoc.el" (17246 57581))
;;; Generated autoloads from perldoc.el

(autoload (quote perldoc) "perldoc" "\
Run perldoc on the given STRING.
If the string is a recognised function then we can call `perldoc-function',
otherwise we call `perldoc-module'." t nil)

(autoload (quote perldoc-at-point) "perldoc" "\
Call `perldoc' for string at point." t nil)

(autoload (quote perldoc-perl-hook) "perldoc" "\
A hook which binds F1 to `perldoc-at-point'." nil nil)

;;;***

;;;### (autoloads (project-add) "projects" "projects.el" (17246 57581))
;;; Generated autoloads from projects.el

(autoload (quote project-add) "projects" "\
Add the project named NAME with root directory DIRECTORY." t nil)

;;;***

;;;### (autoloads (protect-process-buffer-from-kill-mode protect-buffer-from-kill-mode)
;;;;;;  "protbuf" "protbuf.el" (17246 57581))
;;; Generated autoloads from protbuf.el

(defvar protect-buffer-from-kill-mode nil "\
*If non-nil, then prevent buffer from being accidentally killed.
This variable is local to all buffers.")

(defvar protect-process-buffer-from-kill-mode nil "\
*If non-nil, then protect buffer with live process from being killed.
This variable is local to all buffers.")

(defvar protect-process-buffer-from-kill-preserve-function nil "\
*Function to run to determine whether to kill a process buffer.
If function returns non-nil, buffer is preserved.  Otherwise, the buffer
may be killed.

If this variable is undefined, default action is to test whether a process
object is using this buffer as a process buffer.

This variable is buffer-local when set.")

(autoload (quote protect-buffer-from-kill-mode) "protbuf" "\
Toggle `kill-buffer' protection on current buffer.
Optionally, set a PREFIX argument to set or unset protection, and specify
alternate BUFFER." t nil)

(autoload (quote protect-process-buffer-from-kill-mode) "protbuf" "\
Toggle `kill-buffer' protection on current buffer with active process.
Protection only applies as long as the buffer has an active process.
Optionally, set a PREFIX argument to set or unset protection, and specify
alternate BUFFER." t nil)

;;;***

;;;### (autoloads (protocols-clear-cache protocols-lookup) "protocols"
;;;;;;  "protocols.el" (16259 18882))
;;; Generated autoloads from protocols.el

(autoload (quote protocols-lookup) "protocols" "\
Find a protocol and display its details." t nil)

(autoload (quote protocols-clear-cache) "protocols" "\
Clear the protocols \"cache\"." t nil)

;;;***

;;;### (autoloads (rfcview-mode rfcview-customize) "rfcview" "rfcview.el"
;;;;;;  (16313 9500))
;;; Generated autoloads from rfcview.el

(autoload (quote rfcview-customize) "rfcview" nil t nil)

(autoload (quote rfcview-mode) "rfcview" "\
Major mode for viewing Internet RFCs.

http://www.neilvandyke.org/rfcview/

Key bindings:
\\{rfcview-mode-map}" t nil)

;;;***

;;;### (autoloads (services-clear-cache services-lookup) "services"
;;;;;;  "services.el" (16259 18882))
;;; Generated autoloads from services.el

(autoload (quote services-lookup) "services" "\
Find a service and display its details." t nil)

(autoload (quote services-clear-cache) "services" "\
Clear the services \"cache\"." t nil)

;;;***

;;;### (autoloads (turn-on-setnu-mode setnu-mode) "setnu" "setnu.el"
;;;;;;  (17246 57581))
;;; Generated autoloads from setnu.el

(autoload (quote setnu-mode) "setnu" "\
Toggle `setnu-mode'.
With prefix argument ARG, turn `setnu-mode' on if argument is positive.
When `setnu-mode' is enabled, a line number will appear at the left
margin of each line." t nil)

(autoload (quote turn-on-setnu-mode) "setnu" "\
Turn on `setnu-mode'.
Useful for adding to a `major-mode' hook variable.
Example:
    (add-hook 'text-mode-hook 'turn-on-setnu-mode)
to automatically turn on line numbering when enterting `text-mode'." nil nil)

;;;***

;;;### (autoloads (shell-command-completion-mode) "shell-command"
;;;;;;  "shell-command.el" (17221 28092))
;;; Generated autoloads from shell-command.el

(autoload (quote shell-command-completion-mode) "shell-command" "\
Enable or disable tab-completion for some commands.
The commands are `shell-command', `shell-command-on-region', `grep',
`grep-find' and `compile'." t nil)

;;;***

;;;### (autoloads (sm-add-all-headers sm-add-random-header) "silly-mail"
;;;;;;  "silly-mail.el" (17246 57581))
;;; Generated autoloads from silly-mail.el

(autoload (quote sm-add-random-header) "silly-mail" "\
Insert a random silly mail header.
The choice of available headers is taken from `sm-mail-header-table'.
If a random header was already inserted, it it removed in favor of a new one." t nil)

(autoload (quote sm-add-all-headers) "silly-mail" "\
Insert one of every kind of silly mail header defined.
The choice of available headers is taken from `sm-mail-header-table'." t nil)

;;;***

;;;### (autoloads (slang-mode) "slang-mode" "slang-mode.el" (17211
;;;;;;  12498))
;;; Generated autoloads from slang-mode.el

(autoload (quote slang-mode) "slang-mode" "\
Major mode for editing slang scripts.
The following keys are bound:
\\{slang-mode-map}
" t nil)

;;;***

;;;### (autoloads (sys-apropos) "sys-apropos" "sys-apropos.el" (16259
;;;;;;  18883))
;;; Generated autoloads from sys-apropos.el

(autoload (quote sys-apropos) "sys-apropos" "\
Ask the system apropos command for man-pages matching QUERY." t nil)

;;;***

;;;### (autoloads (tabbar-local-mode tabbar-mode tabbar-forward-tab
;;;;;;  tabbar-backward-tab tabbar-forward-group tabbar-backward-group
;;;;;;  tabbar-forward tabbar-backward) "tabbar" "tabbar.el" (17212
;;;;;;  38105))
;;; Generated autoloads from tabbar.el

(autoload (quote tabbar-backward) "tabbar" "\
Select the previous available tab.
Depend on the setting of the option `tabbar-cycling-scope'." t nil)

(autoload (quote tabbar-forward) "tabbar" "\
Select the next available tab.
Depend on the setting of the option `tabbar-cycling-scope'." t nil)

(autoload (quote tabbar-backward-group) "tabbar" "\
Go to selected tab in the previous available group." t nil)

(autoload (quote tabbar-forward-group) "tabbar" "\
Go to selected tab in the next available group." t nil)

(autoload (quote tabbar-backward-tab) "tabbar" "\
Select the previous visible tab." t nil)

(autoload (quote tabbar-forward-tab) "tabbar" "\
Select the next visible tab." t nil)

(defvar tabbar-mode nil "\
Non-nil if Tabbar mode is enabled.
See the command `tabbar-mode' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `tabbar-mode'.")

(custom-add-to-group (quote tabbar) (quote tabbar-mode) (quote custom-variable))

(custom-add-load (quote tabbar-mode) (quote tabbar))

(autoload (quote tabbar-mode) "tabbar" "\
Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled." t nil)

(autoload (quote tabbar-local-mode) "tabbar" "\
Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When on and tab bar global mode is on, if a buffer local value of
`header-line-format' exists, it is saved, then the local header line
is killed to show the tab bar.  When off, the saved local value of the
header line is restored, hiding the tab bar." t nil)

;;;***

;;;### (autoloads (table-version table-release table-capture table-delete-column
;;;;;;  table-delete-row table-insert-sequence table-generate-source
;;;;;;  table-query-dimension table-fixed-width-mode table-justify-column
;;;;;;  table-justify-row table-justify-cell table-justify table-split-cell
;;;;;;  table-split-cell-horizontally table-split-cell-vertically
;;;;;;  table-span-cell table-backward-cell table-forward-cell table-narrow-cell
;;;;;;  table-widen-cell table-shorten-cell table-heighten-cell table-unrecognize-cell
;;;;;;  table-recognize-cell table-unrecognize-table table-recognize-table
;;;;;;  table-unrecognize-region table-recognize-region table-unrecognize
;;;;;;  table-recognize table-insert-row-column table-insert-column
;;;;;;  table-insert-row table-insert table-point-left-cell-hook
;;;;;;  table-point-entered-cell-hook table-load-hook table-cell-map-hook)
;;;;;;  "table" "table.el" (17246 57581))
;;; Generated autoloads from table.el

(defvar table-cell-map-hook nil "\
*Normal hooks run when finishing construction of `table-cell-map'.
User can modify `table-cell-map' by adding custom functions here.")

(defvar table-load-hook nil "\
*List of functions to be called after the table is first loaded.")

(defvar table-point-entered-cell-hook nil "\
*List of functions to be called after point entered a table cell.")

(defvar table-point-left-cell-hook nil "\
*List of functions to be called after point left a table cell.")

(autoload (quote table-insert) "table" "\
Insert an editable text table.
Insert a table of specified number of COLUMNS and ROWS.  Optional
parameter CELL-WIDTH and CELL-HEIGHT can specify the size of each
cell.  The cell size is uniform across the table if the specified size
is a number.  They can be a list of numbers to specify different size
for each cell.  When called interactively, the list of number is
entered by simply listing all the numbers with space characters
delimiting them.

Examples:

\\[table-insert] inserts a table at the current point location.

Suppose we have the following situation where `-!-' indicates the
location of point.

    -!-

Type \\[table-insert] and hit ENTER key.  As it asks table
specification, provide 3 for number of columns, 1 for number of rows,
5 for cell width and 1 for cell height.  Now you shall see the next
table and the point is automatically moved to the beginning of the
first cell.

    +-----+-----+-----+
    |-!-  |     |     |
    +-----+-----+-----+

Inside a table cell, there are special key bindings. \\<table-cell-map>

M-9 \\[table-widen-cell] (or \\[universal-argument] 9 \\[table-widen-cell]) widens the first cell by 9 character
width, which results as

    +--------------+-----+-----+
    |-!-           |     |     |
    +--------------+-----+-----+

Type TAB \\[table-widen-cell] then type TAB M-2 M-7 \\[table-widen-cell] (or \\[universal-argument] 2 7 \\[table-widen-cell]).  Typing
TAB moves the point forward by a cell. The result now looks like this:

    +--------------+------+--------------------------------+
    |              |      |-!-                             |
    +--------------+------+--------------------------------+

If you knew each width of the columns prior to the table creation,
what you could have done better was to have had given the complete
width information to `table-insert'.

Cell width(s): 14 6 32

instead of 

Cell width(s): 5

This would have eliminated the previously mentioned width adjustment
work all together.

If the point is in the last cell type S-TAB S-TAB to move it to the
first cell.  Now type \\[table-heighten-cell] which heighten the row by a line.

    +--------------+------+--------------------------------+
    |-!-           |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Type \\[table-insert-row-column] and tell it to insert a row.

    +--------------+------+--------------------------------+
    |-!-           |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Move the point under the table as shown below.

    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    -!-

Type M-x table-insert-row instead of \\[table-insert-row-column].  \\[table-insert-row-column] does not work
when the point is outside of the table.  This insertion at
outside of the table effectively appends a row at the end.

    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |-!-           |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Text editing inside the table cell produces reasonably expected
results.

    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+
    |              |      |Text editing inside the table   |
    |              |      |cell produces reasonably        |
    |              |      |expected results.-!-            |
    +--------------+------+--------------------------------+
    |              |      |                                |
    |              |      |                                |
    +--------------+------+--------------------------------+

Inside a table cell has a special keymap.

\\{table-cell-map}
" t nil)

(autoload (quote table-insert-row) "table" "\
Insert N table row(s).
When point is in a table the newly inserted row(s) are placed above
the current row.  When point is outside of the table it must be below
the table within the table width range, then the newly created row(s)
are appended at the bottom of the table." t nil)

(autoload (quote table-insert-column) "table" "\
Insert N table column(s).
When point is in a table the newly inserted column(s) are placed left
of the current column.  When point is outside of the table it must be
right side of the table within the table height range, then the newly
created column(s) are appended at the right of the table." t nil)

(autoload (quote table-insert-row-column) "table" "\
Insert row(s) or column(s).
See `table-insert-row' and `table-insert-column'." t nil)

(autoload (quote table-recognize) "table" "\
Recognize all tables within the current buffer and activate them.
Scans the entire buffer and recognizes valid table cells.  If the
optional numeric prefix argument ARG is negative the tables in the
buffer become inactive, meaning the tables become plain text and loses
all the table specific features." t nil)

(autoload (quote table-unrecognize) "table" nil t nil)

(autoload (quote table-recognize-region) "table" "\
Recognize all tables within region.
BEG and END specify the region to work on.  If the optional numeric
prefix argument ARG is negative the tables in the region become
inactive, meaning the tables become plain text and lose all the table
specific features." t nil)

(autoload (quote table-unrecognize-region) "table" nil t nil)

(autoload (quote table-recognize-table) "table" "\
Recognize a table at point.
If the optional numeric prefix argument ARG is negative the table
becomes inactive, meaning the table becomes plain text and loses all
the table specific features." t nil)

(autoload (quote table-unrecognize-table) "table" nil t nil)

(autoload (quote table-recognize-cell) "table" "\
Recognize a table cell that contains current point.
Probe the cell dimension and prepare the cell information.  The
optional two arguments FORCE and NO-COPY are for internal use only and
must not be specified.  When the optional numeric prefix argument ARG
is negative the cell becomes inactive, meaning that the cell becomes
plain text and loses all the table specific features." t nil)

(autoload (quote table-unrecognize-cell) "table" nil t nil)

(autoload (quote table-heighten-cell) "table" "\
Heighten the current cell by N lines by expanding the cell vertically.
Heightening is done by adding blank lines at the bottom of the current
cell.  Other cells aligned horizontally with the current one are also
heightened in order to keep the rectangular table structure.  The
optional argument NO-COPY is internal use only and must not be
specified." t nil)

(autoload (quote table-shorten-cell) "table" "\
Shorten the current cell by N lines by shrinking the cell vertically.
Shortening is done by removing blank lines from the bottom of the cell
and possibly from the top of the cell as well.  Therefor, the cell
must have some bottom/top blank lines to be shorten effectively.  This
is applicable to all the cells aligned horizontally with the current
one because they are also shortened in order to keep the rectangular
table structure." t nil)

(autoload (quote table-widen-cell) "table" "\
Widen the current cell by N columns and expand the cell horizontally.
Some other cells in the same table are widen as well to keep the
table's rectangle structure." t nil)

(autoload (quote table-narrow-cell) "table" "\
Narrow the current cell by N columns and shrink the cell horizontally.
Some other cells in the same table are narrowed as well to keep the
table's rectangle structure." t nil)

(autoload (quote table-forward-cell) "table" "\
Move point forward to the beginning of the next cell.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N cells.
Do not specify NO-RECOGNIZE and UNRECOGNIZE. They are for internal use only.

Sample Cell Traveling Order (In Irregular Table Cases)

You can actually try how it works in this buffer.  Press
\\[table-recognize] and go to cells in the following tables and press
\\[table-forward-cell] or TAB key.

+-----+--+  +--+-----+  +--+--+--+  +--+--+--+  +---------+  +--+---+--+
|0    |1 |  |0 |1    |  |0 |1 |2 |  |0 |1 |2 |  |0        |  |0 |1  |2 |
+--+--+  |  |  +--+--+  +--+  |  |  |  |  +--+  +----+----+  +--+-+-+--+
|2 |3 |  |  |  |2 |3 |  |3 +--+  |  |  +--+3 |  |1   |2   |  |3   |4   |
|  +--+--+  +--+--+  |  +--+4 |  |  |  |4 +--+  +--+-+-+--+  +----+----+
|  |4    |  |4    |  |  |5 |  |  |  |  |  |5 |  |3 |4  |5 |  |5        |
+--+-----+  +-----+--+  +--+--+--+  +--+--+--+  +--+---+--+  +---------+

+--+--+--+  +--+--+--+  +--+--+--+  +--+--+--+
|0 |1 |2 |  |0 |1 |2 |  |0 |1 |2 |  |0 |1 |2 |
|  |  |  |  |  +--+  |  |  |  |  |  +--+  +--+
+--+  +--+  +--+3 +--+  |  +--+  |  |3 +--+4 |
|3 |  |4 |  |4 +--+5 |  |  |3 |  |  +--+5 +--+
|  |  |  |  |  |6 |  |  |  |  |  |  |6 |  |7 |
+--+--+--+  +--+--+--+  +--+--+--+  +--+--+--+

+--+--+--+  +--+--+--+  +--+--+--+--+  +--+-----+--+  +--+--+--+--+
|0 |1 |2 |  |0 |1 |2 |	|0 |1 |2 |3 |  |0 |1    |2 |  |0 |1 |2 |3 |
|  +--+  |  |  +--+  |	|  +--+--+  |  |  |     |  |  |  +--+--+  |
|  |3 +--+  +--+3 |  |	+--+4    +--+  +--+     +--+  +--+4    +--+
+--+  |4 |  |4 |  +--+	|5 +--+--+6 |  |3 +--+--+4 |  |5 |     |6 |
|5 +--+  |  |  +--+5 |	|  |7 |8 |  |  |  |5 |6 |  |  |  |     |  |
|  |6 |  |  |  |6 |  |	+--+--+--+--+  +--+--+--+--+  +--+-----+--+
+--+--+--+  +--+--+--+
" t nil)

(autoload (quote table-backward-cell) "table" "\
Move backward to the beginning of the previous cell.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N cells." t nil)

(autoload (quote table-span-cell) "table" "\
Span current cell into adjacent cell in DIRECTION.
DIRECTION is one of symbols; right, left, above or below." t nil)

(autoload (quote table-split-cell-vertically) "table" "\
Split current cell vertically.
Creates a cell above and a cell below the current point location." t nil)

(autoload (quote table-split-cell-horizontally) "table" "\
Split current cell horizontally.
Creates a cell on the left and a cell on the right of the current point location." t nil)

(autoload (quote table-split-cell) "table" "\
Split current cell in ORIENTATION.
ORIENTATION is a symbol either horizontally or vertically." t nil)

(autoload (quote table-justify) "table" "\
Justify contents of a cell, a row of cells or a column of cells.
WHAT is a symbol 'cell, 'row or 'column.  JUSTIFY is a symbol 'left,
'center, 'right, 'top, 'middle, 'bottom or 'none." t nil)

(autoload (quote table-justify-cell) "table" "\
Justify cell contents.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or 'top,
'middle, 'bottom or 'none for vertical.  When optional PARAGRAPH is
non-nil the justify operation is limited to the current paragraph,
otherwise the entire cell contents is justified." t nil)

(autoload (quote table-justify-row) "table" "\
Justify cells of a row.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or top,
'middle, 'bottom or 'none for vertical." t nil)

(autoload (quote table-justify-column) "table" "\
Justify cells of a column.
JUSTIFY is a symbol 'left, 'center or 'right for horizontal, or top,
'middle, 'bottom or 'none for vertical." t nil)

(autoload (quote table-fixed-width-mode) "table" "\
Toggle fixing width mode.
In the fixed width mode, typing inside a cell never changes the cell
width where in the normal mode the cell width expands automatically in
order to prevent a word being folded into multiple lines." t nil)

(autoload (quote table-query-dimension) "table" "\
Return the dimension of the current cell and the current table.
The result is a list (cw ch tw th c r cells) where cw is the cell
width, ch is the cell height, tw is the table width, th is the table
height, c is the number of columns, r is the number of rows and cells
is the total number of cells.  The cell dimension excludes the cell
frame while the table dimension includes the table frame.  The columns
and the rows are counted by the number of cell boundaries.  Therefore
the number tends to be larger than it appears for the tables with
non-uniform cell structure (heavily spanned and split).  When optional
WHERE is provided the cell and table at that location is reported." t nil)

(autoload (quote table-generate-source) "table" "\
Generate source of the current table in the specified language.
LANGUAGE is a symbol that specifies the language to describe the
structure of the table.  It must be either 'html, 'latex, 'tei or
'cals.  The resulted source text is inserted into DEST-BUFFER and the
buffer object is returned.  When DEST-BUFFER is omitted or nil the
default buffer specified in `table-dest-buffer-name' is used.  In this
case the content of the default buffer is erased prior to the
generation.  When DEST-BUFFER is non-nil it is expected to be either a
destination buffer or a name of the destination buffer.  In this case
the generated result is inserted at the current point in the
destination buffer and the previously existing contents in the buffer
are untouched.

References used for this implementation:

HTML:
        http://www.w3.org

LaTeX:
        http://www.maths.tcd.ie/~dwilkins/LaTeXPrimer/Tables.html

TEI (Text Encoding Initiative XML/SGML DTD):
        http://www.hcu.ox.ac.uk/TEI/Guidelines/ (general) 
        http://www.hcu.ox.ac.uk/TEI/Guidelines/FT.htm#FTTAB (tables)

CALS (DocBook DTD):
        http://www.oasis-open.org/html/a502.htm
        http://www.oreilly.com/catalog/docbook/chapter/book/table.html#AEN114751
" t nil)

(autoload (quote table-insert-sequence) "table" "\
Travel cells forward while inserting a specified sequence string in each cell.
STR is the base string from which the sequence starts.  When STR is an
empty string then each cell content is erased.  When STR ends with
numerical characters (they may optionally be surrounded by a pair of
parentheses) they are incremented as a decimal number.  Otherwise the
last character in STR is incremented in ASCII code order.  N is the
number of sequence elements to insert.  When N is negative the cell
traveling direction is backward.  When N is zero it travels forward
entire table.  INCREMENT is the increment between adjacent sequence
elements and can be a negative number for effectively decrementing.
INTERVAL is the number of cells to travel between sequence element
insertion which is normally 1.  When zero or less is given for
INTERVAL it is interpreted as number of cells per row so that sequence
is placed straight down vertically as long as the table's cell
structure is uniform.  JUSTIFY is one of the symbol 'left, 'center or
'right, that specifies justification of the inserted string.

Example:

  (progn
    (table-insert 16 3 5 1)
    (table-forward-cell 15)
    (table-insert-sequence \"D0\" -16 1 1 'center)
    (table-forward-cell 16)
    (table-insert-sequence \"A[0]\" -16 1 1 'center)
    (table-forward-cell 1)
    (table-insert-sequence \"-\" 16 0 1 'center))

  (progn
    (table-insert 16 8 5 1)
    (table-insert-sequence \"@\" 0 1 2 'right)
    (table-forward-cell 1)
    (table-insert-sequence \"64\" 0 1 2 'left))
" t nil)

(autoload (quote table-delete-row) "table" "\
Delete N row(s) of cells.
Delete N rows of cells from current row.  The current row is the row
contains the current cell where point is located.  Each row must
consists from cells of same height." t nil)

(autoload (quote table-delete-column) "table" "\
Delete N column(s) of cells.
Delete N columns of cells from current column.  The current column is
the column contains the current cell where point is located.  Each
column must consists from cells of same width." t nil)

(autoload (quote table-capture) "table" "\
Convert plain text into a table by capturing the text in the region.
Create a table with the text in region as cell contents.  BEG and END
specify the region.  The text in the region is replaced with a table.
The removed text is inserted in the table.  When optional
COL-DELIM-REGEXP and ROW-DELIM-REGEXP are provided the region contents
is parsed and separated into individual cell contents by using the
delimiter regular expressions.  This parsing determines the number of
columns and rows of the table automatically.  If COL-DELIM-REGEXP and
ROW-DELIM-REGEXP are omitted the result table has only one cell and
the entire region contents is placed in that cell.  Optional JUSTIFY
is one of 'left, 'center or 'right, which specifies the cell
justification.  Optional MIN-CELL-WIDTH specifies the minimum cell
width.  Optional COLUMNS specify the number of columns when
ROW-DELIM-REGEXP is not specified.


Example 1:

1, 2, 3, 4
5, 6, 7, 8
, 9, 10

Running `table-capture' on above 3 line region with COL-DELIM-REGEXP
\",\" and ROW-DELIM-REGEXP \"\\n\" creates the following table.  In
this example the cells are centered and minimum cell width is
specified as 5.

+-----+-----+-----+-----+
|  1  |  2  |  3  |  4  |
+-----+-----+-----+-----+
|  5  |  6  |  7  |  8  |
+-----+-----+-----+-----+
|     |  9  | 10  |     |
+-----+-----+-----+-----+

Note:

In case the function is called interactively user must use \\[quoted-insert] `quoted-insert'
in order to enter \"\\n\" successfully.  COL-DELIM-REGEXP at the end
of each row is optional.


Example 2:

This example shows how a table can be used for text layout editing.
Let `table-capture' capture the following region starting from
-!- and ending at -*-, that contains three paragraphs and two item
name headers.  This time specify empty string for both
COL-DELIM-REGEXP and ROW-DELIM-REGEXP.

-!-`table-capture' is a powerful command however mastering its power
requires some practice.  Here is a list of items what it can do.

Parse Cell Items      By using column delimiter regular
		      expression and raw delimiter regular
		      expression, it parses the specified text
		      area and extracts cell items from
		      non-table text and then forms a table out
		      of them.

Capture Text Area     When no delimiters are specified it
		      creates a single cell table.  The text in
		      the specified region is placed in that
		      cell.-*-

Now the entire content is captured in a cell which is itself a table
like this.

+-----------------------------------------------------------------+
|`table-capture' is a powerful command however mastering its power|
|requires some practice.  Here is a list of items what it can do. |
|                                                                 |
|Parse Cell Items      By using column delimiter regular          |
|                      expression and raw delimiter regular       |
|                      expression, it parses the specified text   |
|                      area and extracts cell items from          |
|                      non-table text and then forms a table out  |
|                      of them.                                   |
|                                                                 |
|Capture Text Area     When no delimiters are specified it        |
|                      creates a single cell table.  The text in  |
|                      the specified region is placed in that     |
|                      cell.                                      |
+-----------------------------------------------------------------+

By splitting the cell appropriately we now have a table consisting of
paragraphs occupying its own cell.  Each cell can now be edited
independently.

+-----------------------------------------------------------------+
|`table-capture' is a powerful command however mastering its power|
|requires some practice.  Here is a list of items what it can do. |
+---------------------+-------------------------------------------+
|Parse Cell Items     |By using column delimiter regular          |
|                     |expression and raw delimiter regular       |
|                     |expression, it parses the specified text   |
|                     |area and extracts cell items from          |
|                     |non-table text and then forms a table out  |
|                     |of them.                                   |
+---------------------+-------------------------------------------+
|Capture Text Area    |When no delimiters are specified it        |
|                     |creates a single cell table.  The text in  |
|                     |the specified region is placed in that     |
|                     |cell.                                      |
+---------------------+-------------------------------------------+

By applying `table-release', which does the opposite process, the
contents become once again plain text.  `table-release' works as
companion command to `table-capture' this way.
" t nil)

(autoload (quote table-release) "table" "\
Convert a table into plain text by removing the frame from a table.
Remove the frame from a table and inactivate the table.  This command
converts a table into plain text without frames.  It is a companion to
`table-capture' which does the opposite process." t nil)

(autoload (quote table-version) "table" "\
Show version number of table package." t nil)

;;;***

;;;### (autoloads (tail-command tail-file) "tail" "tail.el" (16269
;;;;;;  21296))
;;; Generated autoloads from tail.el

(autoload (quote tail-file) "tail" "\
Tails FILE specified with argument FILE inside a new buffer.
FILE *cannot* be a remote file specified with ange-ftp syntax because it is
passed to the Unix tail command." t nil)

(autoload (quote tail-command) "tail" "\
Tails COMMAND with arguments ARGS inside a new buffer.
It is also called by `tail-file'" t nil)

;;;***

;;;### (autoloads (trivial-cite) "tc" "tc.el" (16295 49414))
;;; Generated autoloads from tc.el

(autoload (quote trivial-cite) "tc" "\
A simple citation function for use in news/mailreaders.
It parses the headers via the functions defined in `tc-header-funs', then
makes a attribution for the citation using `tc-make-attribution' and indents
the inserted text with `tc-indent-citation'.
Numeric prefix arguments is how many lines of body to cite (useful for citing
mails with long attachments).
Usage:  (auto-load 'trivial-cite \"tc\" t t)
        (add-hook 'mail-citation-hook 'trivial-cite)
Bugs:  Not very intelligent about old citation marks other than '>'.
Customization:  See variables tc-fill-column, tc-remove-signature,
tc-citation-string, tc-make-attribution and tc-header-funs." nil nil)

;;;***

;;;### (autoloads (thinks-maybe-region thinks-yank thinks-region
;;;;;;  thinks) "thinks" "thinks.el" (16259 18884))
;;; Generated autoloads from thinks.el

(autoload (quote thinks) "thinks" "\
Insert TEXT wrapped in a think bubble.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you." t nil)

(autoload (quote thinks-region) "thinks" "\
Bubble wrap region bounding START and END.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you." t nil)

(autoload (quote thinks-yank) "thinks" "\
Do a `yank' and bubble wrap the yanked text.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you." t nil)

(autoload (quote thinks-maybe-region) "thinks" "\
If region is active, bubble wrap region bounding START and END.
If not, query for text to insert in bubble." t nil)

;;;***

;;;### (autoloads (tlc-mode) "tlc" "tlc.el" (17221 29124))
;;; Generated autoloads from tlc.el

(autoload (quote tlc-mode) "tlc" "\
Major mode for editing Tlc files, or files found in tlc directories." t nil)
(add-to-list 'auto-mode-alist '("\\.tlc$" .tlc-mode))

;;;***

;;;### (autoloads (tld) "tld" "tld.el" (17199 13164))
;;; Generated autoloads from tld.el

(autoload (quote tld) "tld" "\
Search the TLD list." t nil)

;;;***

;;;### (autoloads (twiddle-compile twiddle-start) "twiddle" "twiddle.el"
;;;;;;  (16259 18885))
;;; Generated autoloads from twiddle.el

(autoload (quote twiddle-start) "twiddle" "\
Start a mode line display hack.
If called interactively with a prefix argument, prompt for the name of
a hack to run.  If called from lisp, optional argument HACK is the name of
a hack to run.
Named hacks are defined in the table `twiddle-hacks'." t nil)

(autoload (quote twiddle-compile) "twiddle" "\
Like \\[compile], but run a twiddle hack during compilation.
If called with a prefix argument, prompt for a specific hack to run." t nil)

;;;***

;;;### (autoloads (underhat-region) "under" "under.el" (16269 21249))
;;; Generated autoloads from under.el

(autoload (quote underhat-region) "under" "\
Underline the region." t nil)

;;;***

;;;### (autoloads (wdired-change-to-wdired-mode) "wdired" "wdired.el"
;;;;;;  (16259 18885))
;;; Generated autoloads from wdired.el

(autoload (quote wdired-change-to-wdired-mode) "wdired" "\
Put a dired buffer in a mode in which filenames are editable.
In this mode the names of the files can be changed, and after 
typing C-c C-c the files and directories in disk are renamed.

See `wdired-mode'." t nil)

;;;***

;;;### (autoloads (xrdb-mode) "xrdb-mode" "xrdb-mode.el" (16259 18885))
;;; Generated autoloads from xrdb-mode.el

(autoload (quote xrdb-mode) "xrdb-mode" "\
Major mode for editing xrdb config files" t nil)

;;;***

;;;### (autoloads (all) "all" "all.el" (16107 56175))
;;; Generated autoloads from all.el

(autoload (quote all) "all" "\
Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*All*'.
Any changes made in that buffer will be propagated to this buffer." t nil)

;;;***

(provide 'emacs-goodies-loaddefs)

;;; emacs-goodies-loaddefs.el ends here
