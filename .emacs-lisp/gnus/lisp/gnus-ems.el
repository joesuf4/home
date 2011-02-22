;;; gnus-ems.el --- functions for making Gnus work under different Emacsen

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
;;   2004, 2005, 2006 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'ring))

;;; Function aliases later to be redefined for XEmacs usage.

(defvar gnus-mouse-2 [mouse-2])
(defvar gnus-down-mouse-3 [down-mouse-3])
(defvar gnus-down-mouse-2 [down-mouse-2])
(defvar gnus-widget-button-keymap nil)
(defvar gnus-mode-line-modified
  (if (featurep 'xemacs)
      '("--**-" . "-----")
    '("**" "--")))

(eval-and-compile
  (autoload 'gnus-xmas-define "gnus-xmas")
  (autoload 'gnus-xmas-redefine "gnus-xmas")
  (autoload 'gnus-get-buffer-create "gnus")
  (autoload 'nnheader-find-etc-directory "nnheader"))

(autoload 'smiley-region "smiley")

(defun gnus-kill-all-overlays ()
  "Delete all overlays in the current buffer."
  (let* ((overlayss (overlay-lists))
	 (buffer-read-only nil)
	 (overlays (delq nil (nconc (car overlayss) (cdr overlayss)))))
    (while overlays
      (delete-overlay (pop overlays)))))

;;; Mule functions.

(defun gnus-mule-max-width-function (el max-width)
  `(let* ((val (eval (, el)))
	  (valstr (if (numberp val)
		      (int-to-string val) val)))
     (if (> (length valstr) ,max-width)
	 (truncate-string-to-width valstr ,max-width)
       valstr)))

(eval-and-compile
  (if (featurep 'xemacs)
      (gnus-xmas-define)
    (defvar gnus-mouse-face-prop 'mouse-face
      "Property used for highlighting mouse regions.")))

(eval-when-compile
  (defvar gnus-tmp-unread)
  (defvar gnus-tmp-replied)
  (defvar gnus-tmp-score-char)
  (defvar gnus-tmp-indentation)
  (defvar gnus-tmp-opening-bracket)
  (defvar gnus-tmp-lines)
  (defvar gnus-tmp-name)
  (defvar gnus-tmp-closing-bracket)
  (defvar gnus-tmp-subject-or-nil)
  (defvar gnus-check-before-posting)
  (defvar gnus-mouse-face)
  (defvar gnus-group-buffer))

(defun gnus-ems-redefine ()
  (cond
   ((featurep 'xemacs)
    (gnus-xmas-redefine))

   ((featurep 'mule)
    ;; Mule and new Emacs definitions

    ;; [Note] Now there are three kinds of mule implementations,
    ;; original MULE, XEmacs/mule and Emacs 20+ including
    ;; MULE features.  Unfortunately these APIs are different.  In
    ;; particular, Emacs (including original Mule) and XEmacs are
    ;; quite different.  However, this version of Gnus doesn't support
    ;; anything other than XEmacs 20+ and Emacs 20.3+.

    ;; Predicates to check are following:
    ;; (boundp 'MULE) is t only if Mule (original; anything older than
    ;;                     Mule 2.3) is running.
    ;; (featurep 'mule) is t when other mule variants are running.

    ;; It is possible to detect XEmacs/mule by (featurep 'mule) and
    ;; (featurep 'xemacs).  In this case, the implementation for
    ;; XEmacs/mule may be shareable between XEmacs and XEmacs/mule.

    (defvar gnus-summary-display-table nil
      "Display table used in summary mode buffers.")
    (defalias 'gnus-max-width-function 'gnus-mule-max-width-function)

    (when (boundp 'gnus-check-before-posting)
      (setq gnus-check-before-posting
	    (delq 'long-lines
		  (delq 'control-chars gnus-check-before-posting))))

    (defun gnus-summary-line-format-spec ()
      (insert gnus-tmp-unread gnus-tmp-replied
	      gnus-tmp-score-char gnus-tmp-indentation)
      (put-text-property
       (point)
       (progn
	 (insert
	  gnus-tmp-opening-bracket
	  (format "%4d: %-20s"
		  gnus-tmp-lines
		  (if (> (length gnus-tmp-name) 20)
		      (truncate-string-to-width gnus-tmp-name 20)
		    gnus-tmp-name))
	  gnus-tmp-closing-bracket)
	 (point))
       gnus-mouse-face-prop gnus-mouse-face)
      (insert " " gnus-tmp-subject-or-nil "\n")))))

;; Clone of `appt-select-lowest-window' in appt.el.
(defun gnus-select-lowest-window ()
"Select the lowest window on the frame."
  (let ((lowest-window (selected-window))
	(bottom-edge (nth 3 (window-edges))))
    (walk-windows (lambda (w)
		    (let ((next-bottom-edge (nth 3 (window-edges w))))
		      (when (< bottom-edge next-bottom-edge)
			(setq bottom-edge next-bottom-edge
			      lowest-window w)))))
    (select-window lowest-window)))

(defun gnus-region-active-p ()
  "Say whether the region is active."
  (and (boundp 'transient-mark-mode)
       transient-mark-mode
       (boundp 'mark-active)
       mark-active))

(defun gnus-mark-active-p ()
  "Non-nil means the mark and region are currently active in this buffer."
  mark-active) ; aliased to region-exists-p in XEmacs.

(defun gnus-x-splash ()
  "Show a splash screen using a pixmap in the current buffer."
  (let ((dir (nnheader-find-etc-directory "gnus"))
	pixmap file height beg i)
    (save-excursion
      (switch-to-buffer (gnus-get-buffer-create gnus-group-buffer))
      (let ((buffer-read-only nil)
	    width height)
	(erase-buffer)
	(when (and dir
		   (file-exists-p (setq file
					(expand-file-name "x-splash" dir))))
	  (let ((coding-system-for-read 'raw-text)
		default-enable-multibyte-characters)
	    (with-temp-buffer
	      (insert-file-contents file)
	      (goto-char (point-min))
	      (ignore-errors
		(setq pixmap (read (current-buffer)))))))
	(when pixmap
	  (make-face 'gnus-splash)
	  (setq height (/ (car pixmap) (frame-char-height))
		width (/ (cadr pixmap) (frame-char-width)))
	  (set-face-foreground 'gnus-splash "Brown")
	  (set-face-stipple 'gnus-splash pixmap)
	  (insert-char ?\n (* (/ (window-height) 2 height) height))
	  (setq i height)
	  (while (> i 0)
	    (insert-char ?\  (* (/ (window-width) 2 width) width))
	    (setq beg (point))
	    (insert-char ?\  width)
	    (set-text-properties beg (point) '(face gnus-splash))
	    (insert ?\n)
	    (decf i))
	  (goto-char (point-min))
	  (sit-for 0))))))

;;; Image functions.

(defun gnus-image-type-available-p (type)
  (and (fboundp 'image-type-available-p)
       (image-type-available-p type)))

(defun gnus-create-image (file &optional type data-p &rest props)
  (let ((face (plist-get props :face)))
    (when face
      (setq props (plist-put props :foreground (face-foreground face)))
      (setq props (plist-put props :background (face-background face))))
    (apply 'create-image file type data-p props)))

(defun gnus-put-image (glyph &optional string category)
  (let ((point (point)))
    (insert-image glyph (or string " "))
    (put-text-property point (point) 'gnus-image-category category)
    (unless string
      (put-text-property (1- (point)) (point)
			 'gnus-image-text-deletable t))
    glyph))

(defun gnus-remove-image (image &optional category)
  "Remove the image matching IMAGE and CATEGORY found first."
  (let ((start (point-min))
	val end)
    (while (and (not end)
		(or (setq val (get-text-property start 'display))
		    (and (setq start
			       (next-single-property-change start 'display))
			 (setq val (get-text-property start 'display)))))
      (setq end (or (next-single-property-change start 'display)
		    (point-max)))
      (if (and (equal val image)
	       (equal (get-text-property start 'gnus-image-category)
		      category))
	  (progn
	    (put-text-property start end 'display nil)
	    (when (get-text-property start 'gnus-image-text-deletable)
	      (delete-region start end)))
	(unless (= end (point-max))
	  (setq start end
		end nil))))))

(provide 'gnus-ems)

;;; arch-tag: e7360b45-14b5-4171-aa39-69a44aed3cdb
;;; gnus-ems.el ends here
