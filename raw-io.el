;;; raw-io.el --- input/output without code-conversion

;; Copyright (C) 1997,1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: definition, MIME, multimedia, mail, news

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(eval-when-compile (require 'static))

(static-if (and (featurep 'xemacs)
		(not (featurep 'utf-2000)))
    (defun binary-insert-file-contents (filename
					&optional visit beg end replace)
      "Like `insert-file-contents', but only reads in the file literally.
A buffer may be modified in several ways after reading into the buffer,
to Emacs features such as format decoding, character code
conversion, find-file-hooks, automatic uncompression, etc.

This function ensures that none of these modifications will take place."
      (let ((format-alist nil)
	    (after-insert-file-functions nil)
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (jka-compr-compression-info-list nil)
	    (jam-zcat-filename-list nil)
	    (find-buffer-file-type-function
	     (if (fboundp 'find-buffer-file-type)
		 (symbol-function 'find-buffer-file-type)
	       nil)))
	(unwind-protect
	    (progn
	      (fset 'find-buffer-file-type (lambda (filename) t))
	      (insert-file-contents filename visit beg end replace))
	  (if find-buffer-file-type-function
	      (fset 'find-buffer-file-type find-buffer-file-type-function)
	    (fmakunbound 'find-buffer-file-type)))))
  (defalias 'binary-insert-file-contents 'insert-file-contents-literally))

(defun binary-write-region (start end filename
				  &optional append visit lockname)
  "Like `write-region', q.v., but don't encode."
  (let ((coding-system-for-write 'binary)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename append visit lockname)))

(defun binary-find-file-noselect (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (let ((coding-system-for-read 'binary)
	format-alist)
    (find-file-noselect filename nowarn rawfile)))

(defun binary-open-network-stream (name buffer host service &rest options)
  "Like `open-network-stream', q.v., but don't code and format conversion."
  (let ((coding-system-for-read  'binary)
	(coding-system-for-write 'binary))
    (apply #'open-network-stream name buffer host service options)))

(defun binary-start-process (name buffer program &rest program-args)
  "Like `start-process', q.v., but don't code conversion."
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (apply '#start-process name buffer program program-args)))

(defun binary-start-process-shell-command (name buffer &rest args)
  "Like `start-process-shell-command', q.v., but don't code conversion."
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (apply '#start-process-shell-command name buffer args)))


(defun raw-text-insert-file-contents (filename
				      &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.
Like `binary-insert-file-contents', but it converts line-break
code."
  (let ((coding-system-for-read 'raw-text)
	format-alist)
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename visit beg end replace)))


(defun raw-message-write-region (start end filename
				       &optional append visit lockname)
  "Like `write-region', q.v., but write as network representation."
  (let ((coding-system-for-write 'raw-text-dos)
	format-alist)
    (write-region start end filename append visit lockname)))


;;; @ end
;;;

(provide 'raw-io)

;;; raw-io.el ends here
