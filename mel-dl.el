;;; mel-dl.el: Base64 encoder/decoder using DL module

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: MIME, Base64

;; This file is part of MEL (MIME Encoding Library).

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

(require 'emu)

(defvar base64-dl-module
  (expand-file-name "base64.so" exec-directory))

(defvar base64-dl-handle
  (and (file-exists-p base64-dl-module)
       (dynamic-link base64-dl-module)))

(dynamic-call "emacs_base64_init" base64-dl-handle)

(defalias 'base64-dl-encode-string 'encode-base64-string)
(defalias 'base64-dl-decode-string 'decode-base64-string)

(defun base64-dl-encode-region (start end)
  "Encode current region by base64.
START and END are buffer positions."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((str (buffer-substring start end)))
	(delete-region start end)
	(insert (encode-base64-string str))
	)
      (or (bolp)
	  (insert "\n")
	  )
      )))

(defun base64-dl-decode-region (start end)
  "Decode current region by base64.
START and END are buffer positions."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (looking-at ".*\n")
	(condition-case err
	    (replace-match
	     (decode-base64-string
	      (buffer-substring (match-beginning 0) (1- (match-end 0))))
	     t t)
	  (error
	   (prog1
	       (message (nth 1 err))
	     (replace-match "")))))
      (if (looking-at ".*$")
	  (condition-case err
	      (replace-match
	       (decode-base64-string
		(buffer-substring (match-beginning 0) (match-end 0)))
	       t t)
	    (error
	     (prog1
		 (message (nth 1 err))
	       (replace-match "")))
	    ))
      )))


;;; @ end
;;;

(provide 'mel-dl)

;;; mel-dl.el ends here.
