;;; mel-b-dl.el: Base64 encoder/decoder using DL module

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
(require 'mime-def)

(eval-and-compile
  (defvar base64-dl-module
    (expand-file-name "base64.so" exec-directory))

  (defvar base64-dl-handle
    (and (file-exists-p base64-dl-module)
	 (dynamic-link base64-dl-module)))

  (dynamic-call "emacs_base64_init" base64-dl-handle)
  )

(defun base64-encode-region (start end)
  "Encode current region by base64.
START and END are buffer positions."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (insert (encode-base64-string str))
    )
  (or (bolp)
      (insert "\n"))
  )

(defun base64-decode-region (start end)
  "Decode current region by base64.
START and END are buffer positions."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (condition-case err
	(insert (decode-base64-string str))
      (error (message (nth 1 err)))
      )))

(defalias 'base64-encode-string 'encode-base64-string)
(defalias 'base64-decode-string 'decode-base64-string)


(mel-define-method-function (mime-encode-string string (nil "base64"))
			    'encode-base64-string)
(mel-define-method-function (mime-decode-string string (nil "base64"))
			    'decode-base64-string)
(mel-define-method-function (mime-encode-region start end (nil "base64"))
			    'base64-encode-region)
(mel-define-method-function (mime-decode-region start end (nil "base64"))
			    'base64-decode-region)

(mel-define-method-function (encoded-text-encode-string string (nil "B"))
			    'encode-base64-string)

(mel-define-method encoded-text-decode-string (string (nil "B"))
  (if (and (string-match B-encoded-text-regexp string)
	   (string= string (match-string 0 string)))
      (decode-base64-string string)
    (error "Invalid encoded-text %s" string)))


;;; @ base64 encoder/decoder for file
;;;

(mel-define-method mime-insert-encoded-file (filename (nil "base64"))
  "Encode contents of file FILENAME to base64, and insert the result.
It calls external base64 encoder specified by
`base64-external-encoder'.  So you must install the program (maybe
mmencode included in metamail or XEmacs package)."
  (interactive (list (read-file-name "Insert encoded file: ")))
  (insert (encode-base64-string
	   (with-temp-buffer
	     (insert-file-contents-as-binary filename)
	     (buffer-string))))
  (or (bolp)
      (insert "\n"))
  )

(mel-define-method mime-write-decoded-region (start end filename
						    (nil "base64"))
  "Decode and write current region encoded by base64 into FILENAME.
START and END are buffer positions."
  (interactive
   (list (region-beginning) (region-end)
	 (read-file-name "Write decoded region to file: ")))
  (let ((str (buffer-substring start end)))
    (with-temp-buffer
      (insert (decode-base64-string str))
      (write-region-as-binary (point-min) (point-max) filename)
      )))


;;; @ end
;;;

(provide 'mel-b-dl)

;;; mel-b-dl.el ends here.
