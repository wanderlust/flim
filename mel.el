;;; mel.el : a MIME encoding/decoding library

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; modified by Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Created: 1995/6/25
;; Keywords: MIME, Base64, Quoted-Printable, uuencode, gzip64

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

(defcustom mime-content-transfer-encoding-list
  '("7bit" "8bit" "binary" "base64" "quoted-printable")
  "List of Content-Transfer-Encoding.  Each encoding must be string."
  :group 'mime
  :type '(repeat string))

(defvar mel-encoding-module-alist nil)

(defsubst mel-use-module (name encodings)
  (let (encoding)
    (while (setq encoding (car encodings))
      (set-alist 'mel-encoding-module-alist
		 encoding
		 (cons name (cdr (assoc encoding mel-encoding-module-alist))))
      (setq encodings (cdr encodings))
      )))

(defsubst mel-find-function (service encoding)
  (let* ((oba (symbol-value (intern (format "%s-obarray" service))))
	 (f (intern-soft encoding oba)))
    (or f
	(let ((rest (cdr (assoc encoding mel-encoding-module-alist))))
	  (while (and rest
		      (progn
			(require (car rest))
			(null (setq f (intern-soft encoding oba)))
			))
	    (setq rest (cdr rest))
	    )
	  f))))


;;; @ setting for modules
;;;

(defvar base64-dl-module
  (and (fboundp 'dynamic-link)
       (let ((path (expand-file-name "base64.so" exec-directory)))
	 (and (file-exists-p path)
	      path))))


(mel-use-module 'mel-b '("base64" "B"))
(mel-use-module 'mel-q '("quoted-printable" "Q"))
(mel-use-module 'mel-g '("x-gzip64"))
(mel-use-module 'mel-u '("x-uue" "x-uuencode"))

(if (featurep 'mule)
    (mel-use-module 'mel-ccl '("base64" "quoted-printable" "B" "Q"))
  )

(if base64-dl-module
    (mel-use-module 'mel-b-dl '("base64" "B"))
  )

(mel-define-method-function (mime-encode-string string (nil "7bit"))
			    'identity)
(mel-define-method-function (mime-decode-string string (nil "7bit"))
			    'identity)
(mel-define-method mime-encode-region (start end (nil "7bit")))
(mel-define-method mime-decode-region (start end (nil "7bit")))
(mel-define-method-function (mime-insert-encoded-file filename (nil "7bit"))
			    'insert-file-contents-as-binary)
(mel-define-method-function (mime-write-decoded-region
			     start end filename (nil "7bit"))
			    'write-region-as-binary)

(mel-define-method-function (mime-encode-string string (nil "8bit"))
			    'identity)
(mel-define-method-function (mime-decode-string string (nil "8bit"))
			    'identity)
(mel-define-method mime-encode-region (start end (nil "8bit")))
(mel-define-method mime-decode-region (start end (nil "8bit")))
(mel-define-method-function (mime-insert-encoded-file filename (nil "8bit"))
			    'insert-file-contents-as-binary)
(mel-define-method-function (mime-write-decoded-region
			     start end filename (nil "8bit"))
			    'write-region-as-binary)

(mel-define-method-function (mime-encode-string string (nil "binary"))
			    'identity)
(mel-define-method-function (mime-decode-string string (nil "binary"))
			    'identity)
(mel-define-method mime-encode-region (start end (nil "binary")))
(mel-define-method mime-decode-region (start end (nil "binary")))
(mel-define-method-function (mime-insert-encoded-file filename (nil "binary"))
			    'insert-file-contents-as-binary)
(mel-define-method-function (mime-write-decoded-region
			     start end filename (nil "binary"))
			    'write-region-as-binary)


;;; @ region
;;;

;;;###autoload
(defun mime-encode-region (start end encoding)
  "Encode region START to END of current buffer using ENCODING.
ENCODING must be string."
  (interactive
   (list (region-beginning) (region-end)
	 (completing-read "encoding: "
			  (mapcar #'list mime-content-transfer-encoding-list)
			  nil t "base64")))
  (funcall (mel-find-function 'mime-encode-region encoding) start end)
  )


;;;###autoload
(defun mime-decode-region (start end encoding)
  "Decode region START to END of current buffer using ENCODING.
ENCODING must be string."
  (interactive
   (list (region-beginning) (region-end)
	 (completing-read "encoding: "
			  (mapcar #'list mime-content-transfer-encoding-list)
			  nil t "base64")))
  (funcall (mel-find-function 'mime-decode-region encoding)
	   start end))


;;; @ string
;;;

;;;###autoload
(defun mime-decode-string (string encoding)
  "Decode STRING using ENCODING.
ENCODING must be string.  If ENCODING is found in
`mime-string-decoding-method-alist' as its key, this function decodes
the STRING by its value."
  (funcall (mel-find-function 'mime-decode-string encoding)
	   string))


(mel-define-service encoded-text-encode-string (string encoding)
  "Encode STRING as encoded-text using ENCODING.
ENCODING must be string.")

(mel-define-service encoded-text-decode-string (string encoding)
  "Decode STRING as encoded-text using ENCODING.
ENCODING must be string.")

(defun base64-encoded-length (string)
  (let ((len (length string)))
    (* (+ (/ len 3)
	  (if (= (mod len 3) 0) 0 1)
	  ) 4)
    ))

(defsubst Q-encoding-printable-char-p (chr mode)
  (and (not (memq chr '(?= ?? ?_)))
       (<= ?\   chr)(<= chr ?~)
       (cond ((eq mode 'text) t)
	     ((eq mode 'comment)
	      (not (memq chr '(?\( ?\) ?\\)))
	      )
	     (t
	      (string-match "[A-Za-z0-9!*+/=_---]" (char-to-string chr))
	      ))))

(defun Q-encoded-text-length (string &optional mode)
  (let ((l 0)(i 0)(len (length string)) chr)
    (while (< i len)
      (setq chr (elt string i))
      (if (Q-encoding-printable-char-p chr mode)
	  (setq l (+ l 1))
	(setq l (+ l 3))
	)
      (setq i (+ i 1)) )
    l))


;;; @ file
;;;

;;;###autoload
(defun mime-insert-encoded-file (filename encoding)
  "Insert file FILENAME encoded by ENCODING format."
  (interactive
   (list (read-file-name "Insert encoded file: ")
	 (completing-read "encoding: "
			  (mapcar #'list mime-content-transfer-encoding-list)
			  nil t "base64")))
  (funcall (mel-find-function 'mime-insert-encoded-file encoding)
	   filename))


;;;###autoload
(defun mime-write-decoded-region (start end filename encoding)
  "Decode and write current region encoded by ENCODING into FILENAME.
START and END are buffer positions."
  (interactive
   (list (region-beginning) (region-end)
	 (read-file-name "Write decoded region to file: ")
	 (completing-read "encoding: "
			  (mapcar #'list mime-content-transfer-encoding-list)
			  nil t "base64")))
  (funcall (mel-find-function 'mime-write-decoded-region encoding)
	   start end filename))


;;; @ end
;;;

(provide 'mel)

;;; mel.el ends here.
