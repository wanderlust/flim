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


;;; @ variable
;;;

(defvar base64-internal-encoding-limit 1000
  "*limit size to use internal base64 encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defvar base64-internal-decoding-limit 1000
  "*limit size to use internal base64 decoder.
 size of input to decode is larger than this limit,
external decoder is called.")

(defvar quoted-printable-internal-encoding-limit
  (if (and (featurep 'xemacs)(featurep 'mule))
      0
    (require 'path-util)
    (if (exec-installed-p "mmencode")
        1000
      (message "Don't found external encoder for Quoted-Printable!")
      nil))
  "*limit size to use internal quoted-printable encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defvar quoted-printable-internal-decoding-limit nil
  "*limit size to use internal quoted-printable decoder.
If size of input to decode is larger than this limit,
external decoder is called.")


;;; @ autoload
;;;

;; mel-dl
(defvar base64-dl-module
  (and (fboundp 'dynamic-link)
       (let ((path (expand-file-name "base64.so" exec-directory)))
	 (and (file-exists-p path)
	      path))))

(when base64-dl-module
  (autoload 'base64-dl-encode-string "mel-dl"
   "Encode STRING to base64, and return the result.")
  (autoload 'base64-dl-decode-string "mel-dl"
   "Decode STRING which is encoded in base64, and return the result.")
  (autoload 'base64-dl-encode-region "mel-dl"
   "Encode current region by base64." t)
  (autoload 'base64-dl-decode-region "mel-dl"
   "Decode current region by base64." t))

;; mel-b
(autoload 'base64-internal-encode-string "mel-b"
 "Encode STRING to base64, and return the result.")
(autoload 'base64-internal-decode-string "mel-b"
 "Decode STRING which is encoded in base64, and return the result.")
(autoload 'base64-internal-encode-region "mel-b"
 "Encode current region by base64." t)
(autoload 'base64-internal-decode-region "mel-b"
 "Decode current region by base64." t)
(autoload 'base64-internal-insert-encoded-file "mel-b"
 "Encode contents of file to base64, and insert the result." t)
(autoload 'base64-internal-write-decoded-region "mel-b"
 "Decode and write current region encoded by base64 into FILENAME." t)

(autoload 'base64-external-encode-string "mel-b"
 "Encode STRING to base64, and return the result.")
(autoload 'base64-external-decode-string "mel-b"
 "Decode STRING which is encoded in base64, and return the result.")
(autoload 'base64-external-encode-region "mel-b"
 "Encode current region by base64." t)
(autoload 'base64-external-decode-region "mel-b"
 "Decode current region by base64." t)
(autoload 'base64-external-insert-encoded-file "mel-b"
 "Encode contents of file to base64, and insert the result." t)
(autoload 'base64-external-write-decoded-region "mel-b"
 "Decode and write current region encoded by base64 into FILENAME." t)

;; for encoded-word
(autoload 'base64-internal-encoded-length "mel-b")

;; mel-q
(autoload 'quoted-printable-internal-encode-string "mel-q"
  "Encode STRING to quoted-printable, and return the result.")
(autoload 'quoted-printable-internal-decode-string "mel-q"
  "Decode STRING which is encoded in quoted-printable, and return the result.")
(autoload 'quoted-printable-internal-encode-region "mel-q"
  "Encode current region by Quoted-Printable." t)
(autoload 'quoted-printable-internal-decode-region "mel-q"
  "Decode current region by Quoted-Printable." t)

(autoload 'quoted-printable-external-encode-string "mel-q"
  "Encode STRING to quoted-printable, and return the result.")
(autoload 'quoted-printable-external-decode-string "mel-q"
  "Decode STRING which is encoded in quoted-printable, and return the result.")
(autoload 'quoted-printable-external-encode-region "mel-q"
  "Encode current region by Quoted-Printable." t)
(autoload 'quoted-printable-external-decode-region "mel-q"
  "Decode current region by Quoted-Printable." t)

(autoload 'quoted-printable-external-insert-encoded-file "mel-q"
  "Encode contents of file to quoted-printable, and insert the result." t)
(autoload 'quoted-printable-external-write-decoded-region "mel-q"
  "Decode and write current region encoded by quoted-printable into FILENAME."
  t)

;; for encoded-word
(autoload 'q-encoding-internal-encode-string "mel-q"
  "Encode STRING to Q-encoding of encoded-word, and return the result.")
(autoload 'q-encoding-internal-decode-string "mel-q"
  "Decode STRING which is encoded in Q-encoding and return the result.")
(autoload 'q-encoding-internal-encoded-length "mel-q")

;; mel-u
(autoload 'uuencode-external-encode-region "mel-u"
  "Encode current region by unofficial uuencode format." t)
(autoload 'uuencode-external-decode-region "mel-u"
  "Decode current region by unofficial uuencode format." t)
(autoload 'uuencode-external-insert-encoded-file "mel-u"
  "Insert file encoded by unofficial uuencode format." t)
(autoload 'uuencode-external-write-decoded-region "mel-u"
  "Decode and write current region encoded by uuencode into FILENAME." t)

;; mel-g
(autoload 'gzip64-external-encode-region "mel-g"
  "Encode current region by unofficial x-gzip64 format." t)
(autoload 'gzip64-external-decode-region "mel-g"
  "Decode current region by unofficial x-gzip64 format." t)
(autoload 'gzip64-external-insert-encoded-file "mel-g"
  "Insert file encoded by unofficial gzip64 format." t)
(autoload 'gzip64-external-write-decoded-region "mel-g"
  "Decode and write current region encoded by gzip64 into FILENAME." t)

;; mel-ccl
(when (fboundp 'make-ccl-coding-system)
  (unless (and (boundp 'ccl-encoder-eof-block-is-broken)
               ccl-encoder-eof-block-is-broken)
    (autoload 'base64-ccl-encode-string "mel-ccl"
      "Encode STRING with base64 encoding.")
    (autoload 'base64-ccl-encode-region "mel-ccl"
      "Encode region from START to END with base64 encoding." t)
    (autoload 'base64-ccl-insert-encoded-file "mel-ccl"
      "Encode contents of file FILENAME to base64, and insert the result." t))

  (autoload 'base64-ccl-decode-string "mel-ccl"
    "Decode base64 encoded STRING")
  (autoload 'base64-ccl-decode-region "mel-ccl"
    "Decode base64 encoded STRING" t)
  (autoload 'base64-ccl-write-decoded-region "mel-ccl"
    "Decode the region from START to END and write out to FILENAME." t)

  (unless (and (boundp 'ccl-encoder-eof-block-is-broken)
               ccl-encoder-eof-block-is-broken)
    (autoload 'quoted-printable-ccl-encode-string "mel-ccl"
      "Encode STRING with quoted-printable encoding.")
    (autoload 'quoted-printable-ccl-encode-region "mel-ccl"
      "Encode the region from START to END with quoted-printable
  encoding." t)
    (autoload 'quoted-printable-ccl-insert-encoded-file "mel-ccl"
      "Encode contents of the file named as FILENAME, and insert it." t))

  (autoload 'quoted-printable-ccl-decode-string "mel-ccl"
    "Decode quoted-printable encoded STRING.")
  (autoload 'quoted-printable-ccl-decode-region "mel-ccl"
    "Decode the region from START to END with quoted-printable
  encoding.")
  (autoload 'quoted-printable-ccl-write-decoded-region "mel-ccl"
    "Decode quoted-printable encoded current region and write out to FILENAME." t)

  (autoload 'q-encoding-ccl-encode-string "mel-ccl"
    "Encode STRING to Q-encoding of encoded-word, and return the result.
  MODE allows `text', `comment', `phrase' or nil.  Default value is
  `phrase'.")
  (autoload 'q-encoding-ccl-decode-string "mel-ccl"
    "Decode Q encoded STRING and return the result.")
)

;;; @ entrance functions.
;;;

(cond
  ((fboundp 'base64-dl-encode-string)
    (defalias 'base64-encode-string 'base64-dl-encode-string))
  ((fboundp 'base64-ccl-encode-string)
    (defalias 'base64-encode-string 'base64-ccl-encode-string))
  (t
    (defalias 'base64-encode-string 'base64-internal-encode-string)))

(defun base64-internal-external-decode-string (string)
  "Decode STRING which is encoded in base64, and return the result.
This function calls internal base64 decoder if size of STRING is
smaller than `base64-internal-decoding-limit', otherwise it calls
external base64 decoder specified by `base64-external-decoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "r")
  (if (and base64-internal-decoding-limit
           (> (length string) base64-internal-decoding-limit))
      (base64-external-decode-string string)
    (base64-internal-decode-string string)))

(cond
  ((fboundp 'base64-dl-decode-string)
    (defalias 'base64-decode-string 'base64-dl-decode-string))
  ((fboundp 'base64-ccl-decode-string)
    (defalias 'base64-decode-string 'base64-ccl-decode-string))
  (t
    (defalias 'base64-decode-string 'base64-internal-external-decode-string)))

(defun base64-internal-external-encode-region (start end)
  "Encode current region by base64.
START and END are buffer positions.
This function calls internal base64 encoder if size of region is
smaller than `base64-internal-encoding-limit', otherwise it calls
external base64 encoder specified by `base64-external-encoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "r")
  (if (and base64-internal-encoding-limit
           (> (- end start) base64-internal-encoding-limit))
      (base64-external-encode-region start end)
    (base64-internal-encode-region start end)))

(cond
  ((fboundp 'base64-dl-encode-region)
    (defalias 'base64-encode-region 'base64-dl-encode-region)) ; no fold
  ((fboundp 'base64-ccl-encode-region)
    (defalias 'base64-encode-region 'base64-ccl-encode-region)) ; no fold
  (t
    (defalias 'base64-encode-region 'base64-internal-external-encode-region))) ; LF fold

(defun base64-internal-external-decode-region (start end)
  "Decode current region by base64.
START and END are buffer positions.
This function calls internal base64 decoder if size of region is
smaller than `base64-internal-decoding-limit', otherwise it calls
external base64 decoder specified by `base64-external-decoder'.  In
this case, you must install the program (maybe mmencode included in
metamail or XEmacs package)."
  (interactive "r")
  (if (and base64-internal-decoding-limit
           (> (- end start) base64-internal-decoding-limit))
      (base64-external-decode-region start end)
    (base64-internal-decode-region start end)))

(cond
  ((fboundp 'base64-dl-decode-region)
    (defalias 'base64-decode-region 'base64-dl-decode-region))
  ((fboundp 'base64-ccl-decode-region)
    (defalias 'base64-decode-region 'base64-ccl-decode-region))
  (t
    (defalias 'base64-decode-region 'base64-internal-external-decode-region)))

(defun base64-internal-external-insert-encoded-file (filename)
  "Encode contents of file FILENAME to base64, and insert the result.
It calls external base64 encoder specified by
`base64-external-encoder'.  So you must install the program (maybe
mmencode included in metamail or XEmacs package)."
  (interactive (list (read-file-name "Insert encoded file: ")))
  (if (and base64-internal-encoding-limit
           (> (nth 7 (file-attributes filename))
              base64-internal-encoding-limit))
      (base64-external-insert-encoded-file filename)
    (base64-internal-insert-encoded-file filename)))

(cond
  ((fboundp 'base64-ccl-insert-encoded-file)
    (defalias 'base64-insert-encoded-file 'base64-ccl-insert-encoded-file))
  (t
    (defalias 'base64-insert-encoded-file 'base64-internal-external-insert-encoded-file)))

(defun base64-internal-external-write-decoded-region (start end filename)
  "Decode and write current region encoded by base64 into FILENAME.
START and END are buffer positions."
  (interactive
   (list (region-beginning) (region-end)
         (read-file-name "Write decoded region to file: ")))
  (if (and base64-internal-decoding-limit
           (> (- end start) base64-internal-decoding-limit))
      (base64-external-write-decoded-region start end filename)
    (base64-internal-write-decoded-region start end filename)))

(cond
  ((fboundp 'base64-ccl-write-decoded-region)
    (defalias 'base64-write-decoded-region 'base64-ccl-write-decoded-region))
  (t
    (defalias 'base64-write-decoded-region 'base64-internal-external-write-decoded-region)))

(cond
  (t
    (defalias 'base64-encoded-length 'base64-internal-encoded-length)))

(cond
  ((fboundp 'quoted-printable-ccl-encode-string)
    (defalias 'quoted-printable-encode-string 'quoted-printable-ccl-encode-string))
  (t
    (defun quoted-printable-encode-string (string)
      "Encode STRING to quoted-printable, and return the result."
      (if (and quoted-printable-internal-encoding-limit
               (> (length string) quoted-printable-internal-encoding-limit))
          (quoted-printable-external-encode-string string)
        (quoted-printable-internal-encode-string string)))))

(cond
  ((fboundp 'quoted-printable-ccl-decode-string)
    (defalias 'quoted-printable-decode-string 'quoted-printable-ccl-decode-string))
  (t
    (defun quoted-printable-decode-string (string)
      "Decode STRING which is encoded in quoted-printable, and return the result."
      (if (and quoted-printable-internal-decoding-limit
               (> (length string) quoted-printable-internal-decoding-limit))
          (quoted-printable-external-decode-string string)
        (quoted-printable-internal-decode-string string)))))

(cond
  ((fboundp 'quoted-printable-ccl-encode-region)
    (defalias 'quoted-printable-encode-region 'quoted-printable-ccl-encode-region))
  (t
    (defun quoted-printable-encode-region (start end)
      "Encode current region by quoted-printable.
START and END are buffer positions.
This function calls internal quoted-printable encoder if size of
region is smaller than `quoted-printable-internal-encoding-limit',
otherwise it calls external quoted-printable encoder specified by
`quoted-printable-external-encoder'.  In this case, you must install
the program (maybe mmencode included in metamail or XEmacs package)."
      (interactive "r")
      (if (and quoted-printable-internal-encoding-limit
               (> (- end start) quoted-printable-internal-encoding-limit))
          (quoted-printable-external-encode-region start end)
        (quoted-printable-internal-encode-region start end)
        ))))

(cond
  ((fboundp 'quoted-printable-ccl-decode-region)
    (defalias 'quoted-printable-decode-region 'quoted-printable-ccl-decode-region))
  (t
    (defun quoted-printable-decode-region (start end)
      "Decode current region by quoted-printable.
START and END are buffer positions.
This function calls internal quoted-printable decoder if size of
region is smaller than `quoted-printable-internal-decoding-limit',
otherwise it calls external quoted-printable decoder specified by
`quoted-printable-external-decoder'.  In this case, you must install
the program (maybe mmencode included in metamail or XEmacs package)."
      (interactive "r")
      (if (and quoted-printable-internal-decoding-limit
               (> (- end start) quoted-printable-internal-decoding-limit))
          (quoted-printable-external-decode-region start end)
        (quoted-printable-internal-decode-region start end)
        ))))

(cond
  ((fboundp 'quoted-printable-ccl-insert-encoded-file)
    (defalias 'quoted-printable-insert-encoded-file 'quoted-printable-ccl-insert-encoded-file))
  (t
    (defalias 'quoted-printable-insert-encoded-file 'quoted-printable-external-insert-encoded-file)))

(cond
  ((fboundp 'quoted-printable-ccl-write-decoded-region)
    (defalias 'quoted-printable-write-decoded-region 'quoted-printable-ccl-write-decoded-region))
  (t
    (defalias 'quoted-printable-write-decoded-region 'quoted-printable-external-write-decoded-region)))

(cond
  ((fboundp 'q-encoding-ccl-encode-string)
    (defalias 'q-encoding-encode-string 'q-encoding-ccl-encode-string))
  (t
    (defalias 'q-encoding-encode-string 'q-encoding-internal-encode-string)))

(cond
  ((fboundp 'q-encoding-ccl-decode-string)
    (defalias 'q-encoding-decode-string 'q-encoding-ccl-decode-string))
  (t
    (defalias 'q-encoding-decode-string 'q-encoding-internal-decode-string)))

(cond
  (t
    (defalias 'q-encoding-encoded-length 'q-encoding-internal-encoded-length)))

(cond
  (t
    (defalias 'uuencode-encode-region 'uuencode-external-encode-region)))

(cond
  (t
    (defalias 'uuencode-decode-region 'uuencode-external-decode-region)))

(cond
  (t
    (defalias 'uuencode-insert-encoded-file 'uuencode-external-insert-encoded-file)))

(cond
  (t
    (defalias 'uuencode-write-decoded-region 'uuencode-external-write-decoded-region)))

(cond
  (t
    (defalias 'gzip64-encode-region 'gzip64-external-encode-region)))

(cond
  (t
    (defalias 'gzip64-decode-region 'gzip64-external-decode-region)))

(cond
  (t
    (defalias 'gzip64-insert-encoded-file 'gzip64-external-insert-encoded-file)))

(cond
  (t
    (defalias 'gzip64-write-decoded-region 'gzip64-external-write-decoded-region)))

;;; @ region
;;;

;;;###autoload
(defvar mime-encoding-method-alist
  '(("base64"           . base64-encode-region)
    ("quoted-printable" . quoted-printable-encode-region)
    ;; Not standard, their use is DISCOURAGED.
    ;; ("x-uue"            . uuencode-encode-region)
    ;; ("x-gzip64"         . gzip64-encode-region)
    ("7bit")
    ("8bit")
    ("binary")
    )
  "Alist of encoding vs. corresponding method to encode region.
Each element looks like (STRING . FUNCTION) or (STRING . nil).
STRING is content-transfer-encoding.
FUNCTION is region encoder and nil means not to encode.")

;;;###autoload
(defvar mime-decoding-method-alist
  '(("base64"           . base64-decode-region)
    ("quoted-printable" . quoted-printable-decode-region)
    ("x-uue"            . uuencode-decode-region)
    ("x-uuencode"       . uuencode-decode-region)
    ("x-gzip64"         . gzip64-decode-region)
    )
  "Alist of encoding vs. corresponding method to decode region.
Each element looks like (STRING . FUNCTION).
STRING is content-transfer-encoding.
FUNCTION is region decoder.")

;;;###autoload
(defun mime-encode-region (start end encoding)
  "Encode region START to END of current buffer using ENCODING.
ENCODING must be string.  If ENCODING is found in
`mime-encoding-method-alist' as its key, this function encodes the
region by its value."
  (interactive
   (list (region-beginning) (region-end)
	 (completing-read "encoding: "
			  mime-encoding-method-alist
			  nil t "base64"))
   )
  (let ((f (cdr (assoc encoding mime-encoding-method-alist))))
    (if f
	(funcall f start end)
      )))

;;;###autoload
(defun mime-decode-region (start end encoding)
  "Decode region START to END of current buffer using ENCODING.
ENCODING must be string.  If ENCODING is found in
`mime-decoding-method-alist' as its key, this function decodes the
region by its value."
  (interactive
   (list (region-beginning) (region-end)
	 (completing-read "encoding: "
			  mime-decoding-method-alist
			  nil t "base64"))
   )
  (let ((f (cdr (assoc encoding mime-decoding-method-alist))))
    (if f
	(funcall f start end)
      )))


;;; @ string
;;;

;;;###autoload
(defvar mime-string-decoding-method-alist
  '(("base64"           . base64-decode-string)
    ("quoted-printable" . quoted-printable-decode-string)
    ("7bit"		. identity)
    ("8bit"		. identity)
    ("binary"		. identity)
    )
  "Alist of encoding vs. corresponding method to decode string.
Each element looks like (STRING . FUNCTION).
STRING is content-transfer-encoding.
FUNCTION is string decoder.")

;;;###autoload
(defun mime-decode-string (string encoding)
  "Decode STRING using ENCODING.
ENCODING must be string.  If ENCODING is found in
`mime-string-decoding-method-alist' as its key, this function decodes
the STRING by its value."
  (let ((f (cdr (assoc encoding mime-string-decoding-method-alist))))
    (if f
	(funcall f string)
      (with-temp-buffer
	(insert string)
	(mime-decode-region (point-min)(point-max) encoding)
	(buffer-string)
	))))


;;; @ file
;;;

;;;###autoload
(defvar mime-file-encoding-method-alist
  '(("base64"           . base64-insert-encoded-file)
    ("quoted-printable" . quoted-printable-insert-encoded-file)
    ;; Not standard, their use is DISCOURAGED.
    ;; ("x-uue"            . uuencode-insert-encoded-file)
    ;; ("x-gzip64"         . gzip64-insert-encoded-file)
    ("7bit"		. insert-file-contents-as-binary)
    ("8bit"		. insert-file-contents-as-binary)
    ("binary"		. insert-file-contents-as-binary)
    )
  "Alist of encoding vs. corresponding method to insert encoded file.
Each element looks like (STRING . FUNCTION).
STRING is content-transfer-encoding.
FUNCTION is function to insert encoded file.")

;;;###autoload
(defvar mime-file-decoding-method-alist
  '(("base64"           . base64-write-decoded-region)
    ("quoted-printable" . quoted-printable-write-decoded-region)
    ("x-uue"            . uuencode-write-decoded-region)
    ("x-gzip64"         . gzip64-write-decoded-region)
    ("7bit"		. write-region-as-binary)
    ("8bit"		. write-region-as-binary)
    ("binary"		. write-region-as-binary)
    )
  "Alist of encoding vs. corresponding method to write decoded region to file.
Each element looks like (STRING . FUNCTION).
STRING is content-transfer-encoding.
FUNCTION is function to write decoded region to file.")

;;;###autoload
(defun mime-insert-encoded-file (filename encoding)
  "Insert file FILENAME encoded by ENCODING format."
  (interactive
   (list (read-file-name "Insert encoded file: ")
	 (completing-read "encoding: "
			  mime-encoding-method-alist
			  nil t "base64"))
   )
  (let ((f (cdr (assoc encoding mime-file-encoding-method-alist))))
    (if f
	(funcall f filename)
      )))

;;;###autoload
(defun mime-write-decoded-region (start end filename encoding)
  "Decode and write current region encoded by ENCODING into FILENAME.
START and END are buffer positions."
  (interactive
   (list (region-beginning) (region-end)
	 (read-file-name "Write decoded region to file: ")
	 (completing-read "encoding: "
			  mime-file-decoding-method-alist
			  nil t "base64")))
  (let ((f (cdr (assoc encoding mime-file-decoding-method-alist))))
    (if f
	(funcall f start end filename)
      )))


;;; @ end
;;;

(provide 'mel)

;;; mel.el ends here.
