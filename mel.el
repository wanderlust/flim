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

(require 'pccl)


;;; @ encoder/decoder selection framework
;;;

(defconst mel-stems '(dl ccl int-ext internal external)
  "List of encoder/decoder stems. First stem is most prefered.")

(defmacro mel-call-next (fun formal-args)
  (let ((caller 'funcall)
        actual-args)
    (while formal-args
      (cond
       ((eq (car formal-args) '&optional) nil)
       ((eq (car formal-args) '&rest) (setq caller 'apply))
       (t (setq actual-args (cons (car formal-args) actual-args))))
      (setq formal-args (cdr formal-args)))
    `(,caller ',fun ,@(nreverse actual-args))))

(put 'mel-defgeneric 'lisp-indent-function 4)
(defmacro mel-defgeneric (prefix suffix formal-args
				 &rest docstring-interactive)
  "Define a generic function named PREFIX-SUFFIX for mel.
Arguments for the function is specified as FORMAL-ARGS as usual.
Rest of arguments DOCSTRING-INTERACTIVE should be DOCSTRING and/or
interactive specification placed at front of a function body.

Before a generic function is called, at least one methods must be
defined by `mel-defmethod'.  If more than one methods is defined,
preferest implementation is choosed by `mel-defpreference' and
`mel-stems'."
  (let ((name (intern (format "%s-%s" prefix suffix)))
        (tmp (make-symbol "tmp")))
    (put name 'prefix prefix)
    (put name 'suffix suffix)
    `(progn
       (put ',name 'prefix ',prefix)
       (put ',name 'suffix ',suffix)
       (defun ,name ,formal-args
	 ,@docstring-interactive
	 (catch 'return
	   (let ((,tmp (or (get ',name 'stems)
                           (get ',prefix 'stems)
                           mel-stems))
                 method)
	     (while ,tmp
	       (when (setq method (get ',name (car ,tmp)))
		 (fset ',name method)
		 (throw 'return (mel-call-next ,name ,formal-args)))
	       (setq ,tmp (cdr ,tmp))))
	   (error ,(format "%s: no method" name)))))))

(defun mel-defpreference (stems prefix &optional suffix)
  "Define a preference for a generic functions PREFIX-*
(or PREFIX-SUFFIX if SUFFIX is non-nil) as STEMS."
  (let ((name (if suffix (intern (format "%s-%s" prefix suffix)) prefix)))
    (put name 'stems stems)))

(defmacro mel-usemodule (file prefix stem &optional condition)
  "Declare that FILE defines functions PREFIX-STEM-*.

If the form CONDITION is non-nil, it is evaluated for each methods
PREFIX-STEM-*.  If the value of CONDITION is nil, the method is NOT
defined.  In CONDITION, five variables `prefix', `stem', `suffix',
`prefix-stem' and `prefix-stem-suffix' is available."
  (let ((prefix-stem (intern (format "%s-%s" prefix stem))))
    `(progn
      (put ',prefix-stem 'mel-condition ',(or condition t))
      (put ',prefix ',stem ,file))))

(defmacro mel-defmethod (name stem &optional condition file)
  "Declare that NAME is implemented by STEM in FILE.

If the form CONDITION is non-nil and evaluated to nil, 
the method is NOT declared.  In CONDITION, five variables `prefix',
`stem', `suffix', `prefix-stem' and `prefix-stem-suffix' is available.

If FILE is nil, module declared with `mel-usemodule' is used."
  (let* ((prefix (get name 'prefix))
         (suffix (get name 'suffix))
         (prefix-stem (intern (format "%s-%s" prefix stem)))
         (prefix-stem-suffix (intern (format "%s-%s-%s" prefix stem suffix))))
    `(when (let ((prefix ',prefix)
                 (suffix ',suffix)
                 (stem ',stem)
                 (prefix-stem ',prefix-stem)
                 (prefix-stem-suffix ',prefix-stem-suffix))
            (and ,(or condition 't)
                 (eval (get prefix-stem 'mel-condition))))
       (autoload ',prefix-stem-suffix ,(or file `(get ',prefix ',stem)))
       (put ',name ',stem ',prefix-stem-suffix))))


;;; @ generic
;;;

(mel-defgeneric base64 encode-string (string)
  "Encode STRING with base64.")
(mel-defgeneric base64 decode-string (string)
  "Decode STRING with base64.")
(mel-defgeneric base64 encode-region (start end)
  "Encode current region with base64."
  (interactive "r"))
(mel-defgeneric base64 decode-region (start end)
  "Decode current region with base64."
  (interactive "r"))
(mel-defgeneric base64 insert-encoded-file (filename)
  "Insert a file named FILENAME as base64 encoded form."
  (interactive (list (read-file-name "Insert encoded file: "))))
(mel-defgeneric base64 write-decoded-region (start end filename)
  "Decode and write base64 encoded current region to a file named FILENAME."
  (interactive
   (list (region-beginning) (region-end)
	 (read-file-name "Write decoded region to file: "))))
(mel-defgeneric base64 encoded-length (string))

(mel-defgeneric quoted-printable encode-string (string)
  "Encode STRING with quoted-printable.")
(mel-defgeneric quoted-printable decode-string (string)
  "Decode STRING with quoted-printable.")
(mel-defgeneric quoted-printable encode-region (start end)
  "Encode current region with quoted-printable."
  (interactive "r"))
(mel-defgeneric quoted-printable decode-region (start end)
  "Decode current region with quoted-printable."
  (interactive "r"))
(mel-defgeneric quoted-printable insert-encoded-file (filename)
  "Insert a file named FILENAME as quoted-printable encoded form."
  (interactive (list (read-file-name "Insert encoded file: "))))
(mel-defgeneric quoted-printable write-decoded-region (start end filename)
  "Decode and write quoted-printable encoded current region to a file
named FILENAME."
  (interactive
   (list (region-beginning) (region-end)
	 (read-file-name "Write decoded region to file: "))))

(mel-defgeneric q-encoding encode-string (string &optional mode)
  "Encode STRING with Q-encoding.
If MODE is `text', `comment' or `phrase', the result is appropriate for
unstructured field, comment or phrase in structured field.
If MODE is nil, the result is appropriate for phrase.")
(mel-defgeneric q-encoding decode-string (string)
  "Decode STRING with Q-encoding.")
(mel-defgeneric q-encoding encoded-length (string &optional mode))

(mel-defgeneric uuencode encode-region (start end)
  "Encode current region by unofficial uuencode format."
  (interactive "*r"))
(mel-defgeneric uuencode decode-region (start end)
  "Decode current region by unofficial uuencode format."
  (interactive "*r"))
(mel-defgeneric uuencode insert-encoded-file (filename)
  "Insert file encoded by unofficial uuencode format."
  (interactive (list (read-file-name "Insert encoded file: "))))
(mel-defgeneric uuencode write-decoded-region (start end filename)
  "Decode and write current region encoded by uuencode into FILENAME."
  (interactive
   (list (region-beginning) (region-end)
         (read-file-name "Write decoded region to file: "))))

(mel-defgeneric gzip64 encode-region (start end)
  "Encode current region by unofficial gzip64 format."
  (interactive "*r"))
(mel-defgeneric gzip64 decode-region (start end)
  "Decode current region by unofficial gzip64 format."
  (interactive "*r"))
(mel-defgeneric gzip64 insert-encoded-file (filename)
  "Insert file encoded by unofficial gzip64 format."
  (interactive (list (read-file-name "Insert encoded file: "))))
(mel-defgeneric gzip64 write-decoded-region (start end filename)
  "Decode and write current region encoded by gzip64 into FILENAME."
  (interactive
   (list (region-beginning) (region-end)
         (read-file-name "Write decoded region to file: "))))


;;; @ method
;;;

;; mel-dl
(defvar base64-dl-module
  (and (fboundp 'dynamic-link)
       (let ((path (expand-file-name "base64.so" exec-directory)))
	 (and (file-exists-p path)
	      path))))

(mel-usemodule "mel-dl" base64 dl base64-dl-module)

(mel-defmethod base64-encode-string dl)
(mel-defmethod base64-decode-string dl)
(mel-defmethod base64-encode-region dl)
(mel-defmethod base64-decode-region dl)

;; mel-b
(mel-usemodule "mel-b" base64 internal)
(mel-usemodule "mel-b" base64 external)
(mel-usemodule "mel-b" base64 int-ext)

(mel-defmethod base64-encode-string internal)
(mel-defmethod base64-decode-string internal)
(mel-defmethod base64-encode-region internal)
(mel-defmethod base64-decode-region internal)
(mel-defmethod base64-insert-encoded-file internal)
(mel-defmethod base64-write-decoded-region internal)

(mel-defmethod base64-encode-string external)
(mel-defmethod base64-decode-string external)
(mel-defmethod base64-encode-region external)
(mel-defmethod base64-decode-region external)
(mel-defmethod base64-insert-encoded-file external)
(mel-defmethod base64-write-decoded-region external)

(mel-defmethod base64-encoded-length internal)

(mel-defmethod base64-decode-string int-ext)
(mel-defmethod base64-encode-region int-ext)
(mel-defmethod base64-decode-region int-ext)
(mel-defmethod base64-insert-encoded-file int-ext)
(mel-defmethod base64-write-decoded-region int-ext)

;; mel-q
(mel-usemodule "mel-q" quoted-printable internal)
(mel-usemodule "mel-q" quoted-printable external)
(mel-usemodule "mel-q" quoted-printable int-ext)
(mel-usemodule "mel-q" q-encoding internal)

(mel-defmethod quoted-printable-encode-string internal)
(mel-defmethod quoted-printable-decode-string internal)
(mel-defmethod quoted-printable-encode-region internal)
(mel-defmethod quoted-printable-decode-region internal)

(mel-defmethod quoted-printable-encode-string external)
(mel-defmethod quoted-printable-decode-string external)
(mel-defmethod quoted-printable-encode-region external)
(mel-defmethod quoted-printable-decode-region external)
(mel-defmethod quoted-printable-insert-encoded-file external)
(mel-defmethod quoted-printable-write-decoded-region external)

(mel-defmethod quoted-printable-encode-region int-ext)
(mel-defmethod quoted-printable-decode-region int-ext)

(mel-defmethod q-encoding-encode-string internal)
(mel-defmethod q-encoding-decode-string internal)
(mel-defmethod q-encoding-encoded-length internal)

;; mel-u
(mel-usemodule "mel-u" uuencode external)

(mel-defmethod uuencode-encode-region external)
(mel-defmethod uuencode-decode-region external)
(mel-defmethod uuencode-insert-encoded-file external)
(mel-defmethod uuencode-write-decoded-region external)

;; mel-g
(mel-usemodule "mel-g" gzip64 external)

(mel-defmethod gzip64-encode-region external)
(mel-defmethod gzip64-decode-region external)
(mel-defmethod gzip64-insert-encoded-file external)
(mel-defmethod gzip64-write-decoded-region external)

;; mel-ccl
(mel-usemodule "mel-ccl" base64 ccl (fboundp 'make-ccl-coding-system))
(mel-usemodule "mel-ccl" quoted-printable ccl (fboundp 'make-ccl-coding-system))
(mel-usemodule "mel-ccl" q-encoding ccl (fboundp 'make-ccl-coding-system))

(mel-defmethod base64-encode-string ccl (not (broken-p 'ccl-execute-eof-block-on-encoding-some)))
(mel-defmethod base64-encode-region ccl (not (broken-p 'ccl-execute-eof-block-on-encoding-some)))
(mel-defmethod base64-insert-encoded-file ccl (not (broken-p 'ccl-execute-eof-block-on-encoding-some)))

(mel-defmethod quoted-printable-encode-string ccl (not (broken-p 'ccl-execute-eof-block-on-encoding-some)))
(mel-defmethod quoted-printable-encode-region ccl (not (broken-p 'ccl-execute-eof-block-on-encoding-some)))
(mel-defmethod quoted-printable-insert-encoded-file ccl (not (broken-p 'ccl-execute-eof-block-on-encoding-some)))

(mel-defmethod base64-decode-string ccl)
(mel-defmethod base64-decode-region ccl)
(mel-defmethod base64-write-decoded-region ccl)

(mel-defmethod quoted-printable-decode-string ccl)
(mel-defmethod quoted-printable-decode-region ccl)
(mel-defmethod quoted-printable-write-decoded-region ccl)

(mel-defmethod q-encoding-encode-string ccl)
(mel-defmethod q-encoding-decode-string ccl)

(mel-defmethod q-encoding-encoded-length ccl (not (featurep 'xemacs)))


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
  `(("base64"           . base64-decode-region)
    ("quoted-printable" . quoted-printable-decode-region)
    ("x-uue"            . uuencode-decode-region)
    ("x-uuencode"       . uuencode-decode-region)
    ("x-gzip64"         . gzip64-decode-region)
    ,@(when (fboundp 'base64-dl-decode-region)
        '(("base64-dl" . base64-dl-decode-region)))
    ,@(when (fboundp 'base64-ccl-decode-region)
        '(("base64-ccl" . base64-ccl-decode-region)))
    ,@(when (fboundp 'base64-internal-decode-region)
        '(("base64-internal" . base64-internal-decode-region)))
    ,@(when (fboundp 'base64-external-decode-region)
        '(("base64-external" . base64-external-decode-region)))
    ,@(when (fboundp 'base64-int-ext-decode-region)
        '(("base64-int-ext" . base64-int-ext-decode-region)))
    ,@(when (fboundp 'quoted-printable-internal-decode-region)
        '(("quoted-printable-internal"
	   . quoted-printable-internal-decode-region)))
    ,@(when (fboundp 'quoted-printable-ccl-decode-region)
        '(("quoted-printable-ccl"
	   . quoted-printable-ccl-decode-region)))
    ,@(when (fboundp 'quoted-printable-external-decode-region)
        '(("quoted-printable-external"
	   . quoted-printable-external-decode-region)))
    ,@(when (fboundp 'quoted-printable-int-ext-decode-region)
        '(("quoted-printable-int-ext"
	   . quoted-printable-int-ext-decode-region)))
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
  `(("base64"           . base64-write-decoded-region)
    ("quoted-printable" . quoted-printable-write-decoded-region)
    ("x-uue"            . uuencode-write-decoded-region)
    ("x-gzip64"         . gzip64-write-decoded-region)
    ("7bit"		. write-region-as-binary)
    ("8bit"		. write-region-as-binary)
    ("binary"		. write-region-as-binary)
    ,@(when (fboundp 'base64-internal-write-decoded-region)
        '(("base64-internal" . base64-internal-write-decoded-region)))
    ,@(when (fboundp 'base64-external-write-decoded-region)
        '(("base64-external" . base64-external-write-decoded-region)))
    ,@(when (fboundp 'base64-int-ext-write-decoded-region)
        '(("base64-int-ext" . base64-int-ext-write-decoded-region)))
    ,@(when (fboundp 'base64-ccl-write-decoded-region)
        '(("base64-ccl" . base64-ccl-write-decoded-region)))
    ,@(when (fboundp 'quoted-printable-external-write-decoded-region)
        '(("quoted-printable-external"
	   . quoted-printable-external-write-decoded-region)))
    ,@(when (fboundp 'quoted-printable-ccl-write-decoded-region)
        '(("quoted-printable-ccl"
	   . quoted-printable-ccl-write-decoded-region)))
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
