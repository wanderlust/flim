;;; eword-decode.el --- RFC 2047 based encoded-word decoder for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Tanaka Akira <akr@jaist.ac.jp>
;; Maintainer: Tanaka Akira <akr@jaist.ac.jp>
;; Created: 1995/10/03
;; Original: 1992/07/20 ENAMI Tsugutomo's `mime.el'.
;;	Renamed: 1993/06/03 to tiny-mime.el
;;	Renamed: 1995/10/03 from tiny-mime.el (split off encoder)
;;	Renamed: 1997/02/22 from tm-ew-d.el
;; Keywords: encoded-word, MIME, multilingual, header, mail, news

;; This file is part of FLAM (Faithful Library About MIME).

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

(require 'std11)
(require 'mel)
(require 'mime-def)

(require 'ew-dec)
(require 'ew-line)

(eval-when-compile (require 'cl))

(defgroup eword-decode nil
  "Encoded-word decoding"
  :group 'mime)

;;; TEST

(defvar rotate-memo nil)
(defmacro rotate-memo (var val)
  `(when rotate-memo
     (unless (boundp ',var) (setq ,var ()))
     (setq ,var (cons ,val ,var))
     (let ((tmp (last ,var (- (length ,var) 100))))
       (when tmp (setcdr tmp nil)))
     ,var))

;;; @ variables
;;;

(defcustom eword-decode-sticked-encoded-word nil
  "*If non-nil, decode encoded-words sticked on atoms,
other encoded-words, etc.
however this behaviour violates RFC2047."
  :group 'eword-decode
  :type 'boolean)

(defcustom eword-decode-quoted-encoded-word nil
  "*If non-nil, decode encoded-words in quoted-string
however this behaviour violates RFC2047."
  :group 'eword-decode
  :type 'boolean)

(defcustom eword-max-size-to-decode 1000
  "*Max size to decode header field."
  :group 'eword-decode
  :type '(choice (integer :tag "Limit (bytes)")
		 (const :tag "Don't limit" nil)))


;;; @ MIME encoded-word definition
;;;

(defconst eword-encoded-word-prefix-regexp
  (concat (regexp-quote "=?")
	  "\\(" mime-charset-regexp "\\)"
	  (regexp-quote "?")
	  "\\(B\\|Q\\)"
	  (regexp-quote "?")))
(defconst eword-encoded-word-suffix-regexp
  (regexp-quote "?="))

(defconst eword-encoded-text-in-unstructured-regexp "[!->@-~]+")
(defconst eword-encoded-word-in-unstructured-regexp
  (concat eword-encoded-word-prefix-regexp
	  "\\(" eword-encoded-text-in-unstructured-regexp "\\)"
	  eword-encoded-word-suffix-regexp))
(defconst eword-after-encoded-word-in-unstructured-regexp "\\([ \t]\\|$\\)")

(defconst eword-encoded-text-in-phrase-regexp "[-A-Za-z0-9!*+/=_]+")
(defconst eword-encoded-word-in-phrase-regexp
  (concat eword-encoded-word-prefix-regexp
	  "\\(" eword-encoded-text-in-phrase-regexp "\\)"
	  eword-encoded-word-suffix-regexp))
(defconst eword-after-encoded-word-in-phrase-regexp "\\([ \t]\\|$\\)")

(defconst eword-encoded-text-in-comment-regexp "[]!-'*->@-[^-~]+")
(defconst eword-encoded-word-in-comment-regexp
  (concat eword-encoded-word-prefix-regexp
	  "\\(" eword-encoded-text-in-comment-regexp "\\)"
	  eword-encoded-word-suffix-regexp))
(defconst eword-after-encoded-word-in-comment-regexp "\\([ \t()\\\\]\\|$\\)")

(defconst eword-encoded-text-in-quoted-string-regexp "[]!#->@-[^-~]+")
(defconst eword-encoded-word-in-quoted-string-regexp
  (concat eword-encoded-word-prefix-regexp
	  "\\(" eword-encoded-text-in-quoted-string-regexp "\\)"
	  eword-encoded-word-suffix-regexp))
(defconst eword-after-encoded-word-in-quoted-string-regexp "\\([ \t\"\\\\]\\|$\\)")

; obsolete
(defconst eword-encoded-text-regexp eword-encoded-text-in-unstructured-regexp)
(defconst eword-encoded-word-regexp eword-encoded-word-in-unstructured-regexp)


;;; @ internal utilities
;;;

(defun eword-decode-first-encoded-words (string
					 eword-regexp
					 after-regexp
					 &optional must-unfold)
  "Decode MIME encoded-words in beginning of STRING.

EWORD-REGEXP is the regexp that matches a encoded-word.
Usual value is
eword-encoded-word-in-unstructured-regexp, 
eword-encoded-text-in-phrase-regexp,
eword-encoded-word-in-comment-regexp or
eword-encoded-word-in-quoted-string-regexp.

AFTER-REGEXP is the regexp that matches a after encoded-word.
Usual value is
eword-after-encoded-word-in-unstructured-regexp, 
eword-after-encoded-text-in-phrase-regexp,
eword-after-encoded-word-in-comment-regexp or
eword-after-encoded-word-in-quoted-string-regexp.

If beginning of STRING matches EWORD-REGEXP with AFTER-REGEXP,
returns a cons cell of decoded string(sequence of characters) and 
the rest(sequence of octets).

If beginning of STRING does not matches EWORD-REGEXP and AFTER-REGEXP,
returns nil.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is returned in decoded part
as encoded-word form.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (if eword-decode-sticked-encoded-word (setq after-regexp ""))
  (let* ((between-ewords-regexp
  	   (if eword-decode-sticked-encoded-word
	     "\\(\n?[ \t]\\)*"
	     "\\(\n?[ \t]\\)+"))
	 (between-ewords-eword-after-regexp
	   (concat "\\`\\(" between-ewords-regexp "\\)"
		      "\\(" eword-regexp "\\)"
		      after-regexp))
	 (eword-after-regexp
	   (concat "\\`\\(" eword-regexp "\\)" after-regexp))
  	 (src string)	; sequence of octets.
  	 (dst ""))	; sequence of characters.
    (if (string-match eword-after-regexp src)
      (let* (p
      	     (q (match-end 1))
      	     (ew (substring src 0 q))
      	     (dw (eword-decode-encoded-word ew must-unfold)))
        (setq dst (concat dst dw)
	      src (substring src q))
	(if (not (string= ew dw))
	  (progn
	    (while
	      (and
	        (string-match between-ewords-eword-after-regexp src)
		(progn
		  (setq p (match-end 1)
		  	q (match-end 3)
		        ew (substring src p q)
		        dw (eword-decode-encoded-word ew must-unfold))
		  (if (string= ew dw)
		    (progn
		      (setq dst (concat dst (substring src 0 q))
			    src (substring src q))
		      nil)
		    t)))
	      (setq dst (concat dst dw)
	            src (substring src q)))))
	(cons dst src))
      nil)))

(defun eword-decode-entire-string (string
				   eword-regexp
				   after-regexp
				   safe-regexp
				   escape ; ?\\ or nil.
				   delimiters ; list of chars.
                                   chars-must-be-quote
				   must-unfold
				   code-conversion)
  (if (and code-conversion
	   (not (mime-charset-to-coding-system code-conversion)))
      (setq code-conversion default-mime-charset))
  (let ((equal-safe-regexp (concat "\\`=?" safe-regexp))
  	(dst "")
	(buf "")
  	(src string)
	(ew-enable t))
    (while (< 0 (length src))
      (let ((ch (aref src 0))
      	    (decoded (and
	    		ew-enable
			(eword-decode-first-encoded-words src
			  eword-regexp after-regexp must-unfold))))
	(if (and (not (string= buf ""))
		 (or decoded (memq ch delimiters)))
	  (setq dst (concat dst
	  	      (std11-wrap-as-quoted-pairs
		        (decode-mime-charset-string buf code-conversion)
			chars-must-be-quote))
		buf ""))
	(cond
	  (decoded
	    (setq dst (concat dst
	    		(std11-wrap-as-quoted-pairs
			  (car decoded)
			  chars-must-be-quote))
		  src (cdr decoded)))
	  ((memq ch delimiters)
	    (setq dst (concat dst (list ch))
		  src (substring src 1)
		  ew-enable t))
	  ((eq ch escape)
	    (setq buf (concat buf (list (aref src 1)))
		  src (substring src 2)
		  ew-enable t))
	  ((string-match "\\`[ \t\n]+" src)
	    (setq buf (concat buf (substring src 0 (match-end 0)))
		  src (substring src (match-end 0))
		  ew-enable t))
	  ((and (string-match equal-safe-regexp src)
	  	(< 0 (match-end 0)))
	    (setq buf (concat buf (substring src 0 (match-end 0)))
		  src (substring src (match-end 0))
		  ew-enable eword-decode-sticked-encoded-word))
	  (t (error "something wrong")))))
    (if (not (string= buf ""))
      (setq dst (concat dst
      		  (std11-wrap-as-quoted-pairs
		    (decode-mime-charset-string buf code-conversion)
		    chars-must-be-quote))))
    dst))


;;; @ for string
;;;

(defun eword-decode-unstructured (string code-conversion &optional must-unfold)
  (eword-decode-entire-string
    string
    eword-encoded-word-in-unstructured-regexp
    eword-after-encoded-word-in-unstructured-regexp
    "[^ \t\n=]*"
    nil
    nil
    nil
    must-unfold
    code-conversion))

(defun eword-decode-comment (string code-conversion &optional must-unfold)
  (eword-decode-entire-string
    string
    eword-encoded-word-in-comment-regexp
    eword-after-encoded-word-in-comment-regexp
    "[^ \t\n()\\\\=]*"
    ?\\
    '(?\( ?\))
    '(?\( ?\) ?\\ ?\r ?\n)
    must-unfold
    code-conversion))

(defun eword-decode-quoted-string (string code-conversion &optional must-unfold)
  (eword-decode-entire-string
    string
    eword-encoded-word-in-quoted-string-regexp
    eword-after-encoded-word-in-quoted-string-regexp
    "[^ \t\n\"\\\\=]*"
    ?\\
    '(?\")
    '(?\" ?\\ ?\r ?\n)
    must-unfold
    code-conversion))

(defun eword-decode-string (string &optional must-unfold code-conversion)
  "Decode MIME encoded-words in STRING.

STRING is unfolded before decoding.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape).

If CODE-CONVERSION is nil, it decodes only encoded-words.  If it is
mime-charset, it decodes non-ASCII bit patterns as the mime-charset.
Otherwise it decodes non-ASCII bit patterns as the
default-mime-charset."
  (eword-decode-unstructured
    (std11-unfold-string string)
    code-conversion
    must-unfold))

(defun eword-decode-structured-field-body (string
                                           &optional 
                                           start-column max-column)
  (let* ((ew-decode-field-default-syntax '(ew-scan-unibyte-std11))
         (decoded (ew-decode-field "" (ew-lf-crlf-to-crlf string))))
    (ew-crlf-to-lf decoded)))

(defun eword-decode-and-unfold-structured-field-body (string
						      &optional
						      start-column
						      max-column)
  "Decode and unfold STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded."
  (let* ((decoded (ew-decode-field "" (ew-lf-crlf-to-crlf string))))
    (ew-crlf-to-lf (ew-crlf-unfold decoded))))

(defun eword-decode-and-fold-structured-field-body (string
						    start-column
						    &optional max-column)
  (or max-column
      (setq max-column fill-column))
  (let* ((field-name (make-string (1- start-column) ?X))
	 (field-body (ew-lf-crlf-to-crlf string))
	 (ew-decode-field-default-syntax '(ew-scan-unibyte-std11))
	 (decoded (ew-decode-field field-name field-body)))
    (unless (equal field-body decoded)
      (setq decoded (ew-crlf-refold decoded start-column max-column)))
    (ew-crlf-to-lf decoded)))

(defun eword-decode-unstructured-field-body (string &optional start-column
						    max-column)
  (let ((decoded (ew-decode-field "" (ew-lf-crlf-to-crlf string))))
    (ew-crlf-to-lf decoded)))

(defun eword-decode-and-unfold-unstructured-field-body (string
							&optional start-column
							max-column)
  (let ((decoded (ew-decode-field "" (ew-lf-crlf-to-crlf string))))
    (ew-crlf-to-lf (ew-crlf-unfold decoded))))

(defun eword-decode-unfolded-unstructured-field-body (string
						      &optional start-column
						      max-column)
  (let ((decoded (ew-decode-field "" (ew-lf-crlf-to-crlf string))))
    (ew-crlf-to-lf decoded)))


;;; @ for region
;;;

(defun eword-decode-region (start end &optional unfolding must-unfold
						code-conversion)
  "Decode MIME encoded-words in region between START and END.

If UNFOLDING is not nil, it unfolds before decoding.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape).

If CODE-CONVERSION is nil, it decodes only encoded-words.  If it is
mime-charset, it decodes non-ASCII bit patterns as the mime-charset.
Otherwise it decodes non-ASCII bit patterns as the
default-mime-charset."
  (interactive "*r")
  (rotate-memo args-eword-decode-region
	       (list start end (buffer-substring start end) unfolding must-unfold code-conversion))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (if unfolding
	  (eword-decode-unfold)
	)
      (let ((str (eword-decode-unstructured
		   (buffer-substring (point-min) (point-max))
		   code-conversion
		   must-unfold)))
	(delete-region (point-min) (point-max))
	(insert str)))))

(defun eword-decode-unfold ()
  (goto-char (point-min))
  (let (field beg end)
    (while (re-search-forward std11-field-head-regexp nil t)
      (setq beg (match-beginning 0)
            end (std11-field-end))
      (setq field (buffer-substring beg end))
      (if (string-match eword-encoded-word-regexp field)
          (save-restriction
            (narrow-to-region (goto-char beg) end)
            (while (re-search-forward "\n\\([ \t]\\)" nil t)
              (replace-match (match-string 1))
              )
	    (goto-char (point-max))
	    ))
      )))

;;; @ for message header
;;;

(defvar mime-field-decoder-alist nil)

(defvar mime-field-decoder-cache nil)

(defvar mime-update-field-decoder-cache 'ew-mime-update-field-decoder-cache
  "*Field decoder cache update function.")

;;;###autoload
(defun mime-set-field-decoder (field &rest specs)
  "Set decoder of FILED.
SPECS must be like `MODE1 DECODER1 MODE2 DECODER2 ...'.
Each mode must be `nil', `plain', `wide', `summary' or `nov'.
If mode is `nil', corresponding decoder is set up for every modes."
  (when specs
    (let ((mode (pop specs))
	  (function (pop specs)))
      (if mode
	  (progn
	    (let ((cell (assq mode mime-field-decoder-alist)))
	      (if cell
		  (setcdr cell (put-alist field function (cdr cell)))
		(setq mime-field-decoder-alist
		      (cons (cons mode (list (cons field function)))
			    mime-field-decoder-alist))
		))
	    (apply (function mime-set-field-decoder) field specs)
	    )
	(mime-set-field-decoder field
				'plain function
				'wide function
				'summary function
				'nov function)
	))))

;;;###autoload
(defmacro mime-find-field-presentation-method (name)
  "Return field-presentation-method from NAME.
NAME must be `plain', `wide', `summary' or `nov'."
  (cond ((eq name nil)
	 `(or (assq 'summary mime-field-decoder-cache)
	      '(summary))
	 )
	((and (consp name)
	      (car name)
	      (consp (cdr name))
	      (symbolp (car (cdr name)))
	      (null (cdr (cdr name))))
	 `(or (assq ,name mime-field-decoder-cache)
	      (cons ,name nil))
	 )
	(t
	 `(or (assq (or ,name 'summary) mime-field-decoder-cache)
	      (cons (or ,name 'summary) nil))
	 )))

(defun mime-find-field-decoder-internal (field &optional mode)
  "Return function to decode field-body of FIELD in MODE.
Optional argument MODE must be object of field-presentation-method."
  (cdr (or (assq field (cdr mode))
	   (prog1
	       (funcall mime-update-field-decoder-cache
			field (car mode))
	     (setcdr mode
		     (cdr (assq (car mode) mime-field-decoder-cache)))
	     ))))

;;;###autoload
(defun mime-find-field-decoder (field &optional mode)
  "Return function to decode field-body of FIELD in MODE.
Optional argument MODE must be object or name of
field-presentation-method.  Name of field-presentation-method must be
`plain', `wide', `summary' or `nov'.
Default value of MODE is `summary'."
  (if (symbolp mode)
      (let ((p (cdr (mime-find-field-presentation-method mode))))
	(if (and p (setq p (assq field p)))
	    (cdr p)
	  (cdr (funcall mime-update-field-decoder-cache
			field (or mode 'summary)))))
    (inline (mime-find-field-decoder-internal field mode))
    ))

;;;###autoload
(defun mime-update-field-decoder-cache (field mode &optional function)
  "Update field decoder cache `mime-field-decoder-cache'."
  (cond ((eq function 'identity)
	 (setq function nil)
	 )
	((null function)
	 (let ((decoder-alist
		(cdr (assq (or mode 'summary) mime-field-decoder-alist))))
	   (setq function (cdr (or (assq field decoder-alist)
				   (assq t decoder-alist)))))
	 ))
  (let ((cell (assq mode mime-field-decoder-cache))
        ret)
    (if cell
        (if (setq ret (assq field (cdr cell)))
            (setcdr ret function)
          (setcdr cell (cons (setq ret (cons field function)) (cdr cell))))
      (setq mime-field-decoder-cache
            (cons (cons mode (list (setq ret (cons field function))))
                  mime-field-decoder-cache)))
    ret))

;; ignored fields
(mime-set-field-decoder 'Archive                nil nil)
(mime-set-field-decoder 'Content-Md5            nil nil)
(mime-set-field-decoder 'Control                nil nil)
(mime-set-field-decoder 'Date			nil nil)
(mime-set-field-decoder 'Distribution           nil nil)
(mime-set-field-decoder 'Followup-Host          nil nil)
(mime-set-field-decoder 'Followup-To            nil nil)
(mime-set-field-decoder 'Lines			nil nil)
(mime-set-field-decoder 'Message-Id		nil nil)
(mime-set-field-decoder 'Newsgroups		nil nil)
(mime-set-field-decoder 'Nntp-Posting-Host	nil nil)
(mime-set-field-decoder 'Path			nil nil)
(mime-set-field-decoder 'Posted-And-Mailed      nil nil)
(mime-set-field-decoder 'Received		nil nil)
(mime-set-field-decoder 'Status                 nil nil)
(mime-set-field-decoder 'X-Face                 nil nil)
(mime-set-field-decoder 'X-Face-Version         nil nil)
(mime-set-field-decoder 'X-Info                 nil nil)
(mime-set-field-decoder 'X-Pgp-Key-Info         nil nil)
(mime-set-field-decoder 'X-Pgp-Sig              nil nil)
(mime-set-field-decoder 'X-Pgp-Sig-Version      nil nil)
(mime-set-field-decoder 'Xref                   nil nil)

;; structured fields
(let ((fields
       '(Reply-To Resent-Reply-To From Resent-From Sender Resent-Sender
	 To Resent-To Cc Resent-Cc Bcc Resent-Bcc Dcc
	 Mail-Followup-To
	 Mime-Version Content-Type Content-Transfer-Encoding
	 Content-Disposition User-Agent))
      field)
  (while fields
    (setq field (pop fields))
    (mime-set-field-decoder
     field
     'plain	#'eword-decode-structured-field-body
     'wide	#'eword-decode-and-fold-structured-field-body
     'summary	#'eword-decode-and-unfold-structured-field-body
     'nov	#'eword-decode-and-unfold-structured-field-body)
    ))

;; unstructured fields (default)
(mime-set-field-decoder
 t
 'plain	#'eword-decode-unstructured-field-body
 'wide	#'eword-decode-unstructured-field-body
 'summary #'eword-decode-and-unfold-unstructured-field-body
 'nov	#'eword-decode-unfolded-unstructured-field-body)

;;;###autoload
(defun ew-mime-update-field-decoder-cache (field mode)
  (let ((fun (cond
              ((eq mode 'plain)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (let ((res (ew-crlf-to-lf
                               (ew-decode-field field-name field-body))))
                     (add-text-properties
                      0 (length res)
                      (list 'original-field-name field-name
                            'original-field-body field-body)
                      res)
                     res))))
              ((eq mode 'wide)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (let ((res (ew-crlf-to-lf
                               (ew-crlf-refold
                                (ew-decode-field field-name field-body)
                                (length field-name)
                                (or max-column fill-column)))))
                     (add-text-properties
                      0 (length res)
                      (list 'original-field-name field-name
                            'original-field-body field-body)
                      res)
                     res))))
              ((eq mode 'summary)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (let ((res (ew-crlf-to-lf
                               (ew-crlf-unfold
                                (ew-decode-field field-name field-body)))))
                     (add-text-properties
                      0 (length res)
                      (list 'original-field-name field-name
                            'original-field-body field-body)
                      res)
                     res))))
              ((eq mode 'nov)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (require 'ew-var)
                   (let ((ew-ignore-76bytes-limit t))
                     (let ((res (ew-crlf-to-lf
                                 (ew-crlf-unfold
                                  (ew-decode-field field-name field-body)))))
                       (add-text-properties
                        0 (length res)
                        (list 'original-field-name field-name
                              'original-field-body field-body)
                        res)
                       res)))))
              (t
               nil))))
    (mime-update-field-decoder-cache field mode fun)))

;;;###autoload
(defun mime-decode-field-body (field-body field-name
					  &optional mode max-column)
  "Decode FIELD-BODY as FIELD-NAME in MODE, and return the result.
Optional argument MODE must be `plain', `wide', `summary' or `nov'.
Default mode is `summary'.

If MODE is `wide' and MAX-COLUMN is non-nil, the result is folded with
MAX-COLUMN.

Non MIME encoded-word part in FILED-BODY is decoded with
`default-mime-charset'."
  (unless mode (setq mode 'summary))
  (if (symbolp field-name) (setq field-name (symbol-name field-name)))
  (let ((decoded
          (if (eq mode 'nov)
            (let ((ew-ignore-76bytes-limit t))
              (ew-decode-field
               field-name (ew-lf-crlf-to-crlf field-body)))
            (ew-decode-field
             field-name (ew-lf-crlf-to-crlf field-body)))))
    (if (and (eq mode 'wide) max-column)
        (setq decoded (ew-crlf-refold
                       decoded
                       (1+ (string-width field-name))
                       max-column))
      (if (not (eq mode 'plain))
          (setq decoded (ew-crlf-unfold decoded))))
    (setq decoded (ew-crlf-to-lf decoded))
    (add-text-properties 0 (length decoded)
                         (list 'original-field-name field-name
                               'original-field-body field-body)
                         decoded)
    decoded))

;;;###autoload
(defun mime-decode-header-in-region (start end
					   &optional code-conversion)
  "Decode MIME encoded-words in region between START and END.
If CODE-CONVERSION is nil, it decodes only encoded-words.  If it is
mime-charset, it decodes non-ASCII bit patterns as the mime-charset.
Otherwise it decodes non-ASCII bit patterns as the
default-mime-charset."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((default-charset
	      (if code-conversion
		  (if (mime-charset-to-coding-system code-conversion)
		      code-conversion
		    default-mime-charset))))
	(if default-charset
	    (let ((mode-obj (mime-find-field-presentation-method 'wide))
		  beg p end len field-decoder
                  field-name field-body)
	      (goto-char (point-min))
	      (while (re-search-forward std11-field-head-regexp nil t)
		(setq beg (match-beginning 0)
		      p (match-end 0)
		      field-name (buffer-substring beg (1- p))
		      len (string-width field-name)
		      field-decoder (inline
				      (mime-find-field-decoder-internal
				       (intern (capitalize field-name))
                                       mode-obj)))
		(when field-decoder
		  (setq end (std11-field-end)
			field-body (buffer-substring p end))
		  (let ((default-mime-charset default-charset))
		    (delete-region p end)
		    (insert (funcall field-decoder field-body (1+ len)))
		    ))
                (add-text-properties beg (min (1+ (point)) (point-max))
                                     (list 'original-field-name field-name
                                           'original-field-body field-body))
		))
	  (eword-decode-region (point-min) (point-max) t)
	  )))))

;;;###autoload
(defun mime-decode-header-in-buffer (&optional code-conversion separator)
  "Decode MIME encoded-words in header fields.
If CODE-CONVERSION is nil, it decodes only encoded-words.  If it is
mime-charset, it decodes non-ASCII bit patterns as the mime-charset.
Otherwise it decodes non-ASCII bit patterns as the
default-mime-charset.
If SEPARATOR is not nil, it is used as header separator."
  (interactive "*")
  (mime-decode-header-in-region
   (point-min)
   (save-excursion
     (goto-char (point-min))
     (if (re-search-forward
	  (concat "^\\(" (regexp-quote (or separator "")) "\\)?$")
	  nil t)
	 (match-beginning 0)
       (point-max)
       ))
   code-conversion))

(define-obsolete-function-alias 'eword-decode-header
  'mime-decode-header-in-buffer)


;;; @ encoded-word decoder
;;;

(defvar eword-decode-encoded-word-error-handler
  'eword-decode-encoded-word-default-error-handler)

(defvar eword-warning-face nil
  "Face used for invalid encoded-word.")

(defun eword-decode-encoded-word-default-error-handler (word signal)
  (and (add-text-properties 0 (length word)
			    (and eword-warning-face
				 (list 'face eword-warning-face))
			    word)
       word))

(defun eword-decode-encoded-word (word &optional must-unfold)
  "Decode WORD if it is an encoded-word.

If your emacs implementation can not decode the charset of WORD, it
returns WORD.  Similarly the encoded-word is broken, it returns WORD.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-word (generated by bad manner MUA such
as a version of Net$cape)."
  (or (if (string-match eword-encoded-word-regexp word)
	  (let ((charset
		 (substring word (match-beginning 1) (match-end 1))
		 )
		(encoding
		 (upcase
		  (substring word (match-beginning 2) (match-end 2))
		  ))
		(text
		 (substring word (match-beginning 3) (match-end 3))
		 ))
            (condition-case err
                (eword-decode-encoded-text charset encoding text must-unfold)
              (error
	       (funcall eword-decode-encoded-word-error-handler word err)
               ))
            ))
      word))


;;; @ encoded-text decoder
;;;

(defun eword-decode-encoded-text (charset encoding string
					  &optional must-unfold)
  "Decode STRING as an encoded-text.

If your emacs implementation can not decode CHARSET, it returns nil.

If ENCODING is not \"B\" or \"Q\", it occurs error.
So you should write error-handling code if you don't want break by errors.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-text (generated by bad manner MUA such
as a version of Net$cape)."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(let ((dest (encoded-text-decode-string string encoding)))
	  (when dest
	    (setq dest (decode-mime-charset-string dest charset))
	    (if must-unfold
		(mapconcat (function
			    (lambda (chr)
			      (cond ((eq chr ?\n) "")
				    ((eq chr ?\t) " ")
				    (t (char-to-string chr)))
			      ))
			   (std11-unfold-string dest)
			   "")
	      dest))))))


;;; @ lexical analyze
;;;

(defvar eword-lexical-analyze-cache nil)
(defvar eword-lexical-analyze-cache-max 299
  "*Max position of eword-lexical-analyze-cache.
It is max size of eword-lexical-analyze-cache - 1.")

(defcustom eword-lexical-analyzers
  '(eword-analyze-quoted-string
    eword-analyze-domain-literal
    eword-analyze-comment
    eword-analyze-spaces
    eword-analyze-special
    eword-analyze-encoded-word
    eword-analyze-atom)
  "*List of functions to return result of lexical analyze.
Each function must have two arguments: STRING and MUST-UNFOLD.
STRING is the target string to be analyzed.
If MUST-UNFOLD is not nil, each function must unfold and eliminate
bare-CR and bare-LF from the result even if they are included in
content of the encoded-word.
Each function must return nil if it can not analyze STRING as its
format.

Previous function is preferred to next function.  If a function
returns nil, next function is used.  Otherwise the return value will
be the result."
  :group 'eword-decode
  :type '(repeat function))

(defun eword-analyze-quoted-string (string &optional must-unfold)
  (let ((p (std11-check-enclosure string ?\" ?\")))
    (if p
        (cons (cons 'quoted-string
                    (if eword-decode-quoted-encoded-word
                        (eword-decode-quoted-string
                          (substring string 0 p)
                          default-mime-charset)
                      (std11-wrap-as-quoted-string
                       (decode-mime-charset-string
                        (std11-strip-quoted-pair (substring string 1 (1- p)))
                        default-mime-charset))))
              (substring string p)))
    ))

(defun eword-analyze-domain-literal (string &optional must-unfold)
  (std11-analyze-domain-literal string))

(defun eword-analyze-comment (string &optional must-unfold)
  (let ((len (length string)))
    (if (and (< 0 len) (eq (aref string 0) ?\())
	(let ((p 0))
	  (while (and p (< p len) (eq (aref string p) ?\())
	    (setq p (std11-check-enclosure string ?\( ?\) t p)))
	  (setq p (or p len))
	  (cons (cons 'comment
		      (eword-decode-comment
		        (std11-unfold-string (substring string 0 p))
			default-mime-charset))
		(substring string p)))
      nil)))


(defun eword-analyze-spaces (string &optional must-unfold)
  (std11-analyze-spaces string))

(defun eword-analyze-special (string &optional must-unfold)
  (std11-analyze-special string))

(defun eword-analyze-encoded-word (string &optional must-unfold)
  (let ((decoded (eword-decode-first-encoded-words
                  string
                  eword-encoded-word-in-phrase-regexp
                  eword-after-encoded-word-in-phrase-regexp
                  must-unfold)))
    (if decoded
        (let ((s (car decoded)))
          (while (or (string-match std11-atom-regexp s)
                     (string-match std11-spaces-regexp s))
            (setq s (substring s (match-end 0))))
          (if (= (length s) 0)
              (cons (cons 'atom (car decoded)) (cdr decoded))
            (cons (cons 'quoted-string
                        (std11-wrap-as-quoted-string (car decoded)))
                  (cdr decoded)))))))

(defun eword-analyze-atom (string &optional must-unfold)
  (if (string-match std11-atom-regexp (string-as-unibyte string))
      (let ((end (match-end 0)))
	(if (and eword-decode-sticked-encoded-word
		 (string-match eword-encoded-word-in-phrase-regexp
		 	       (substring string 0 end))
		 (< 0 (match-beginning 0)))
	    (setq end (match-beginning 0)))
	(cons (cons 'atom (decode-mime-charset-string
			   (substring string 0 end)
			   default-mime-charset))
	      (substring string end)
	      ))))

(defun eword-lexical-analyze-internal (string must-unfold)
  (let ((last 'eword-analyze-spaces)
        dest ret)
    (while (not (string-equal string ""))
      (setq ret
            (let ((rest eword-lexical-analyzers)
                  func r)
              (while (and (setq func (car rest))
                          (or
                           (and
                            (not eword-decode-sticked-encoded-word)
                            (not (eq last 'eword-analyze-spaces))
                            (eq func 'eword-analyze-encoded-word))
                           (null (setq r (funcall func string must-unfold))))
                          )
                (setq rest (cdr rest)))
              (setq last func)
              (or r `((error . ,string) . ""))
              ))
      (setq dest (cons (car ret) dest))
      (setq string (cdr ret))
      )
    (nreverse dest)
    ))

(defun eword-lexical-analyze (string &optional must-unfold)
  "Return lexical analyzed list corresponding STRING.
It is like std11-lexical-analyze, but it decodes non us-ascii
characters encoded as encoded-words or invalid \"raw\" format.
\"Raw\" non us-ascii characters are regarded as variable
`default-mime-charset'."
  (let* ((str (copy-sequence string))
  	 (key (cons str (cons default-mime-charset must-unfold)))
	 ret)
    (set-text-properties 0 (length str) nil str)
    (if (setq ret (assoc key eword-lexical-analyze-cache))
	(cdr ret)
      (setq ret (eword-lexical-analyze-internal str must-unfold))
      (setq eword-lexical-analyze-cache
	    (cons (cons key ret)
		  (last eword-lexical-analyze-cache
			eword-lexical-analyze-cache-max)))
      ret)))

(defun eword-decode-token (token)
  (cdr token))

(defun eword-extract-address-components (string)
  "Extract full name and canonical address from STRING.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'."
  (rotate-memo args-eword-extract-address-components (list string))
  (let* ((structure (car (std11-parse-address
			  (eword-lexical-analyze
			   (std11-unfold-string string) 'must-unfold))))
         (phrase  (std11-full-name-string structure))
         (address (std11-address-string structure))
         )
    (list phrase address)
    ))


;;; @ end
;;;

(provide 'eword-decode)

;;; eword-decode.el ends here
