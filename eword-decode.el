;;; eword-decode.el --- RFC 2047 based encoded-word decoder for GNU Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <tomo@m17n.org>
;;         TANAKA Akira <akr@m17n.org>
;; Created: 1995/10/03
;; Original: 1992/07/20 ENAMI Tsugutomo's `mime.el'.
;;	Renamed: 1993/06/03 to tiny-mime.el by MORIOKA Tomohiko
;;	Renamed: 1995/10/03 to tm-ew-d.el (split off encoder)
;;               by MORIOKA Tomohiko
;;	Renamed: 1997/02/22 from tm-ew-d.el by MORIOKA Tomohiko
;; Keywords: encoded-word, MIME, multilingual, header, mail, news

;; This file is part of FLIM (Faithful Library about Internet Message).

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'mime-def)
(require 'mel)
(require 'std11)
(require 'cl-lib)


;;; @ Variables
;;;

;; User options are defined in mime-def.el.


;;; @ MIME encoded-word definition
;;;

(eval-and-compile
  (defconst eword-encoded-text-regexp "[!->@-~]+")

  (defconst eword-encoded-word-regexp
    (eval-when-compile
      (concat (regexp-quote "=?")
	      "\\("
	      mime-charset-regexp	; 1
	      "\\)"
	      "\\("
	      (regexp-quote "*")
	      mime-language-regexp	; 2
	      "\\)?"
	      (regexp-quote "?")
	      "\\("
	      mime-encoding-regexp	; 3
	      "\\)"
	      (regexp-quote "?")
	      "\\("
	      eword-encoded-text-regexp	; 4
	      "\\)"
	      (regexp-quote "?="))))
  )


;;; @ for string
;;;

(defun eword-decode-string (string &optional must-unfold)
  "Decode MIME encoded-words in STRING.

STRING is unfolded before decoding.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape).

The language informations specified in the encoded words, if any, are
put to the decoded text as the `mime-language' text property."
  (setq string (std11-unfold-string string))
  (let ((regexp (concat "[\n\t ]*\\(" eword-encoded-word-regexp "\\)"))
	(next 0)
	match start words)
    (while (setq match (string-match regexp string next))
      (setq start (match-beginning 1)
	    words nil)
      (while match
	(setq next (match-end 0))
	(push (list (match-string 2 string) ;; charset
		    (when (match-beginning 3) ;; language
		      (intern
		       (downcase
			(substring string
				   (1+ (match-beginning 3)) (match-end 3)))))
		    (match-string 4 string) ;; encoding
		    (match-string 5 string) ;; encoded-text
		    (match-string 1 string)) ;; encoded-word
	      words)
	(setq match (and (string-match regexp string next)
			 (= next (match-beginning 0)))))
      (setq words (eword-decode-encoded-words (nreverse words) must-unfold)
	    string (concat (substring string 0 start)
			   words
			   (substring string next))
	    next (+ start (length words)))))
  string)

(defun eword-decode-structured-field-body
    (string &optional _start-column _max-column start)
  (let ((tokens (eword-lexical-analyze string start 'must-unfold))
	result
	token)
    (while tokens
      (setq token (car tokens))
      (setq result (cons (eword-decode-token token) result))
      (setq tokens (cdr tokens)))
    (apply 'concat (nreverse result))))

(defun eword-decode-and-unfold-structured-field-body
    (string &optional _start-column _max-column start)
  "Decode and unfold STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded."
  (let ((tokens (eword-lexical-analyze string start 'must-unfold))
	result)
    (while tokens
      (let* ((token (car tokens))
	     (type (car token)))
	(setq tokens (cdr tokens))
	(setq result
	      (cons (if (eq type 'spaces)
			" "
		      (eword-decode-token token))
		    result
		    ))))
    (apply 'concat (nreverse result))))

(defun eword-decode-and-fold-structured-field-body (string
						    start-column
						    &optional max-column
						    start)
  (if (and mime-field-decoding-max-size
	   (> (length string) mime-field-decoding-max-size))
      string
    (or max-column
	(setq max-column fill-column))
    (let ((c start-column)
	  (tokens (eword-lexical-analyze string start 'must-unfold))
	  result
	  token)
      (while (and (setq token (car tokens))
		  (setq tokens (cdr tokens)))
	(let* ((type (car token)))
	  (if (eq type 'spaces)
	      (let* ((next-token (car tokens))
		     (next-str (eword-decode-token next-token))
		     (next-len (string-width next-str))
		     (next-c (+ c next-len 1)))
		(if (< next-c max-column)
		    (setq result (cons next-str (cons " " result))
			  c next-c)
		  (setq result (cons next-str (cons "\n " result))
			c (1+ next-len)))
		(setq tokens (cdr tokens))
		)
	    (let* ((str (eword-decode-token token)))
	      (setq result (cons str result)
		    c (+ c (string-width str)))
	      ))))
      (apply 'concat (nreverse
		      (cons (when token (eword-decode-token token))
			    result))))))

(defun eword-decode-unstructured-field-body
    (string &optional _start-column _max-column)
  (eword-decode-string
   (mime-charset-decode-string string default-mime-charset)))

(defun eword-decode-and-unfold-unstructured-field-body
    (string &optional _start-column _max-column)
  (eword-decode-string
   (mime-charset-decode-string (std11-unfold-string string)
			       default-mime-charset)
   'must-unfold))

(defun eword-decode-unfolded-unstructured-field-body
    (string &optional _start-column _max-column)
  (eword-decode-string
   (mime-charset-decode-string string default-mime-charset)
   'must-unfold))


;;; @ for region
;;;

(defun eword-decode-region (start end &optional unfolding must-unfold)
  "Decode MIME encoded-words in region between START and END.

If UNFOLDING is not nil, it unfolds before decoding.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape).

The language informations specified in the encoded words, if any, are
put to the decoded text as the `mime-language' text property."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (if unfolding
	  (eword-decode-unfold))
      (goto-char (point-min))
      (let ((regexp (concat "[\n\t ]*\\(" eword-encoded-word-regexp "\\)"))
	    match words)
	(while (setq match (re-search-forward regexp nil t))
	  (setq start (match-beginning 1)
		words nil)
	  (while match
	    (goto-char (setq end (match-end 0)))
	    (push (list (match-string 2) ;; charset
			(when (match-beginning 3) ;; language
			  (intern
			   (downcase
			    (buffer-substring (1+ (match-beginning 3))
					      (match-end 3)))))
			(match-string 4) ;; encoding
			(match-string 5) ;; encoded-text
			(match-string 1)) ;; encoded-word
		  words)
	    (setq match (looking-at regexp)))
	  (delete-region start end)
	  (insert
	   (eword-decode-encoded-words (nreverse words) must-unfold)))))))

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

(defvar mime-update-field-decoder-cache 'mime-update-field-decoder-cache
  "*Field decoder cache update function.")

;;;###autoload
(defun mime-set-field-decoder (field &rest specs)
  "Set decoder of FIELD.
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
(defun mime-decode-field-body (field-body field-name
					  &optional mode max-column)
  "Decode FIELD-BODY as FIELD-NAME in MODE, and return the result.
Optional argument MODE must be `plain', `wide', `summary' or `nov'.
Default mode is `summary'.

If MODE is `wide' and MAX-COLUMN is non-nil, the result is folded with
MAX-COLUMN.

Non MIME encoded-word part in FILED-BODY is decoded with
`default-mime-charset'."
  (let (field-name-symbol len decoder)
    (if (symbolp field-name)
        (setq field-name-symbol field-name
              len (1+ (string-width (symbol-name field-name))))
      (setq field-name-symbol (intern (capitalize field-name))
            len (1+ (string-width field-name))))
    (setq decoder (mime-find-field-decoder field-name-symbol mode))
    (if decoder
	(funcall decoder field-body len max-column)
      ;; Don't decode
      (if (eq mode 'summary)
	  (std11-unfold-string field-body)
	field-body)
      )))

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
		  beg p end field-name len field-decoder)
	      (goto-char (point-min))
	      (while (re-search-forward std11-field-head-regexp nil t)
		(setq beg (match-beginning 0)
		      p (match-end 0)
		      field-name (buffer-substring beg (1- p))
		      len (string-width field-name)
		      field-name (intern (capitalize field-name))
		      field-decoder (inline
				      (mime-find-field-decoder-internal
				       field-name mode-obj)))
		(when field-decoder
		  (setq end (std11-field-end))
		  (let ((body (buffer-substring p end))
			(default-mime-charset default-charset))
		    (delete-region p end)
		    (insert (funcall field-decoder body (1+ len)))
		    ))
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

(defalias 'eword-decode-header 'mime-decode-header-in-buffer)
(make-obsolete 'eword-decode-header
	       'mime-decode-header-in-buffer "28 Oct 1998")


;;; @ encoded-words decoder
;;;

(defvar eword-decode-allow-incomplete-encoded-text t
  "*Non-nil means allow incomplete encoded-text in successive encoded-words.
Dividing of encoded-text in the place other than character boundaries
violates RFC2047 section 5, while we have a capability to decode it.
If it is non-nil, the decoder will decode B- or Q-encoding in each
encoded-word, concatenate them, and decode it by charset.  Otherwise,
the decoder will fully decode each encoded-word before concatenating
them.")

(defun eword-decode-encoded-words (words must-unfold)
  "Decode successive encoded-words in WORDS and return a decoded string.
Each element of WORDS looks like (CHARSET LANGUAGE ENCODING ENCODED-TEXT
ENCODED-WORD).

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (let (word language charset encoding text rest)
    (while words
      (setq word (pop words)
	    language (nth 1 word))
      (if (and (or (mime-charset-to-coding-system (setq charset (car word)))
		   (progn
		     (message "Unknown charset: %s" charset)
		     nil))
	       (cond ((member (setq encoding (nth 2 word)) '("B" "Q"))
		      t)
		     ((member encoding '("b" "q"))
		      (setq encoding (upcase encoding)))
		     (t
		      (message "Invalid encoding: %s" encoding)
		      nil))
	       (condition-case err
		   (setq text
			 (encoded-text-decode-string (nth 3 word) encoding))
		 (error
		  (message "%s" (error-message-string err))
		  nil)))
	  (if (and eword-decode-allow-incomplete-encoded-text
		   rest
		   (cl-caaar rest)
		   (string-equal (downcase charset) (downcase (cl-caaar rest)))
		   (equal language (cl-cdaar rest)))
	      ;; Concatenate text of which the charset is the same.
	      (setcdr (car rest) (concat (cdar rest) text))
	    (push (cons (cons charset language) text) rest))
	;; Don't decode encoded-word.
	(push (cons (cons nil language) (nth 4 word)) rest)))
    (while rest
      (setq word (or (and (setq charset (cl-caaar rest))
			  (condition-case err
			      (mime-charset-decode-string (cdar rest) charset)
			    (error
			     (message "%s" (error-message-string err))
			     nil)))
		     (concat
		      (when (cdr rest) " ")
		      (cdar rest)
		      (when (and words
				 (not (eq (string-to-char (car words)) ? )))
			" "))))
      (when must-unfold
	(setq word (mapconcat (lambda (chr)
				(cond ((memq chr '(?\n ?\r)) nil)
				      ((eq chr ?\t) " ")
				      (t (list chr))))
			      (std11-unfold-string word) nil)))
      (when (setq language (cl-cdaar rest))
	(put-text-property 0 (length word) 'mime-language language word))
      (when (> (length word) 0) (setq words (cons word words)))
      (setq rest (cdr rest)))
    (apply 'concat words)))

;;; @ lexical analyze
;;;

(defvar eword-lexical-analyze-cache nil)
(defvar eword-lexical-analyze-cache-max 299
  "*Max position of eword-lexical-analyze-cache.
It is max size of eword-lexical-analyze-cache - 1.")

(defvar mime-header-lexical-analyzer
  '(eword-analyze-quoted-string
    eword-analyze-domain-literal
    eword-analyze-comment
    eword-analyze-spaces
    eword-analyze-special
    eword-analyze-encoded-word
    eword-analyze-atom)
  "*List of functions to return result of lexical analyze.
Each function must have three arguments: STRING, START and MUST-UNFOLD.
STRING is the target string to be analyzed.
START is start position of STRING to analyze.
If MUST-UNFOLD is not nil, each function must unfold and eliminate
bare-CR and bare-LF from the result even if they are included in
content of the encoded-word.
Each function must return nil if it can not analyze STRING as its
format.

Previous function is preferred to next function.  If a function
returns nil, next function is used.  Otherwise the return value will
be the result.")

(defun eword-analyze-quoted-string (string start &optional _must-unfold)
  (let ((p (std11-check-enclosure string ?\" ?\" nil start))
	ret)
    (when p
      (setq ret (mime-charset-decode-string
		 (std11-strip-quoted-pair
		  (substring string (1+ start) (1- p)))
		 default-mime-charset))
      (if mime-header-accept-quoted-encoded-words
	  (setq ret (eword-decode-string ret)))
      (cons (cons 'quoted-string ret)
	    p))))

(defun eword-analyze-domain-literal (string start &optional _must-unfold)
  (std11-analyze-domain-literal string start))

(defun eword-analyze-comment (string from &optional must-unfold)
  (let ((len (length string))
	(i (or from 0))
	dest last-str
	chr ret)
    (when (and (> len i)
	       (eq (aref string i) ?\())
      (setq i (1+ i)
	    from i)
      (catch 'tag
	(while (< i len)
	  (setq chr (aref string i))
	  (cond ((eq chr ?\\)
		 (setq i (1+ i))
		 (if (>= i len)
		     (throw 'tag nil)
		   )
		 (setq last-str (cons (list (aref string i))
				      (cons (substring string from (1- i))
					    last-str))
		       i (1+ i)
		       from i)
		 )
		((eq chr ?\))
		 (setq ret
		       (apply 'concat
			      (substring string from i) (nreverse last-str)))
		 (throw 'tag (cons
			      (cons 'comment
				    (nreverse
				     (if (string= ret "")
					 dest
				       (cons
					(eword-decode-string
					 (mime-charset-decode-string
					  ret default-mime-charset)
					 must-unfold)
					dest)
				       )))
			      (1+ i)))
		 )
		((eq chr ?\()
		 (if (setq ret (eword-analyze-comment string i must-unfold))
		     (setq last-str
			   (apply 'concat (substring string from i)
				  (nreverse last-str))
			   dest
			   (if (string= last-str "")
			       (cons (car ret) dest)
			     (cl-list* (car ret)
				    (eword-decode-string
				     (mime-charset-decode-string
				      last-str default-mime-charset)
				     must-unfold)
				    dest)
			     )
			   i (cdr ret)
			   from i
			   last-str nil)
		   (throw 'tag nil)
		   ))
		(t
		 (setq i (1+ i))
		 ))
	  )))))

(defun eword-analyze-spaces (string start &optional _must-unfold)
  (std11-analyze-spaces string start))

(defun eword-analyze-special (string start &optional _must-unfold)
  (std11-analyze-special string start))

(defun eword-analyze-encoded-word (string start &optional must-unfold)
  (let* ((regexp (concat "[\n\t ]*\\(" eword-encoded-word-regexp "\\)"))
	 (match (and (string-match regexp string start)
		     (= start (match-beginning 0))))
	 next words)
    (while match
      (setq next (match-end 0))
      (push (list (match-string 2 string) ;; charset
		  (when (match-beginning 3) ;; language
		    (intern
		     (downcase
		      (substring string
				 (1+ (match-beginning 3)) (match-end 3)))))
		  (match-string 4 string) ;; encoding
		  (match-string 5 string) ;; encoded-text
		  (match-string 1 string)) ;; encoded-word
	    words)
      (setq match (and (string-match regexp string next)
		       (= next (match-beginning 0)))))
    (when words
      (setq words (eword-decode-encoded-words (nreverse words) must-unfold))
      (cons
       (cons 'atom
	     (if (and (string-match (eval-when-compile
				      (concat "[" std11-special-char-list "]"))
				    words)
		      (null (eq (cdr (std11-analyze-quoted-string words 0))
				(length words))))
		 ;; Docoded words contains non-atom special chars and are
		 ;; not quoted.
		 (std11-wrap-as-quoted-string words)
	       words))
       next))))

(defun eword-analyze-atom (string start &optional _must-unfold)
  (if (and (string-match std11-atom-regexp string start)
	   (= (match-beginning 0) start))
      (let ((end (match-end 0)))
	(cons (cons 'atom (mime-charset-decode-string
			   (substring string start end)
			   default-mime-charset))
	      ;;(substring string end)
	      end)
	)))

(defun eword-lexical-analyze-internal (string start must-unfold)
  (let ((len (length string))
	dest ret)
    (while (< start len)
      (setq ret
	    (let ((rest mime-header-lexical-analyzer)
		  func r)
	      (while (and (setq func (car rest))
			  (null
			   (setq r (funcall func string start must-unfold)))
			  )
		(setq rest (cdr rest)))
	      (or r
		  (cons (cons 'error (substring string start)) (1+ len)))
	      ))
      (setq dest (cons (car ret) dest)
	    start (cdr ret))
      )
    (nreverse dest)
    ))

(defun eword-lexical-analyze (string &optional start must-unfold)
  "Return lexical analyzed list corresponding STRING.
It is like std11-lexical-analyze, but it decodes non us-ascii
characters encoded as encoded-words or invalid \"raw\" format.
\"Raw\" non us-ascii characters are regarded as variable
`default-mime-charset'."
  (let ((key (substring string (or start 0)))
	ret cell)
    (set-text-properties 0 (length key) nil key)
    (if (setq ret (assoc key eword-lexical-analyze-cache))
	(cdr ret)
      (setq ret (eword-lexical-analyze-internal key 0 must-unfold))
      (setq eword-lexical-analyze-cache
	    (cons (cons key ret)
		  eword-lexical-analyze-cache))
      (if (cdr (setq cell (nthcdr eword-lexical-analyze-cache-max
				  eword-lexical-analyze-cache)))
	  (setcdr cell nil))
      ret)))

(defun eword-decode-token (token)
  (let ((type (car token))
	(value (cdr token)))
    (cond ((eq type 'quoted-string)
	   (std11-wrap-as-quoted-string value))
	  ((eq type 'comment)
	   (let (dest)
	     (while value
	       (setq dest (cons (if (stringp (car value))
				    (std11-wrap-as-quoted-pairs
				     (car value) '(?\( ?\)))
				  (eword-decode-token (car value)))
				dest)
		     value (cdr value)))
	     (apply 'concat "(" (nreverse (cons ")" dest)))))
	  (t value))))

(defun eword-extract-address-components (string &optional start)
  "Extract full name and canonical address from STRING.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'."
  (let* ((structure (car (std11-parse-address
			  (eword-lexical-analyze
			   (std11-unfold-string string) start
			   'must-unfold))))
         (phrase  (std11-full-name-string structure))
         (address (std11-address-string structure))
         )
    (list phrase address)
    ))


;;; @ end
;;;

(provide 'eword-decode)

;;; eword-decode.el ends here
