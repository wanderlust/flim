;;; eword-decode.el --- RFC 2047 based encoded-word decoder for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         TANAKA Akira <akr@jaist.ac.jp>
;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/10/03
;; Original: 1992/07/20 ENAMI Tsugutomo's `mime.el'.
;;	Renamed: 1993/06/03 to tiny-mime.el
;;	Renamed: 1995/10/03 from tiny-mime.el (split off encoder)
;;	Renamed: 1997/02/22 from tm-ew-d.el
;; Keywords: encoded-word, MIME, multilingual, header, mail, news

;; This file is part of SEMI (Spadework for Emacs MIME Interfaces).

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

(defgroup eword-decode nil
  "Encoded-word decoding"
  :group 'mime)

(defcustom eword-max-size-to-decode 1000
  "*Max size to decode header field."
  :group 'eword-decode
  :type '(choice (integer :tag "Limit (bytes)")
		 (const :tag "Don't limit" nil)))


;;; @ MIME encoded-word definition
;;;

(eval-and-compile
  (defconst eword-encoded-text-regexp "[!->@-~]+")
  )
(defconst eword-encoded-word-regexp
  (eval-when-compile
    (concat (regexp-quote "=?")
	    "\\("
	    mime-charset-regexp
	    "\\)"
	    (regexp-quote "?")
	    "\\(B\\|Q\\)"
	    (regexp-quote "?")
	    "\\("
	    eword-encoded-text-regexp
	    "\\)"
	    (regexp-quote "?="))))


;;; @ for string
;;;

(defun eword-decode-string (string &optional must-unfold)
  "Decode MIME encoded-words in STRING.

STRING is unfolded before decoding.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (setq string (std11-unfold-string string))
  (let ((dest "")(ew nil)
	beg end)
    (while (and (string-match eword-encoded-word-regexp string)
		(setq beg (match-beginning 0)
		      end (match-end 0))
		)
      (if (> beg 0)
	  (if (not
	       (and (eq ew t)
		    (string-match "^[ \t]+$" (substring string 0 beg))
		    ))
	      (setq dest (concat dest (substring string 0 beg)))
	    )
	)
      (setq dest
	    (concat dest
		    (eword-decode-encoded-word
		     (substring string beg end) must-unfold)
		    ))
      (setq string (substring string end))
      (setq ew t)
      )
    (concat dest string)
    ))

(defun eword-decode-and-fold-structured-field
  (string start-column &optional max-column must-unfold)
  "Decode and fold (fill) STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MAX-COLUMN is omitted, `fill-column' is used.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (if (and eword-max-size-to-decode
	   (> (length string) eword-max-size-to-decode))
      string
    (or max-column
	(setq max-column fill-column))
    (let ((c start-column)
	  (tokens (eword-lexical-analyze string must-unfold))
	  (result "")
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
		    (setq result (concat result " " next-str)
			  c next-c)
		  (setq result (concat result "\n " next-str)
			c (1+ next-len)))
		(setq tokens (cdr tokens))
		)
	    (let* ((str (eword-decode-token token)))
	      (setq result (concat result str)
		    c (+ c (string-width str)))
	      ))))
      (if token
	  (concat result (eword-decode-token token))
	result))))

(defun eword-decode-and-unfold-structured-field (string)
  "Decode and unfold STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded."
  (let ((tokens (eword-lexical-analyze string 'must-unfold))
	(result ""))
    (while tokens
      (let* ((token (car tokens))
	     (type (car token)))
	(setq tokens (cdr tokens))
	(setq result
	      (if (eq type 'spaces)
		  (concat result " ")
		(concat result (eword-decode-token token))
		))))
    result))

(defun eword-decode-structured-field-body (string &optional must-unfold
						  start-column max-column)
  "Decode non us-ascii characters in STRING as structured field body.
STRING is unfolded before decoding.

It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (if start-column
      ;; fold with max-column
      (eword-decode-and-fold-structured-field
       string start-column max-column must-unfold)
    ;; Don't fold
    (mapconcat (function eword-decode-token)
	       (eword-lexical-analyze string must-unfold)
	       "")
    ))

(defun eword-decode-unstructured-field-body (string &optional must-unfold)
  "Decode non us-ascii characters in STRING as unstructured field body.
STRING is unfolded before decoding.

It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (eword-decode-string
   (decode-mime-charset-string string default-mime-charset)
   must-unfold))

(defun eword-decode-and-unfold-unstructured-field (string)
  "Decode and unfold STRING as unstructured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded."
  (eword-decode-string
   (decode-mime-charset-string (std11-unfold-string string)
			       default-mime-charset)
   'must-unfold))


;;; @ for region
;;;

(defun eword-decode-region (start end &optional unfolding must-unfold)
  "Decode MIME encoded-words in region between START and END.

If UNFOLDING is not nil, it unfolds before decoding.

If MUST-UNFOLD is non-nil, it unfolds and eliminates line-breaks even
if there are in decoded encoded-words (generated by bad manner MUA
such as a version of Net$cape)."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (if unfolding
	  (eword-decode-unfold)
	)
      (goto-char (point-min))
      (while (re-search-forward (concat "\\(" eword-encoded-word-regexp "\\)"
                                        "\\(\n?[ \t]\\)+"
                                        "\\(" eword-encoded-word-regexp "\\)")
                                nil t)
	(replace-match "\\1\\6")
        (goto-char (point-min))
	)
      (while (re-search-forward eword-encoded-word-regexp nil t)
	(insert (eword-decode-encoded-word
		 (prog1
		     (buffer-substring (match-beginning 0) (match-end 0))
		   (delete-region (match-beginning 0) (match-end 0))
		   ) must-unfold))
	)
      )))


;;; @ for message header
;;;

(defcustom eword-decode-ignored-field-list
  '(Newsgroups Path Lines Nntp-Posting-Host Received Message-Id Date)
  "*List of field-names to be ignored when decoding.
Each field name must be symbol."
  :group 'eword-decode
  :type '(repeat symbol))

(defcustom eword-decode-structured-field-list
  '(Reply-To Resent-Reply-To From Resent-From Sender Resent-Sender
	     To Resent-To Cc Resent-Cc Bcc Resent-Bcc Dcc
	     Mail-Followup-To
	     Mime-Version Content-Type Content-Transfer-Encoding
	     Content-Disposition User-Agent)
  "*List of field-names to decode as structured field.
Each field name must be symbol."
  :group 'eword-decode
  :type '(repeat symbol))

(defun eword-decode-field-body
  (field-body field-name &optional unfolded max-column)
  "Decode FIELD-BODY as FIELD-NAME, and return the result.

If UNFOLDED is non-nil, it is assumed that FIELD-BODY is
already unfolded.

If MAX-COLUMN is non-nil, the result is folded with MAX-COLUMN
or `fill-column' if MAX-COLUMN is t.
Otherwise, the result is unfolded.

MIME encoded-word in FIELD-BODY is recognized according to
`eword-decode-ignored-field-list',
`eword-decode-structured-field-list' and FIELD-NAME.

Non MIME encoded-word part in FILED-BODY is decoded with
`default-mime-charset'."
  (when (eq max-column t)
    (setq max-column fill-column))
  (let ((field-name-symbol (if (symbolp field-name)
                               field-name
                             (intern (capitalize field-name))))
        (len (1+ (string-width field-name))))
    (when (symbolp field-name)
      (setq field-name (symbol-name field-name)))
    (if (memq field-name-symbol eword-decode-ignored-field-list)
        ;; Don't decode
        (if max-column
            field-body
          (std11-unfold-string field-body))
      (if (memq field-name-symbol eword-decode-structured-field-list)
          ;; Decode as structured field
          (if max-column
              (eword-decode-and-fold-structured-field
               field-body len max-column t)
            (eword-decode-and-unfold-structured-field field-body))
        ;; Decode as unstructured field
        (if max-column
            (eword-decode-unstructured-field-body field-body len)
          (eword-decode-unstructured-field-body
           (std11-unfold-string field-body) len))))))

(defun eword-decode-header (&optional code-conversion separator)
  "Decode MIME encoded-words in header fields.
If CODE-CONVERSION is nil, it decodes only encoded-words.  If it is
mime-charset, it decodes non-ASCII bit patterns as the mime-charset.
Otherwise it decodes non-ASCII bit patterns as the
default-mime-charset.
If SEPARATOR is not nil, it is used as header separator."
  (interactive "*")
  (save-excursion
    (save-restriction
      (std11-narrow-to-header separator)
      (let ((default-charset
	      (if code-conversion
		  (if (mime-charset-to-coding-system code-conversion)
		      code-conversion
		    default-mime-charset))))
	(if default-charset
	    (let (beg p end field-name len)
	      (goto-char (point-min))
	      (while (re-search-forward std11-field-head-regexp nil t)
		(setq beg (match-beginning 0)
		      p (match-end 0)
		      field-name (buffer-substring beg (1- p))
		      len (string-width field-name)
		      field-name (intern (capitalize field-name))
		      end (std11-field-end))
		(cond ((memq field-name eword-decode-ignored-field-list)
		       ;; Don't decode
		       )
		      ((memq field-name eword-decode-structured-field-list)
		       ;; Decode as structured field
		       (let ((body (buffer-substring p end))
			     (default-mime-charset default-charset))
			 (delete-region p end)
			 (insert (eword-decode-and-fold-structured-field
				  body (1+ len)))
			 ))
		      (t
		       ;; Decode as unstructured field
		       (save-restriction
			 (narrow-to-region beg (1+ end))
			 (decode-mime-charset-region p end default-charset)
			 (goto-char p)
			 (if (re-search-forward eword-encoded-word-regexp
						nil t)
			     (eword-decode-region beg (point-max) 'unfold))
			 )))))
	  (eword-decode-region (point-min) (point-max) t)
	  )))))

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
		    (decode-mime-charset-string
		     (std11-strip-quoted-pair (substring string 1 (1- p)))
		     default-mime-charset))
	      (substring string p))
      )))

(defun eword-analyze-domain-literal (string &optional must-unfold)
  (std11-analyze-domain-literal string))

(defun eword-analyze-comment (string &optional must-unfold)
  (let ((p (std11-check-enclosure string ?\( ?\) t)))
    (if p
	(cons (cons 'comment
		    (eword-decode-string
		     (decode-mime-charset-string
		      (std11-strip-quoted-pair (substring string 1 (1- p)))
		      default-mime-charset)
		     must-unfold))
	      (substring string p))
      )))

(defun eword-analyze-spaces (string &optional must-unfold)
  (std11-analyze-spaces string))

(defun eword-analyze-special (string &optional must-unfold)
  (std11-analyze-special string))

(defun eword-analyze-encoded-word (string &optional must-unfold)
  (if (eq (string-match eword-encoded-word-regexp string) 0)
      (let ((end (match-end 0))
	    (dest (eword-decode-encoded-word (match-string 0 string)
					     must-unfold))
	    )
	(setq string (substring string end))
	(while (eq (string-match `,(concat "[ \t\n]*\\("
					   eword-encoded-word-regexp
					   "\\)")
				 string)
		   0)
	  (setq end (match-end 0))
	  (setq dest
		(concat dest
			(eword-decode-encoded-word (match-string 1 string)
						   must-unfold))
		string (substring string end))
	  )
	(cons (cons 'atom dest) string)
	)))

(defun eword-analyze-atom (string &optional must-unfold)
  (if (string-match std11-atom-regexp string)
      (let ((end (match-end 0)))
	(cons (cons 'atom (decode-mime-charset-string
			   (substring string 0 end)
			   default-mime-charset))
	      (substring string end)
	      ))))

(defun eword-lexical-analyze-internal (string must-unfold)
  (let (dest ret)
    (while (not (string-equal string ""))
      (setq ret
	    (let ((rest eword-lexical-analyzers)
		  func r)
	      (while (and (setq func (car rest))
			  (null (setq r (funcall func string must-unfold)))
			  )
		(setq rest (cdr rest)))
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
  (let ((key (copy-sequence string))
	ret)
    (set-text-properties 0 (length key) nil key)
    (if (setq ret (assoc key eword-lexical-analyze-cache))
	(cdr ret)
      (setq ret (eword-lexical-analyze-internal key must-unfold))
      (setq eword-lexical-analyze-cache
	    (cons (cons key ret)
		  (last eword-lexical-analyze-cache
			eword-lexical-analyze-cache-max)))
      ret)))

(defun eword-decode-token (token)
  (let ((type (car token))
	(value (cdr token)))
    (cond ((eq type 'quoted-string)
	   (std11-wrap-as-quoted-string value))
	  ((eq type 'comment)
	   (concat "(" (std11-wrap-as-quoted-pairs value '(?( ?))) ")"))
	  (t value))))

(defun eword-extract-address-components (string)
  "Extract full name and canonical address from STRING.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'."
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
