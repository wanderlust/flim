;;; eword-decode.el --- RFC 2047 based encoded-word decoder for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: ENAMI Tsugutomo <enami@sys.ptg.sony.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         TANAKA Akira <akr@jaist.ac.jp>
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'std11)
(require 'mel)
(require 'mime-def)

(eval-when-compile (require 'cl))

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

(defun eword-decode-structured-field-body (string
					   &optional start-column max-column
					   start)
  (let ((tokens (eword-lexical-analyze string start 'must-unfold))
	(result "")
	token)
    (while tokens
      (setq token (car tokens))
      (setq result (concat result (eword-decode-token token)))
      (setq tokens (cdr tokens)))
    result))

(defun eword-decode-and-unfold-structured-field-body (string
						      &optional
						      start-column
						      max-column
						      start)
  "Decode and unfold STRING as structured field body.
It decodes non us-ascii characters in FULL-NAME encoded as
encoded-words or invalid \"raw\" string.  \"Raw\" non us-ascii
characters are regarded as variable `default-mime-charset'.

If an encoded-word is broken or your emacs implementation can not
decode the charset included in it, it is not decoded."
  (let ((tokens (eword-lexical-analyze string start 'must-unfold))
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

(defun eword-decode-and-fold-structured-field-body (string
						    start-column
						    &optional max-column
						    start)
  (if (and eword-max-size-to-decode
	   (> (length string) eword-max-size-to-decode))
      string
    (or max-column
	(setq max-column fill-column))
    (let ((c start-column)
	  (tokens (eword-lexical-analyze string start 'must-unfold))
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

(defun eword-decode-unstructured-field-body (string &optional start-column
						    max-column)
  (eword-decode-string
   (decode-mime-charset-string string default-mime-charset)))

(defun eword-decode-and-unfold-unstructured-field-body (string
							&optional start-column
							max-column)
  (eword-decode-string
   (decode-mime-charset-string (std11-unfold-string string)
			       default-mime-charset)
   'must-unfold))

(defun eword-decode-unfolded-unstructured-field-body (string
						      &optional start-column
						      max-column)
  (eword-decode-string
   (decode-mime-charset-string string default-mime-charset)
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

(defun ew-mime-update-field-decoder-cache (field mode)
  (require 'ew-dec)
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
		   (let* ((res (ew-decode-field field-name field-body))
			  (res (if (string= res field-body)
				   res
				 (ew-crlf-refold res
						 (length field-name)
						 (or max-column fill-column))))
			  (res (ew-crlf-to-lf res)))
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
		  beg p end field-name len field-decoder)
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
		  (setq end (std11-field-end))
		  (let ((body (buffer-substring p end))
			(default-mime-charset default-charset))
		    (delete-region p end)
		    (insert (funcall field-decoder body (1+ len)))
		    (add-text-properties beg (min (1+ (point)) (point-max))
					 (list 'original-field-name field-name
					       'original-field-body field-body))
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

(defcustom eword-lexical-analyzer
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
be the result."
  :group 'eword-decode
  :type '(repeat function))

(defun eword-analyze-quoted-string-without-encoded-word (string start &optional must-unfold)
  (let ((p (std11-check-enclosure string ?\" ?\" nil start)))
    (if p
	(cons (cons 'quoted-string
		    (decode-mime-charset-string
		     (std11-strip-quoted-pair
		      (substring string (1+ start) (1- p)))
		     default-mime-charset))
	      ;;(substring string p))
	      p)
      )))

(defun eword-analyze-quoted-string-with-encoded-word (string start &optional must-unfold)
  (let ((p (std11-check-enclosure string ?\" ?\" nil start)))
    (if p
	(cons (cons 'quoted-string
		    (let ((str
			   (std11-strip-quoted-pair
			    (substring string (1+ start) (1- p)))))
		      (if (string-match eword-encoded-word-regexp str)
			  (eword-decode-encoded-word str)
			(decode-mime-charset-string str default-mime-charset)
			)))
	      p)
      )))

(defvar eword-analyze-quoted-encoded-word nil)
(defun eword-analyze-quoted-string (string start &optional must-unfold)
  (if eword-analyze-quoted-encoded-word
      (eword-analyze-quoted-string-with-encoded-word string start must-unfold)
    (eword-analyze-quoted-string-without-encoded-word string start must-unfold)))

(defun eword-analyze-domain-literal (string start &optional must-unfold)
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
		 (setq last-str (concat last-str
					(substring string from (1- i))
					(char-to-string (aref string i)))
		       i (1+ i)
		       from i)
		 )
		((eq chr ?\))
		 (setq ret (concat last-str
				   (substring string from i)))
		 (throw 'tag (cons
			      (cons 'comment
				    (nreverse
				     (if (string= ret "")
					 dest
				       (cons
					(eword-decode-string
					 (decode-mime-charset-string
					  ret default-mime-charset)
					 must-unfold)
					dest)
				       )))
			      (1+ i)))
		 )
		((eq chr ?\()
		 (if (setq ret (eword-analyze-comment string i must-unfold))
		     (setq last-str
			   (concat last-str
				   (substring string from i))
			   dest
			   (if (string= last-str "")
			       (cons (car ret) dest)
			     (list* (car ret)
				    (eword-decode-string
				     (decode-mime-charset-string
				      last-str default-mime-charset)
				     must-unfold)
				    dest)
			     )
			   i (cdr ret)
			   from i
			   last-str "")
		   (throw 'tag nil)
		   ))
		(t
		 (setq i (1+ i))
		 ))
	  )))))

(defun eword-analyze-spaces (string start &optional must-unfold)
  (std11-analyze-spaces string start))

(defun eword-analyze-special (string start &optional must-unfold)
  (std11-analyze-special string start))

(defun eword-analyze-encoded-word (string start &optional must-unfold)
  (if (and (string-match eword-encoded-word-regexp string start)
	   (= (match-beginning 0) start))
      (let ((end (match-end 0))
	    (dest (eword-decode-encoded-word (match-string 0 string)
					     must-unfold))
	    )
	;;(setq string (substring string end))
	(setq start end)
	(while (and (string-match (eval-when-compile
				    (concat "[ \t\n]*\\("
					    eword-encoded-word-regexp
					    "\\)"))
				  string start)
		    (= (match-beginning 0) start))
	  (setq end (match-end 0))
	  (setq dest
		(concat dest
			(eword-decode-encoded-word (match-string 1 string)
						   must-unfold))
		;;string (substring string end))
		start end)
	  )
	(cons (cons 'atom dest) ;;string)
	      end)
	)))

(defun eword-analyze-atom (string start &optional must-unfold)
  (if (and (string-match std11-atom-regexp string start)
	   (= (match-beginning 0) start))
      (let ((end (match-end 0)))
	(cons (cons 'atom (decode-mime-charset-string
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
	    (let ((rest eword-lexical-analyzer)
		  func r)
	      (while (and (setq func (car rest))
			  (null
			   (setq r (funcall func string start must-unfold)))
			  )
		(setq rest (cdr rest)))
	      (or r
		  (list (cons 'error (substring string start)) (1+ len)))
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
	   (let ((dest ""))
	     (while value
	       (setq dest (concat dest
				  (if (stringp (car value))
				      (std11-wrap-as-quoted-pairs
				       (car value) '(?( ?)))
				    (eword-decode-token (car value))
				    ))
		     value (cdr value))
	       )
	     (concat "(" dest ")")
	     ))
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
