;;;
;;; $Id$
;;;

;;; @ constants
;;;

(defconst quoted-printable-hex-chars "0123456789ABCDEF")
(defconst quoted-printable-octet-regexp
  (concat "=[" quoted-printable-hex-chars
	  "][" quoted-printable-hex-chars "]"))


;;; @ variables
;;;

(defvar quoted-printable-external-encoder '("mmencode" "-q")
  "*list of quoted-printable encoder program name and its arguments.")

(defvar quoted-printable-external-decoder '("mmencode" "-q" "-u")
  "*list of quoted-printable decoder program name and its arguments.")

(defvar quoted-printable-internal-encoding-limit 10000
  "*limit size to use internal quoted-printable encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defvar quoted-printable-internal-decoding-limit nil
  "*limit size to use internal quoted-printable decoder.
If size of input to decode is larger than this limit,
external decoder is called.")


;;; @ Quoted-Printable (Q-encode) encoder/decoder
;;;

(defun quoted-printable-quote-char (chr)
  (concat "="
	  (char-to-string (elt quoted-printable-hex-chars (ash chr -4)))
	  (char-to-string (elt quoted-printable-hex-chars (logand chr 15)))
	  ))


;;; @@ Quoted-Printable encode/decode string
;;;

(defun quoted-printable-encode-string (str)
  (let ((i 0))
    (mapconcat (function
		(lambda (chr)
		  (cond ((or (< chr 32) (< 126 chr) (eq chr ?=))
			 (if (>= i 73)
			     (progn
			       (setq i 0)
			       (concat "=\n" (quoted-printable-quote-char chr))
			       )
			   (progn
			     (setq i (+ i 3))
			     (quoted-printable-quote-char chr)
			     )))
			(t (if (>= i 75)
			       (progn
				 (setq i 0)
				 (concat "=\n" (char-to-string chr))
				 )
			     (progn
			       (setq i (1+ i))
			       (char-to-string chr)
			       )))
			)))
	       str "")))

(defun quoted-printable-decode-string (str)
  (let (q h l)
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ?=)
			 (setq q t)
			 "")
			(q (setq h
				 (cond ((<= ?a chr) (+ (- chr ?a) 10))
				       ((<= ?A chr) (+ (- chr ?A) 10))
				       ((<= ?0 chr) (- chr ?0))
				       ))
			   (setq q nil)
			   "")
			(h (setq l (cond ((<= ?a chr) (+ (- chr ?a) 10))
					 ((<= ?A chr) (+ (- chr ?A) 10))
					 ((<= ?0 chr) (- chr ?0))
					 ))
			   (prog1
			       (char-to-string (logior (ash h 4) l))
			     (setq h nil)
			     )
			   )
			(t (char-to-string chr))
			)))
	       str "")))


;;; @@ Quoted-Printable encode/decode region
;;;

(defun quoted-printable-internal-encode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (catch 'tag
	(let (b e str)
	  (while t
	    (beginning-of-line) (setq b (point))
	    (end-of-line)       (setq e (point))
	    (if (< b e)
		(progn
		  (setq str (buffer-substring b e))
		  (delete-region b e)
		  (insert (quoted-printable-encode-string str))
		  ))
	    (if (eobp)
		(throw 'tag nil)
	      )
	    (forward-char 1)
	    )))
      )))

(defun quoted-printable-internal-decode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "=\n" nil t)
	(replace-match "")
	)
      (goto-char (point-min))
      (let (b e str)
	(while (re-search-forward quoted-printable-octet-regexp nil t)
	  (setq b (match-beginning 0))
	  (setq e (match-end 0))
	  (setq str (buffer-substring b e))
	  (delete-region b e)
	  (insert (quoted-printable-decode-string str))
	  ))
      )))

(cond ((boundp 'MULE)
       (define-program-coding-system
	 nil (car quoted-printable-external-encoder) *noconv*)
       (define-program-coding-system
	 nil (car quoted-printable-external-decoder) *noconv*)
       )
      ((boundp 'NEMACS)
       (define-program-kanji-code
	 nil (car quoted-printable-external-encoder) 0)
       (define-program-kanji-code
	 nil (car quoted-printable-external-decoder) 0)
       ))

(defun quoted-printable-external-encode-region (beg end)
  (save-excursion
    (apply (function call-process-region)
	   beg end (car quoted-printable-external-encoder)
	   t t nil (cdr quoted-printable-external-encoder))
    ))

(defun quoted-printable-external-decode-region (beg end)
  (save-excursion
    (apply (function call-process-region)
	   beg end (car quoted-printable-external-decoder)
	   t t nil (cdr quoted-printable-external-decoder))
    ))

(defun quoted-printable-encode-region (beg end)
  (interactive "r")
  (if (and quoted-printable-internal-encoding-limit
	   (> (- end beg) quoted-printable-internal-encoding-limit))
      (quoted-printable-external-encode-region beg end)
    (quoted-printable-internal-encode-region beg end)
    ))

(defun quoted-printable-decode-region (beg end)
  (interactive "r")
  (if (and quoted-printable-internal-decoding-limit
	   (> (- end beg) quoted-printable-internal-decoding-limit))
      (quoted-printable-external-decode-region beg end)
    (quoted-printable-internal-decode-region beg end)
    ))


;;; @ Q-encoding encode/decode string
;;;

(defun q-encoding-encode-string-for-text (str)
  (mapconcat (function
	      (lambda (chr)
		(cond ((eq chr 32) "_")
		      ((or (< chr 32) (< 126 chr) (eq chr ?=))
		       (quoted-printable-quote-char chr)
		       )
		      (t (char-to-string chr))
		      )))
	     str ""))

(defun q-encoding-encode-string-for-comment (str)
  (mapconcat (function
	      (lambda (chr)
		(cond ((eq chr 32) "_")
		      ((or (< chr 32) (< 126 chr)
			   (memq chr '(?= ?\( ?\) ?\\))
			   )
		       (quoted-printable-quote-char chr)
		       )
		      (t (char-to-string chr))
		      )))
	     str ""))

(defun q-encoding-encode-string-for-phrase (str)
  (mapconcat (function
	      (lambda (chr)
		(cond ((eq chr 32) "_")
		      ((or (and (<= ?A chr)(<= chr ?Z))
			   (and (<= ?a chr)(<= chr ?z))
			   (and (<= ?0 chr)(<= chr ?9))
			   (memq chr '(?! ?* ?+ ?- ?/))
			   )
		       (char-to-string chr)
		       )
		      (t (quoted-printable-quote-char chr))
		      )))
	     str ""))

(defun q-encoding-encode-string (str &optional mode)
  (cond ((eq mode 'text)
	 (q-encoding-encode-string-for-text str)
	 )
	((eq mode 'comment)
	 (q-encoding-encode-string-for-comment str)
	 )
	(t
	 (q-encoding-encode-string-for-phrase str)
	 )))

(defun q-encoding-decode-string (str)
  (let (q h l)
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ?_) " ")
			((eq chr ?=)
			 (setq q t)
			 "")
			(q (setq h (cond ((<= ?a chr) (+ (- chr ?a) 10))
					 ((<= ?A chr) (+ (- chr ?A) 10))
					 ((<= ?0 chr) (- chr ?0))
					 ))
			   (setq q nil)
			   "")
			(h (setq l (cond ((<= ?a chr) (+ (- chr ?a) 10))
					 ((<= ?A chr) (+ (- chr ?A) 10))
					 ((<= ?0 chr) (- chr ?0))
					 ))
			   (prog1
			       (char-to-string (logior (ash h 4) l))
			     (setq h nil)
			     )
			   )
			(t (char-to-string chr))
			)))
	       str "")))


;;; @@ etc
;;;

(defun q-encoding-encoded-length (string &optional mode)
  (let ((l 0)(i 0)(len (length string)) chr)
    (while (< i len)
      (setq chr (elt string i))
      (if (string-match "[A-Za-z0-9!*+/=_---]" (char-to-string chr))
	  (setq l (+ l 1))
	(setq l (+ l 3))
	)
      (setq i (+ i 1)) )
    l))


;;; @ end
;;;

(provide 'mel-q)
