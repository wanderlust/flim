(require 'emu)
(require 'ew-var)
(require 'ew-unit)
(require 'ew-scan-s)
(require 'ew-scan-m)
(require 'ew-scan-u)
(require 'ew-parse)
(provide 'ew-dec)

(defvar ew-decode-field-cache-buf '())
(defvar ew-decode-field-cache-num 300)

(defun ew-decode-field (field-name field-body &optional eword-filter)
  "Decode MIME RFC2047 encoded-words in a field.
FIELD-NAME is a name of the field such as \"To\", \"Subject\" etc. and
used to selecting syntax of body of the field and deciding first
column of body of the field.
FIELD-BODY is a body of the field.

If FIELD-BODY has multiple lines, each line is separated by CRLF as
pure network representation. Also if the result has multiple lines,
each line is separated by CRLF.

If EWORD-FILTER is non-nil, it should be closure. it is called for
each successful decoded encoded-word with decoded string as a
argument. The return value of EWORD-FILTER is used as decoding result
instead of its argument."
  (let* ((key (ew-cons* field-name field-body eword-filter
			(ew-dynamic-options)))
	 (tmp (assoc key ew-decode-field-cache-buf)))
    (if tmp
	(cdr tmp)
      (progn
	(setq tmp (nthcdr ew-decode-field-cache-num
			  ew-decode-field-cache-buf))
	(if (cdr tmp)
	    (progn
	      (setcdr (cdr tmp) ew-decode-field-cache-buf)
	      (setq ew-decode-field-cache-buf (cdr tmp))
	      (setcdr tmp nil))
	  (setq ew-decode-field-cache-buf
		(cons (cons nil nil)
		      ew-decode-field-cache-buf)))
	(setcar (car ew-decode-field-cache-buf) key)
	(setcdr (car ew-decode-field-cache-buf)
		(ew-decode-field-no-cache
		 field-name field-body eword-filter))
	(cdar ew-decode-field-cache-buf)))))

(defun ew-decode-field-no-cache (field-name field-body &optional eword-filter)
  "No caching version of ew-decode-field."
  (let ((tmp (assoc (downcase field-name) ew-decode-field-syntax-alist))
	frag-anchor frag1 frag2 decode)
    (if tmp
	(setq tmp (cdr tmp))
      (setq tmp ew-decode-field-default-syntax))
    (setq frag-anchor (funcall (car tmp) (1+ (length field-name)) field-body))
    ;;(setq zzz frag-anchor)
    (when (and (eq (car tmp) 'ew-scan-unibyte-unstructured)
	       ew-decode-sticked-encoded-word)
      (ew-separate-eword (get frag-anchor 'next-frag)
			 frag-anchor
			 '(ew:us-texts)))
    (when (cdr tmp)
      (ew-mark (cdr tmp) frag-anchor))
    (setq frag1 (get frag-anchor 'next-frag))
    (while (not (eq frag1 frag-anchor))
      (setq decode (get frag1 'decode))
      (setq frag2 (get frag1 'next-frag))
      (while (and (not (eq frag2 frag-anchor))
		  (eq decode (get frag2 'decode)))
	(setq frag2 (get frag2 'next-frag)))
      (funcall decode frag-anchor frag1 frag2 eword-filter)
      (setq frag1 frag2))
    (setq frag1 (get frag-anchor 'prev-frag)
	  tmp ())
    (while (not (eq frag1 frag-anchor))
      (setq tmp (cons (or (get frag1 'result) (symbol-name frag1)) tmp)
	    frag1 (get frag1 'prev-frag)))
    (apply 'concat tmp)))

(defun ew-mark (tag anchor)
  (let ((tlist (cons (list (symbol-value tag)) (ew-pair-list anchor))))
    ;;(insert (format "%s" tlist))
    (ew-parse
     (lambda ()
       (if (null tlist)
           (cons 0 anchor)
         (prog1 (car tlist) (setq tlist (cdr tlist)))))
     (lambda (msg tok)
       (message "%s%s : %s" msg tok anchor)
       (when (< 0 ew-parse-error-sit-for-seconds)
	 (sit-for ew-parse-error-sit-for-seconds))))))

(defun ew-decode-none (anchor frag end eword-filter)
  (while (not (eq frag end))
    (put frag 'result (funcall ew-decode-us-ascii (symbol-name frag)))
    (setq frag (get frag 'next-frag))))

(defun ew-decode-generic (anchor start end
			  decode-ewords
			  decode-others
			  eword gap all
			  eword-filter)
  (let ((frag start) result buff type f)
    (while (not (eq frag end))
      (setq type (get frag 'type))
      (cond
       ((and (memq type eword)
	     (ew-proper-eword-p frag))
	(when buff
	  (setq result (ew-rappend result
				   (funcall decode-others
					    (nreverse buff)))
		buff ()))
	(let ((first frag) (ewords (list frag)))
	  (while (progn
		   (setq f (get frag 'next-frag))
		   (while (and (not (eq f end))
			       (memq (get f 'type) gap))
		     (setq f (get f 'next-frag)))
		   (and (not (eq f end))
			(ew-proper-eword-p f)))
	    (setq ewords (ew-rcons* ewords f)
		  frag f))
	  (while (not (eq first frag))
	    (put first 'result "")
	    (setq first (get first 'next-frag)))
	  (put frag 'result "")
	  (setq result (ew-rappend result
				   (funcall decode-ewords
					    (nreverse ewords)
					    eword-filter)))))
       ((memq type all)
	(setq buff (cons frag buff))
	(put frag 'result ""))
       (t
	(error "unexpected token: %s (%s)" frag type)))
      (setq frag (get frag 'next-frag)))
    (when buff
      (setq result (ew-rappend result (funcall decode-others (nreverse buff)))))
    (put start 'result
	 (apply 'ew-quote-concat (nreverse result)))
    ))

(defun ew-decode-generic-others (frags puncts quotes targets)
  (let (result buff frag type tmp)
    (while frags
      (setq frag (car frags)
	    type (get frag 'type)
	    frags (cdr frags))
      (cond
       ((memq type puncts)
	(when buff
	  (setq buff (nreverse buff)
		tmp (funcall ew-decode-us-ascii
			     (mapconcat 'car buff "")))
	  (if (ew-contain-non-ascii-p tmp)
	      (setq result (ew-rcons* result tmp))
	    (setq result (ew-rcons*
			  result
			  (funcall ew-decode-us-ascii
				   (mapconcat 'cdr buff "")))))
	  (setq buff ()))
	(setq result (ew-rcons*
		      result
		      (symbol-name frag))))
       ((memq type quotes)
	(setq buff (ew-rcons*
		    buff
		    (cons (substring (symbol-name frag) 1)
			  (symbol-name frag)))))
       ((memq type targets)
	(setq buff (ew-rcons*
		    buff
		    (cons (symbol-name frag)
			  (symbol-name frag)))))
       (t
	(error "something wrong: unexpected token: %s (%s)" frag type))))
    (when buff
      (setq buff (nreverse buff)
	    tmp (funcall ew-decode-us-ascii
			 (mapconcat 'car buff "")))
      (if (ew-contain-non-ascii-p tmp)
	  (setq result (ew-rcons* result tmp))
	(setq result (ew-rcons*
		      result
		      (funcall ew-decode-us-ascii
			       (mapconcat 'cdr buff "")))))
      (setq buff ()))
    (nreverse result)))

(defun ew-decode-unstructured-ewords (ewords eword-filter)
  (let (result)
    (while ewords
      (setq result (ew-rcons*
		    result
		    (list (ew-decode-eword (symbol-name (car ewords))
					   eword-filter
					   'ew-encode-crlf)))
	    ewords (cdr ewords)))
    (nreverse result)))

(defun ew-decode-unstructured-others (frags)
  (let (result)
    (while frags
      (setq result (ew-rcons*
		    result
		    (symbol-name (car frags)))
	    frags (cdr frags)))
    (list (funcall ew-decode-us-ascii
		   (apply 'concat (nreverse result))))))

(defun ew-decode-unstructured (anchor start end eword-filter)
  (ew-decode-generic
   anchor start end
   'ew-decode-unstructured-ewords
   'ew-decode-unstructured-others
   '(ew:us-texts)
   '(ew:us-wsp
     ew:us-fold)
   '(ew:us-texts
     ew:us-wsp
     ew:us-fold)
   eword-filter))

(defun ew-decode-phrase-ewords (ewords eword-filter)
  (let ((qs (eq (get (car ewords) 'type) 'ew:qs-texts))
	require-quoting
	result)
    (while ewords
      (setq result (ew-rcons*
		    result
		    (list (ew-decode-eword (symbol-name (car ewords))
					   eword-filter
					   'ew-encode-crlf)))
	    require-quoting (or require-quoting
				(string-match "[][()<>@,;:\\\".\000-\037]"
                                              (caar result)))
	    ewords (cdr ewords)))
    (if require-quoting
	(list
	 (funcall (if qs 'ew-embed-in-quoted-string 'ew-embed-in-phrase)
		  (apply 'ew-quote-concat
			 (nreverse result))))
      (nreverse result))))

(defun ew-decode-phrase-others (frags)
  (ew-decode-generic-others
   frags
   '(ew:qs-begin
     ew:qs-end)
   '(ew:qs-qfold
     ew:qs-qpair)
   '(ew:atom
     ew:wsp
     ew:fold
     ew:qs-texts
     ew:qs-wsp
     ew:qs-fold)))

(defun ew-decode-phrase (anchor start end eword-filter)
  (ew-decode-generic
   anchor start end
   'ew-decode-phrase-ewords
   'ew-decode-phrase-others
   (if ew-decode-quoted-encoded-word
       '(ew:atom ew:qs-texts)
     '(ew:atom))
   '(ew:wsp
     ew:fold)
   '(ew:atom
     ew:wsp
     ew:fold
     ew:qs-begin
     ew:qs-end
     ew:qs-texts
     ew:qs-wsp
     ew:qs-fold
     ew:qs-qfold
     ew:qs-qpair)
   eword-filter))

(defun ew-decode-comment-ewords (ewords eword-filter)
  (let (require-quoting
	result)
    (while ewords
      (setq result (ew-rcons*
		    result
		    (list (ew-decode-eword (symbol-name (car ewords))
					   eword-filter
					   'ew-encode-crlf)))
	    require-quoting (or require-quoting
				(string-match "[()\\\\]" (caar result)))
	    ewords (cdr ewords)))
    (if require-quoting
	(list
	 (ew-embed-in-comment
	  (apply 'ew-quote-concat
		 (nreverse result))))
      (nreverse result))))

(defun ew-decode-comment-others (frags)
  (ew-decode-generic-others
   frags
   '()
   '(ew:cm-qfold
     ew:cm-qpair)
   '(ew:cm-texts
     ew:cm-wsp
     ew:cm-fold)))

(defun ew-decode-comment (anchor start end eword-filter)
  (ew-decode-generic
   anchor start end
   'ew-decode-comment-ewords
   'ew-decode-comment-others
   '(ew:cm-texts)
   '(ew:cm-wsp
     ew:cm-fold)
   '(ew:cm-texts
     ew:cm-wsp
     ew:cm-fold
     ew:cm-qfold
     ew:cm-qpair)
   eword-filter))

;;;

(defun ew-embed-in-phrase (str)
  (concat "\"" (ew-embed-in-quoted-string str) "\""))

(defun ew-embed-in-quoted-string (str)
  (ew-quote-as-quoted-pair str '(?\\ ?\")))

(defun ew-embed-in-comment (str)
  (ew-quote-as-quoted-pair str '(?\\ ?\( ?\))))

(defun ew-quote-as-quoted-pair (str specials)
  (let ((i 0) (j 0) (l (length str)) result)
    (while (< j l)
      (when (member (aref str j) specials)
	(setq result (ew-rcons*
		      result
		      (substring str i j)
		      "\\")
	      i j))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
		    result
		    (substring str i))))
    (apply 'concat (nreverse result))))

;;;

(defun ew-proper-eword-p (frag)
  (and
   (or ew-ignore-75bytes-limit
       (<= (length (symbol-name frag)) 75))
   (or ew-ignore-76bytes-limit
       (<= (get frag 'line-length) 76))
   (cond
    ((eq (get frag 'type) 'ew:cm-texts)
     (ew-eword-p (symbol-name frag)))
    ((eq (get frag 'type) 'ew:qs-texts)
     (ew-eword-p (symbol-name frag)))
    ((eq (get frag 'type) 'ew:atom)
     (and
      (or ew-permit-sticked-comment
	  (and
	   (not (ew-comment-frag-p (get frag 'prev-frag)))
	   (not (ew-comment-frag-p (get frag 'next-frag)))))
      (or ew-permit-sticked-special
	  (and
	   (or (ew-comment-frag-p (get frag 'prev-frag))
	       (not (ew-special-frag-p (get frag 'prev-frag))))
	   (or (ew-comment-frag-p (get frag 'next-frag))
	       (not (ew-special-frag-p (get frag 'next-frag))))))
      (ew-eword-p (symbol-name frag))))
    ((eq (get frag 'type) 'ew:us-texts)
     (and
      (or ew-permit-sticked-special
	  (not (ew-special-frag-p (get frag 'prev-frag))))
      (ew-eword-p (symbol-name frag))))
    (t
     nil))))

(defun ew-contain-non-ascii-p (str)
  (not (eq (charsets-to-mime-charset (find-charset-string str)) 'us-ascii)))

'(

(ew-decode-field "To" " =?US-ASCII?Q?phrase?= <akr@jaist.ac.jp>")
(ew-decode-field "To" " =?US-ASCII?Q?phrase?= < =?US-ASCII?Q?akr?= @jaist.ac.jp>")
(ew-decode-field "To" " =?US-ASCII?Q?akr?= @jaist.ac.jp")
(ew-decode-field "Subject" " =?ISO-2022-JP?B?GyRCJCIbKEI=?=")
(ew-decode-field "Content-Type" " text/vnd.latex-z(=?US-ASCII?Q?What=3F?=);charset=ISO-2022-JP")

(ew-decode-field "To" " =?US-ASCII?Q?A=22B=5CC?= <akr@jaist.ac.jp>")
(let ((ew-decode-quoted-encoded-word t))
  (ew-decode-field "To" " \"=?US-ASCII?Q?A=22B=5CC?=\" <akr@jaist.ac.jp>"))

(ew-decode-field "To" " akr@jaist.ac.jp (=?US-ASCII?Q?=28A=29B=5C?=)")

(ew-decode-field "To" "\"A\\BC\e$B\\\"\\\\\e(B\" <foo@bar>")
(ew-decode-field "To" "\"A\\BC\" <foo@bar>")
(ew-decode-field "To" "\"\e\\$\\B\\$\\\"\e\\(\\B\" <foo@bar>")

)
