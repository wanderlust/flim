(require 'emu)
(require 'ew-var)
(require 'ew-unit)
(require 'ew-scan-s)
(require 'ew-scan-m)
(require 'ew-scan-u)
(require 'ew-scan-n)
(require 'ew-parse)
(provide 'ew-dec)

(defvar ew-decode-field-cache-buf '())
(defvar ew-decode-field-cache-num 300)

(defun ew-decode-field (field-name field-body)
  "Decode MIME RFC2047 encoded-words in a field.
FIELD-NAME is a name of the field such as \"To\", \"Subject\" etc. and
used to selecting syntax of body of the field and deciding first
column of body of the field.
FIELD-BODY is a body of the field.

If FIELD-BODY has multiple lines, each line is separated by CRLF as
pure network representation. Also if the result has multiple lines,
each line is separated by CRLF."
  (let* ((key (ew-cons* field-name field-body
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
		 field-name field-body))
	(cdar ew-decode-field-cache-buf)))))

(defun ew-analyze-field-to-decode (field-name field-body)
  "Analyze FIELD-BODY to decode."
  (let ((tmp (assq (intern (downcase field-name)) ew-decode-field-syntax-alist))
	anchor)
    (if tmp
	(setq tmp (cdr tmp))
      (setq tmp ew-decode-field-default-syntax))
    (setq anchor (funcall (car tmp) (1+ (length field-name)) field-body))
    (put anchor 'field-name field-name)
    (put anchor 'scanner (car tmp))
    (put anchor 'marker (cdr tmp))
    anchor))

(defun ew-decode-analyzed-field (anchor)
  "Decode analyzed field."
  (or (get anchor 'decoded)
      (let (tmp frag1 frag2 decode)
	(when ew-decode-sticked-encoded-word
	  (ew-separate-eword
	   (get anchor 'next-frag)
	   anchor
	   (if (eq (get anchor 'scanner) 'ew-scan-unibyte-unstructured)
	       '(ew:us-texts)
	     '(ew:cm-texts))))
	(when (get anchor 'marker)
	  (ew-mark (get anchor 'marker) anchor))
	(setq frag1 (get anchor 'next-frag))
	(while (not (eq frag1 anchor))
	  (setq decode (get frag1 'decode))
	  (setq frag2 (get frag1 'next-frag))
	  (while (and (not (eq frag2 anchor))
		      (eq decode (get frag2 'decode)))
	    (setq frag2 (get frag2 'next-frag)))
	  (funcall decode anchor frag1 frag2)
	  (setq frag1 frag2))
	(setq frag1 (get anchor 'prev-frag)
	      tmp ())
	(while (not (eq frag1 anchor))
	  (setq tmp (cons (or (get frag1 'decoded) (symbol-name frag1)) tmp)
		frag1 (get frag1 'prev-frag)))
	(put anchor 'decoded (apply 'concat tmp)))))

(defun ew-decode-field-no-cache (field-name field-body)
  "No caching version of ew-decode-field."
  (ew-decode-analyzed-field
   (ew-analyze-field-to-decode field-name field-body)))

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

(defsubst ew-decode-us-ascii (str)
  (decode-mime-charset-string str ew-default-mime-charset 'LF))

(defun ew-decode-none (anchor frag end)
  (while (not (eq frag end))
    (put frag 'decoded (ew-decode-us-ascii (symbol-name frag)))
    (setq frag (get frag 'next-frag))))

(defsubst ew-proper-eword-p (frag)
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

(defun ew-decode-generic (anchor start end
			  decode-ewords
			  decode-others
			  eword gap all)
  (let ((frag start) (start-others start) type f)
    (while (not (eq frag end))
      (setq type (get frag 'type))
      (cond
       ((and (memq type eword)
	     (ew-proper-eword-p frag))
	(when (not (eq start-others frag))
	  (funcall decode-others start-others frag))
	(let ((first frag) (ewords (list frag)))
	  (while (progn
		   (setq f (get frag 'next-frag))
		   (while (and (not (eq f end))
			       (memq (get f 'type) gap))
		     (setq f (get f 'next-frag)))
		   (and (not (eq f end))
			(ew-proper-eword-p f)))
	    (setq frag (get frag 'next-frag))
	    (while (not (eq frag f))
	      (put frag 'decoded "")
	      (setq frag (get frag 'next-frag)))
	    (setq ewords (ew-rcons* ewords f)
		  frag f))
	  (funcall decode-ewords
		   (nreverse ewords)))
	(setq start-others (get frag 'next-frag)))
       ((memq type all)
	nil)
       (t
	(error "unexpected token: %s (%s)" frag type)))
      (setq frag (get frag 'next-frag)))
    (when (not (eq start-others end))
      (funcall decode-others start-others end))))

(defun ew-decode-generic-others (start end puncts quotes targets)
  (let ((frag start) (start-nonpunct start) type buff tmp)
    (while (not (eq frag end))
      (setq type (get frag 'type))
      (cond
       ((memq type puncts)
	(when buff
	  (setq buff (apply 'concat (nreverse buff))
		tmp (ew-decode-us-ascii buff))
	  (if (equal buff tmp)
	      (while (not (eq start-nonpunct frag))
		(put start-nonpunct 'decoded (symbol-name start-nonpunct))
		(setq start-nonpunct (get start-nonpunct 'next-frag)))
	    (progn
	      (put start-nonpunct 'decoded tmp)
	      (setq start-nonpunct (get start-nonpunct 'next-frag))
	      (while (not (eq start-nonpunct frag))
		(put start-nonpunct 'decoded "")
		(setq start-nonpunct (get start-nonpunct 'next-frag)))))
	  (setq buff ()))
	(put frag 'decoded (symbol-name frag))
	(setq start-nonpunct (get frag 'next-frag)))
       ((memq type quotes)
	(setq buff (ew-rcons* buff
			      (substring (symbol-name frag) 1))))
       ((memq type targets)
	(setq buff (ew-rcons* buff
			      (symbol-name frag))))
       (t (error "something wrong: unexpected token: %s (%s)" frag type)))
      (setq frag (get frag 'next-frag)))
    (when buff
      (setq buff (apply 'concat (nreverse buff))
	    tmp (ew-decode-us-ascii buff))
      (if (equal buff tmp)
	  (while (not (eq start-nonpunct frag))
	    (put start-nonpunct 'decoded (symbol-name start-nonpunct))
	    (setq start-nonpunct (get start-nonpunct 'next-frag)))
	(progn
	  (put start-nonpunct 'decoded tmp)
	  (setq start-nonpunct (get start-nonpunct 'next-frag))
	  (while (not (eq start-nonpunct frag))
	    (put start-nonpunct 'decoded "")
	    (setq start-nonpunct (get start-nonpunct 'next-frag))))))))

(defun ew-decode-unstructured-ewords (ewords)
  (while ewords
    (put (car ewords)
	 'decoded
	 (list (ew-decode-eword (symbol-name (car ewords)))))
    (setq ewords (cdr ewords))))

(defun ew-decode-unstructured-others (start end)
  (let (strs)
    (while (not (eq start end))
      (put start 'decoded "")
      (setq strs (ew-rcons* strs
			    (symbol-name start))
	    start (get start 'next-frag)))
    (put (get end 'prev-frag)
	 'decoded
	 (ew-decode-us-ascii
	  (apply 'concat (nreverse strs))))))

(defun ew-decode-unstructured (anchor start end)
  (ew-decode-generic
   anchor start end
   'ew-decode-unstructured-ewords
   'ew-decode-unstructured-others
   '(ew:us-texts)
   '(ew:us-wsp
     ew:us-fold)
   '(ew:us-texts
     ew:us-wsp
     ew:us-fold))
  (let ((frag end) tmp)
    (while (not (eq frag start))
      (setq frag (get frag 'prev-frag)
	    tmp (cons (get frag 'decoded) tmp))
      (put frag 'decoded ""))
    (put start 'decoded (ew-encode-crlf (apply 'ew-quote-concat tmp)))))

(defun ew-decode-phrase-ewords (ewords)
  (let* ((qs (eq (get (car ewords) 'type) 'ew:qs-texts))
	 (regexp (if qs "[\\\\\\\"]" "[][()<>@,;:\\\\\\\".\000-\037]"))
	 has-dangerous-char
	 tmp decoded)
    (setq tmp ewords)
    (while tmp
      (put (car tmp)
	   'decoded
	   (list (setq decoded (ew-decode-eword (symbol-name (car tmp))))))
      (setq tmp (cdr tmp)
	    has-dangerous-char (or has-dangerous-char
				   (string-match regexp decoded))))
    (when has-dangerous-char
      (setq tmp ewords)
      (while tmp
	(setq decoded (get (car tmp) 'decoded))
	(setcar decoded (ew-embed-in-quoted-string (car decoded)))
	(setq tmp (cdr tmp)))
      (when (not qs)
	(setq decoded (get (car ewords) 'decoded))
	(setcar decoded (concat "\"" (car decoded)))
	(setq decoded (get (car (last ewords)) 'decoded))
	(setcar decoded (concat (car decoded) "\""))))))

(defun ew-decode-phrase-others (start end)
  (ew-decode-generic-others
   start end
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

(defmacro ew-rotate (var val len)
  (let ((tmp (make-symbol "tmp")))
    `(let ((,tmp (nthcdr ,(- len 2) ,var)))
       (if (cdr ,tmp)
	   (progn
	     (setcdr (cdr ,tmp) ,var)
	     (setq ,var (cdr ,tmp))
	     (setcdr ,tmp nil))
	 (setq ,var (cons nil ,var)))
       (setcar ,var ,val))))

(defun ew-decode-phrase (anchor start end)
  (ew-decode-generic
   anchor start end
   'ew-decode-phrase-ewords
   'ew-decode-phrase-others
   (if ew-decode-quoted-encoded-word
       '(ew:atom ew:qs-texts)
     '(ew:atom))
   '(ew:wsp
     ew:fold
     ew:qs-wsp
     ew:qs-fold)
   '(ew:atom
     ew:wsp
     ew:fold
     ew:qs-begin
     ew:qs-end
     ew:qs-texts
     ew:qs-wsp
     ew:qs-fold
     ew:qs-qfold
     ew:qs-qpair))
  (let ((frag start) decoded str len idx char
	chars frags
	tmp)
    (while (not (eq frag end))
      (setq decoded (get frag 'decoded)
	    str (or (car-safe decoded) decoded)
	    len (length str)
	    idx 0)
      (while (< idx len)
	(setq char (sref str idx))
	(ew-rotate chars char 3)
	(ew-rotate frags frag 3)
	(when (and (not (memq char '(?\t ?\ )))
		   (equal (cdr chars) '(?\n ?\r))
		   (eq (get (setq tmp (nth 2 frags)) 'type) 'ew:qs-qpair)
		   (eq (symbol-name tmp) (get tmp 'decoded)))
	  (put tmp 'decoded "\r"))
	(setq idx (char-next-index char idx)))
      (setq frag (get frag 'next-frag)))
    (setq frag end
	  tmp ())
    (while (not (eq frag start))
      (setq frag (get frag 'prev-frag)
	    tmp (cons (get frag 'decoded) tmp))
      (put frag 'decoded ""))
    (put start 'decoded (ew-encode-crlf (apply 'ew-quote-concat tmp)))))

(defun ew-decode-comment-ewords (ewords)
  (let* ((regexp "[()\\\\]")
	 has-dangerous-char
	 tmp decoded)
    (setq tmp ewords)
    (while tmp
      (put (car tmp)
	   'decoded
	   (list (setq decoded (ew-decode-eword (symbol-name (car tmp))))))
      (setq tmp (cdr tmp)
	    has-dangerous-char (or has-dangerous-char
				   (string-match regexp decoded))))
    (when has-dangerous-char
      (setq tmp ewords)
      (while tmp
	(setq decoded (get (car tmp) 'decoded))
	(setcar decoded (ew-embed-in-comment (car decoded)))
	(setq tmp (cdr tmp))))))

(defun ew-decode-comment-others (start end)
  (ew-decode-generic-others
   start end
   '()
   '(ew:cm-qfold
     ew:cm-qpair)
   '(ew:cm-texts
     ew:cm-wsp
     ew:cm-fold)))

(defun ew-decode-comment (anchor start end)
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
     ew:cm-qpair))
  (let ((frag start) decoded str len idx char
	chars frags tmp)
    (while (not (eq frag end))
      (setq decoded (get frag 'decoded)
	    str (or (car-safe decoded) decoded)
	    len (length str)
	    idx 0)
      (while (< idx len)
	(setq char (sref str idx))
	(ew-rotate chars char 3)
	(ew-rotate frags frag 3)
	(when (and (not (memq char '(?\t ?\ )))
		   (equal (cdr chars) '(?\n ?\r))
		   (eq (get (setq tmp (nth 2 frags)) 'type) 'ew:cm-qpair)
		   (eq (symbol-name tmp) (get tmp 'decoded)))
	  (put tmp 'decoded "\r"))
	(setq idx (char-next-index char idx)))
      (setq frag (get frag 'next-frag)))
    (setq frag end
	  tmp ())
    (while (not (eq frag start))
      (setq frag (get frag 'prev-frag)
	    tmp (cons (get frag 'decoded) tmp))
      (put frag 'decoded ""))
    (put start 'decoded (ew-encode-crlf (apply 'ew-quote-concat tmp)))))

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

(defun ew-contain-non-ascii-p (str)
  (not (eq (charsets-to-mime-charset (find-charset-string str)) 'us-ascii)))

;;;
(defun ew-decode-field-test (field-name field-body)
  (interactive
   (list
    (read-string "field-name:" (or (get-text-property (point) 'original-field-name)
				   (save-excursion
				     (end-of-line)
				     (and
				      (re-search-backward "^\\([!-9;-~]+\\):" nil t)
				      (match-string 1)))
				   ""))
    (read-string "field-body:" (or (get-text-property (point) 'original-field-body)
				   (save-excursion
				     (end-of-line)
				     (and
				      (re-search-backward "^\\([!-9;-~]+\\):" nil t)
				      (progn
					(goto-char (match-end 0))
					(looking-at ".*\\(\n[ \t].*\\)*")
					(ew-lf-crlf-to-crlf (match-string 0)))))
				   ""))))
  (with-output-to-temp-buffer "*DOODLE*"
    (save-excursion
      (set-buffer standard-output)
      (let ((ew-decode-sticked-encoded-word nil)
	    (ew-decode-quoted-encoded-word nil)
	    (ew-ignore-75bytes-limit nil)
	    (ew-ignore-76bytes-limit nil)
	    (ew-permit-sticked-comment nil)
	    (ew-permit-sticked-special nil)
	    (options
	     '(ew-ignore-76bytes-limit
	       ew-ignore-75bytes-limit
	       ew-permit-sticked-special
	       ew-permit-sticked-comment
	       ew-decode-sticked-encoded-word
	       ew-decode-quoted-encoded-word
	       ))
	    d1 d2)
	(setq d1 (ew-decode-field-no-cache field-name field-body))
	(insert field-name ":" field-body "\n"
		(make-string 76 ?-) "\n"
		field-name ":" d1 "\n")
	(while options
	  (set (car options) t)
	  (insert (format "-- %s -> t\n" (car options)))
	  (setq d2 (ew-decode-field-no-cache field-name field-body))
	  (unless (equal d1 d2)
	    (insert field-name ":" d2 "\n")
	    (setq d1 d2))
	  (setq options (cdr options)))))))

;;;

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
