(require 'lex)
(require 'automata)
(require 'ew-data)
(require 'ew-parse)
(provide 'ew-scan-s)

(defmacro ew-scan-std11 (scan col str)
  `(let ((res (ew-make-anchor col str))
	 (mode 'token)
	 (p 0)
	 (q (length str))
	 r
	 type
	 nest)
     (while (< p q)
       (setq r p)
       (cond
	((eq mode 'token)
	 (setq
	  type
	  (,scan
	   str p q
	   ([" \t"] 'ew:wsp)
	   (?< 'ew:lt)
	   (?> 'ew:gt)
	   (?@ 'ew:at)
	   (?, 'ew:comma)
	   (?\; 'ew:semicolon)
	   (?: 'ew:colon)
	   (?. 'ew:dot)
	   ((?\r ?\n [" \t"]) 'ew:fold)
	   ((?\r ?\n [^ " \t"])
	    (setq p q) 'ew:*err*)
	   ((+ [(?a ?z) (?A ?Z) (?0 ?9) "!#$%&'*+-/=?^_`{|}~" non-ascii])
	    'ew:atom)
	   (?\" (setq mode 'quoted-string) 'ew:qs-begin)
	   (?\[ (setq mode 'domain-literal) 'ew:dl-begin)
	   (?\( (setq mode 'comment
		      nest 1)
		'ew:cm-begin)
	   (() (setq p q) 'ew:*err*)))
	 (ew-add-frag res r p type))
	((eq mode 'quoted-string)
	 (setq
	  type
	  (,scan
	   str p q
	   (?\" (setq mode 'token) 'ew:qs-end)
	   ((?\\ ?\r ?\n [" \t"]) 'ew:qs-qfold)
	   ((?\\ ?\r ?\n [^ " \t"])
	    (setq p q) 'ew:*err*)
	   (((* [^ "\"\\ \t\r"])
	     (* (+ ?\r) [^ "\"\\ \t\r\n"] (* [^ "\"\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [" \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:qs-texts)
	      (setq r (- p 3)))
	    'ew:qs-fold)
	   (((* [^ "\"\\ \t\r"])
	     (* (+ ?\r) [^ "\"\\ \t\r\n"] (* [^ "\"\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [^ " \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:qs-texts)
	      (setq r (- p 3)))
	    (setq p q) 'ew:*err*)
	   ((?\\ (any))
	    'ew:qs-qpair)
	   ([" \t"]
	    'ew:qs-wsp)
	   (((* [^ "\"\\ \t\r"])
	     (* (+ ?\r) [^ "\"\\ \t\r\n"] (* [^ "\"\\ \t\r"]))
	     (* ?\r))
	    (if (< r p)
		'ew:qs-texts
	      (progn (setq p q) 'ew:*err*)))))
	 (ew-add-frag res r p type))
	((eq mode 'domain-literal)
	 (setq
	  type
	  (,scan
	   str p q
	   (?\] (setq mode 'token) 'ew:dl-end)
	   ((?\\ ?\r ?\n [" \t"])
	    'ew:dl-qfold)
	   ((?\\ ?\r ?\n [^ " \t"])
	    (setq p q) 'ew:*err*)
	   (((* [^ "[]\\ \t\r"])
	     (* (+ ?\r) [^ "[]\\ \t\r\n"] (* [^ "[]\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [" \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:dl-texts)
	      (setq r (- p 3)))
	    'ew:dl-fold)
	   (((* [^ "[]\\ \t\r"])
	     (* (+ ?\r) [^ "[]\\ \t\r\n"] (* [^ "[]\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [^ " \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:dl-texts)
	      (setq r (- p 3)))
	    (setq p q) 'ew:*err*)
	   ((?\\ (any))
	    'ew:dl-qpair)
	   ([" \t"]
	    'ew:dl-wsp)
	   (((* [^ "[]\\ \t\r"])
	     (* (+ ?\r) [^ "[]\\ \t\r\n"] (* [^ "[]\\ \t\r"]))
	     (* ?\r))
	    (if (< r p)
		'ew:dl-texts
	      (progn (setq p q) 'ew:*err*)))))
	 (ew-add-frag res r p type))
	((eq mode 'comment)
	 (setq
	  type
	  (,scan
	   str p q
	   (?\( (setq nest (1+ nest)) 'ew:cm-nested-begin)
	   (?\) (setq nest (1- nest))
		(if (zerop nest)
		    (progn (setq mode 'token) 'ew:cm-end)
		  'ew:cm-nested-end))
	   ((?\\ ?\r ?\n [" \t"])
	    'ew:cm-qfold)
	   ((?\\ ?\r ?\n [^ " \t"])
	    (setq p q) 'ew:*err*)
	   (((* [^ "()\\ \t\r"])
	     (* (+ ?\r) [^ "()\\ \t\r\n"] (* [^ "()\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [" \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:cm-texts)
	      (setq r (- p 3)))
	    'ew:cm-fold)
	   (((* [^ "()\\ \t\r"])
	     (* (+ ?\r) [^ "()\\ \t\r\n"] (* [^ "()\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [^ " \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:cm-texts)
	      (setq r (- p 3)))
	    (setq p q) 'ew:*err*)
	   ((?\\ (any))
	    'ew:cm-qpair)
	   ([" \t"]
	    'ew:cm-wsp)
	   (((* [^ "()\\ \t\r"])
	     (* (+ ?\r) [^ "()\\ \t\r\n"] (* [^ "()\\ \t\r"]))
	     (* ?\r))
	    (if (< r p)
		'ew:cm-texts
	      (progn (setq p q) 'ew:*err*)))))
	 (ew-add-frag res r p type))))
     (ew-terminate res)
     res))

(defun ew-scan-unibyte-std11 (col str)
  (ew-scan-std11 lex-scan-unibyte col str))
(defun ew-scan-multibyte-std11 (col str)
  (ew-scan-std11 lex-scan-multibyte col str))

'(	
(npp
 (mapcar
  'symbol-plist
  (ew-frag-list
   (ew-scan-unibyte-std11
    0 " Tanaka Akira <akr@jaist.ac.jp> (Tanaka Akira)"))))

(npp
 (mapcar
  (lambda (frag) (cons (get frag 'type) (symbol-name frag)))
  (ew-frag-list
   (ew-scan-unibyte-std11
    0 " Tanaka Akira <akr@jaist.ac.jp> (Tanaka Akira)"))))
)
