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
	   ([" \t"] 'ew:wsp-tok)
	   (?< 'ew:lt-tok)
	   (?> 'ew:gt-tok)
	   (?@ 'ew:at-tok)
	   (?, 'ew:comma-tok)
	   (?\; 'ew:semicolon-tok)
	   (?: 'ew:colon-tok)
	   (?. 'ew:dot-tok)
	   ((?\r ?\n [" \t"]) 'ew:fold-tok)
	   ((?\r ?\n [^ " \t"])
	    (setq p q) 'ew:err-tok)
	   ((+ [(?a ?z) (?A ?Z) (?0 ?9) "!#$%&'*+-/=?^_`{|}~" non-ascii])
	    'ew:atom-tok)
	   (?\" (setq mode 'quoted-string) 'ew:qs-begin-tok)
	   (?\[ (setq mode 'domain-literal) 'ew:dl-begin-tok)
	   (?\( (setq mode 'comment
		      nest 1)
		'ew:cm-begin-tok)
	   (() (setq p q) 'ew:err-tok)))
	 (ew-add-frag res r p type))
	((eq mode 'quoted-string)
	 (setq
	  type
	  (,scan
	   str p q
	   (?\" (setq mode 'token) 'ew:qs-end-tok)
	   ((?\\ ?\r ?\n [" \t"]) 'ew:qs-qfold-tok)
	   ((?\\ ?\r ?\n [^ " \t"])
	    (setq p q) 'ew:err-tok)
	   (((* [^ "\"\\ \t\r"])
	     (* (+ ?\r) [^ "\"\\ \t\r\n"] (* [^ "\"\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [" \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:qs-texts-tok)
	      (setq r (- p 3)))
	    'ew:qs-fold-tok)
	   (((* [^ "\"\\ \t\r"])
	     (* (+ ?\r) [^ "\"\\ \t\r\n"] (* [^ "\"\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [^ " \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:qs-texts-tok)
	      (setq r (- p 3)))
	    (setq p q) 'ew:err-tok)
	   ((?\\ (any))
	    'ew:qs-qpair-tok)
	   ([" \t"]
	    'ew:qs-wsp-tok)
	   (((* [^ "\"\\ \t\r"])
	     (* (+ ?\r) [^ "\"\\ \t\r\n"] (* [^ "\"\\ \t\r"]))
	     (* ?\r))
	    (if (< r p)
		'ew:qs-texts-tok
	      (progn (setq p q) 'ew:err-tok)))))
	 (ew-add-frag res r p type))
	((eq mode 'domain-literal)
	 (setq
	  type
	  (,scan
	   str p q
	   (?\] (setq mode 'token) 'ew:dl-end-tok)
	   ((?\\ ?\r ?\n [" \t"])
	    'ew:dl-qfold-tok)
	   ((?\\ ?\r ?\n [^ " \t"])
	    (setq p q) 'ew:err-tok)
	   (((* [^ "[]\\ \t\r"])
	     (* (+ ?\r) [^ "[]\\ \t\r\n"] (* [^ "[]\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [" \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:dl-texts-tok)
	      (setq r (- p 3)))
	    'ew:dl-fold-tok)
	   (((* [^ "[]\\ \t\r"])
	     (* (+ ?\r) [^ "[]\\ \t\r\n"] (* [^ "[]\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [^ " \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:dl-texts-tok)
	      (setq r (- p 3)))
	    (setq p q) 'ew:err-tok)
	   ((?\\ (any))
	    'ew:dl-qpair-tok)
	   ([" \t"]
	    'ew:dl-wsp-tok)
	   (((* [^ "[]\\ \t\r"])
	     (* (+ ?\r) [^ "[]\\ \t\r\n"] (* [^ "[]\\ \t\r"]))
	     (* ?\r))
	    (if (< r p)
		'ew:dl-texts-tok
	      (progn (setq p q) 'ew:err-tok)))))
	 (ew-add-frag res r p type))
	((eq mode 'comment)
	 (setq
	  type
	  (,scan
	   str p q
	   (?\( (setq nest (1+ nest)) 'ew:cm-nested-begin-tok)
	   (?\) (setq nest (1- nest))
		(if (zerop nest)
		    (progn (setq mode 'token) 'ew:cm-end-tok)
		  'ew:cm-nested-end-tok))
	   ((?\\ ?\r ?\n [" \t"])
	    'ew:cm-qfold-tok)
	   ((?\\ ?\r ?\n [^ " \t"])
	    (setq p q) 'ew:err-tok)
	   (((* [^ "()\\ \t\r"])
	     (* (+ ?\r) [^ "()\\ \t\r\n"] (* [^ "()\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [" \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:cm-texts-tok)
	      (setq r (- p 3)))
	    'ew:cm-fold-tok)
	   (((* [^ "()\\ \t\r"])
	     (* (+ ?\r) [^ "()\\ \t\r\n"] (* [^ "()\\ \t\r"]))
	     (* ?\r)
	     (?\r ?\n [^ " \t"]))
	    (when (< r (- p 3))
	      (ew-add-frag res r (- p 3) 'ew:cm-texts-tok)
	      (setq r (- p 3)))
	    (setq p q) 'ew:err-tok)
	   ((?\\ (any))
	    'ew:cm-qpair-tok)
	   ([" \t"]
	    'ew:cm-wsp-tok)
	   (((* [^ "()\\ \t\r"])
	     (* (+ ?\r) [^ "()\\ \t\r\n"] (* [^ "()\\ \t\r"]))
	     (* ?\r))
	    (if (< r p)
		'ew:cm-texts-tok
	      (progn (setq p q) 'ew:err-tok)))))
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
