(require 'lex)
(require 'automata)
(require 'ew-data)
(require 'ew-parse)
(provide 'ew-scan-u)

(defmacro ew-scan-unstructured (scan col str)
  `(let ((res (ew-make-anchor col str))
	 (p 0)
	 (q (length str))
	 r
	 type)
     (while (< p q)
       (setq r p)
       (setq
	type
	(,scan
	 str p q
	 ([" \t"] 'ew:us-wsp-tok)
	 (((* [^ " \t\r"])
	   (* (+ ?\r) [^ " \t\r\n"] (* [^ " \t\r"]))
	   (* ?\r)
	   (?\r ?\n [" \t"]))
	  (when (< r (- p 3))
	    (ew-add-frag res r (- p 3) 'ew:us-texts-tok)
	    (setq r (- p 3)))
	  'ew:us-fold-tok)
	 (((* [^ " \t\r"])
	   (* (+ ?\r) [^ " \t\r\n"] (* [^ " \t\r"]))
	   (* ?\r)
	   (?\r ?\n [^ " \t"]))
	  (when (< r (- p 3))
	    (ew-add-frag res r (- p 3) 'ew:us-texts-tok)
	    (setq r (- p 3)))
	  (setq p q) 'ew:err-tok)
	 (((* [^ " \t\r"])
	   (* (+ ?\r) [^ " \t\r\n"] (* [^ " \t\r"]))
	   (* ?\r))
	  (if (< r p)
	      'ew:us-texts-tok
	    (progn (setq p q) 'ew:err-tok)))))
       (ew-add-frag res r p type))
     (ew-terminate res)
     res))

(defun ew-scan-unibyte-unstructured (col str)
  (ew-scan-unstructured lex-scan-unibyte col str))
(defun ew-scan-multibyte-unstructured (col str)
  (ew-scan-unstructured lex-scan-multibyte col str))

'(	
(npp
 (mapcar
  (lambda (frag) (cons (get frag 'type) (symbol-name frag)))
  (ew-frag-list
   (ew-scan-unibyte-unstructured
    0 " Hello! =?US-ASCII?Q?Hello!?="))))

(npp
 (mapcar
  (lambda (frag) (cons (get frag 'type) (symbol-name frag)))
  (ew-frag-list
   (ew-scan-unibyte-unstructured
    0 " \r\na"))))

)
