(require 'lex)
(require 'ew-util)
(provide 'ew-line)

(put 'ew-crlf-line-generic 'lisp-indent-function 1)
(put 'ew-crlf-line-convert 'lisp-indent-function 1)
(put 'ew-lf-line-generic 'lisp-indent-function 1)
(put 'ew-lf-line-convert 'lisp-indent-function 1)

(defun ew-tab-to-space (str)
  (let ((i 0) (j 0) (l (length str)) result)
    (while (< j l)
      (when (equal (aref str j) ?\t)
        (setq result (ew-rcons*
                      result
                      (substring str i j)
                      " ")
              i j))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-lf-to-crlf (str)
  (let ((i 0) (j 0) (l (length str)) result)
    (while (< j l)
      (when (equal (aref str j) ?\n)
        (setq result (ew-rcons*
                      result
                      (substring str i j)
                      "\r")
              i j))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-crlf-to-lf (str)
  (let* ((i 0) (j 0) (l (length str)) (l- (1- l)) result)
    (while (< j l-)
      (when (and (equal (aref str j) ?\r)
		 (equal (aref str (1+ j)) ?\n))
        (setq result (ew-rcons*
                      result
                      (substring str i j))
	      j (1+ j)
              i j))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-lf-crlf-to-crlf (str)
  (let* ((i 0) (j 0) (l (length str)) (l- (1- l)) result)
    (while (< j l)
      (cond
       ((and (< j l-)
	     (equal (aref str j) ?\r)
	     (equal (aref str (1+ j)) ?\n))
	(setq j (1+ j)))
       ((equal (aref str j) ?\n)
        (setq result (ew-rcons*
                      result
                      (substring str i j)
		      "\r")
              i j)))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-crlf-unfold (str)
  (let* ((i 0) (j 0) (l (length str)) (l- (- l 2)) result)
    (while (< j l-)
      (when (and (equal (aref str j) ?\r)
		 (equal (aref str (1+ j)) ?\n)
		 (member (aref str (+ j 2)) '(?\t ?\ )))
        (setq result (ew-rcons*
                      result
                      (substring str i j))
	      j (+ j 2)
              i j))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-lf-unfold (str)
  (let* ((i 0) (j 0) (l (length str)) (l- (- l 1)) result)
    (while (< j l-)
      (when (and (equal (aref str j) ?\n)
		 (member (aref str (+ j 1)) '(?\t ?\ )))
        (setq result (ew-rcons*
                      result
                      (substring str i j))
	      j (+ j 1)
              i j))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-cut-generic (str chars)
  (let ((i 0) (j 0) (l (length str)) result)
    (while (< j l)
      (when (member (aref str j) chars)
        (setq result (ew-rcons*
                      result
                      (substring str i j))
              i (1+ j)))
      (setq j (1+ j)))
    (when (< i l)
      (setq result (ew-rcons*
                    result
                    (substring str i))))
    (apply 'concat (nreverse result))))

(defun ew-cut-cr-lf (str)  (ew-cut-generic str '(?\r ?\n)))
(defun ew-cut-cr (str) (ew-cut-generic str '(?\r)))
(defun ew-cut-lf (str) (ew-cut-generic str '(?\n)))

(defmacro ew-crlf-generic-define ()
  (let ((str (make-symbol "str"))
	(others-fun (make-symbol "others-fun"))
	(fold-fun (make-symbol "fold-fun"))
	(nl-fun (make-symbol "nl-fun"))
	(cr-fun (make-symbol "cr-fun"))
	(lf-fun (make-symbol "lf-fun"))
	(p (make-symbol "p"))
	(q (make-symbol "q"))
	(r (make-symbol "r")))
    `(defun ew-crlf-generic
       (,str ,others-fun ,fold-fun ,nl-fun ,cr-fun ,lf-fun)
       (let ((,p 0) (,q (length ,str)) ,r)
	 (while (< ,p ,q)
	   (setq ,r ,p)
	   (lex-scan-unibyte ,str ,p ,q
	     ((+ [^ "\r\n"]) (when ,others-fun (funcall ,others-fun ,r ,p)))
	     ((?\r ?\n [" \t"]) (when ,fold-fun (funcall ,fold-fun ,r ,p)))
	     ((?\r ?\n) (when ,nl-fun (funcall ,nl-fun ,r ,p)))
	     ((?\r) (when ,cr-fun (funcall ,cr-fun ,r ,p)))
	     ((?\n) (when ,lf-fun (funcall ,lf-fun ,r ,p)))
	     (() (error "something wrong"))))
	 ,q))))
(ew-crlf-generic-define)

(defmacro ew-crlf-line-generic-define ()
  (let ((str (make-symbol "str"))
	(others-fun (make-symbol "others-fun"))
	(fold-fun (make-symbol "fold-fun"))
	(nl-fun (make-symbol "nl-fun"))
	(p (make-symbol "p"))
	(q (make-symbol "q"))
	(r (make-symbol "r")))
    `(defun ew-crlf-line-generic
       (,str ,others-fun ,fold-fun ,nl-fun)
       (let ((,p 0) (,q (length ,str)) ,r)
	 (while (< ,p ,q)
	   (setq ,r ,p)
	   (lex-scan-unibyte ,str ,p ,q
	     (() (error "something wrong"))
	     (((* [^ "\r"])
	       (* (+ ?\r) [^ "\r\n"] (* [^ "\r"]))
	       (* ?\r)
	       (?\r ?\n [" \t"]))
	      (when (and ,others-fun (< ,r (- ,p 3))) (funcall ,others-fun ,r (- ,p 3)))
	      (when ,fold-fun (funcall ,fold-fun (- ,p 3) ,p)))
	     (((* [^ "\r"])
	       (* (+ ?\r) [^ "\r\n"] (* [^ "\r"]))
	       (* ?\r)
	       (?\r ?\n))
	      (when (and ,others-fun (< ,r (- ,p 2))) (funcall ,others-fun ,r (- ,p 2)))
	      (when ,nl-fun (funcall ,nl-fun (- ,p 2) ,p)))
	     (((* [^ "\r"])
	       (* (+ ?\r) [^ "\r\n"] (* [^ "\r"]))
	       (* ?\r))
	      (when ,others-fun (funcall ,others-fun ,r ,p)))))
	 ,q))))
(ew-crlf-line-generic-define)

(defmacro ew-lf-generic-define ()
  (let ((str (make-symbol "str"))
	(others-fun (make-symbol "others-fun"))
	(fold-fun (make-symbol "fold-fun"))
	(nl-fun (make-symbol "nl-fun"))
	(cr-fun (make-symbol "cr-fun"))
	(p (make-symbol "p"))
	(q (make-symbol "q"))
	(r (make-symbol "r")))
    `(defun ew-lf-generic
       (,str ,others-fun ,fold-fun ,nl-fun ,cr-fun)
       (let ((,p 0) (,q (length ,str)) ,r)
	 (while (< ,p ,q)
	   (setq ,r ,p)
	   (lex-scan-unibyte ,str ,p ,q
	     ((+ [^ "\r\n"]) (when ,others-fun (funcall ,others-fun ,r ,p)))
	     ((?\n [" \t"]) (when ,fold-fun (funcall ,fold-fun ,r ,p)))
	     ((?\n) (when ,nl-fun (funcall ,nl-fun ,r ,p)))
	     ((?\r) (when ,cr-fun (funcall ,cr-fun ,r ,p)))
	     (() (error "something wrong"))))
	 ,q))))
(ew-lf-generic-define)

(defmacro ew-lf-line-generic-define ()
  (let ((str (make-symbol "str"))
	(others-fun (make-symbol "others-fun"))
	(fold-fun (make-symbol "fold-fun"))
	(nl-fun (make-symbol "nl-fun"))
	(p (make-symbol "p"))
	(q (make-symbol "q"))
	(r (make-symbol "r")))
    `(defun ew-lf-line-generic
       (,str ,others-fun ,fold-fun ,nl-fun)
       (let ((,p 0) (,q (length ,str)) ,r)
	 (while (< ,p ,q)
	   (setq ,r ,p)
	   (lex-scan-unibyte ,str ,p ,q
	     (() (error "something wrong"))
	     ((+ [^ "\n"])
	      (when ,others-fun (funcall ,others-fun ,r ,p)))
	     ((?\n [" \t"])
	      (when ,fold-fun (funcall ,fold-fun ,r ,p)))
	     (?\n
	      (when ,nl-fun (funcall ,nl-fun ,r ,p)))))
	 ,q))))
(ew-lf-line-generic-define)

(defmacro ew-generic-convert-define (name generic &rest funcs)
  (let ((str (make-symbol "str"))
	(funcs-vars (mapcar (lambda (func) (make-symbol (symbol-name func))) funcs))
	(index (make-symbol "index"))
	(result (make-symbol "result"))
	(tmp (make-symbol "tmp"))
	(start (make-symbol "starx"))
	(end (make-symbol "end")))
    `(defun ,name
       (,str ,@funcs-vars)
       (let ((,index 0) ,result ,tmp)
	 (when (> (,generic
		   ,str
		   ,@(mapcar
		      (lambda (fun)
			`(when ,fun
			   (lambda (,start ,end)
			     (setq ,tmp (funcall ,fun (substring ,str ,start ,end)))
			     (when ,tmp
			       (when (< ,index ,start)
				 (setq ,result
				       (ew-rcons* ,result
						  (substring ,str ,index ,start))))
			       (setq ,result (ew-rcons* ,result ,tmp)
				     ,index ,end)))))
		      funcs-vars))
		  ,index)
	   (setq ,result
		 (ew-rcons* ,result
			    (substring ,str ,index))))
	 (apply 'concat (nreverse ,result))))))
(ew-generic-convert-define ew-crlf-convert ew-crlf-generic others-fun fold-fun nl-fun cr-fun lf-fun)
(ew-generic-convert-define ew-crlf-line-convert ew-crlf-line-generic others-fun fold-fun nl-fun)
(ew-generic-convert-define ew-lf-convert ew-lf-generic others-fun fold-fun nl-fun cr-fun)
(ew-generic-convert-define ew-lf-line-convert ew-lf-line-generic others-fun fold-fun nl-fun)

(defmacro ew-fold-define (name convert nl)
  `(defun ,name (str start-column line-length)
     (let ((column start-column))
       (,convert str
         (lambda (line)
	   (let ((start 0)
		 (end (length line))
		 result tmp fold width)
	     (while (and (< start end)
			 (progn
			   (when (<= column 1)
			     (setq tmp (sref line start)
				   result (ew-rcons* result (char-to-string tmp))
				   column (+ column (char-width tmp))
				   start (char-next-index tmp start)))
			   (string-match "[ \t]" line start)))
	       (setq tmp (substring line start (match-beginning 0))
		     width (string-width tmp)
		     result (ew-rcons* result tmp)
		     column (+ column width)
		     start (match-beginning 0))
	       (if (<= line-length column)
		   (progn
		     (when (and fold (not (= line-length column)))
		       (setcdr fold (cons (car fold) (cdr fold)))
		       (setcar fold ,nl)
		       (setq column (+ width
				       (if (eq (cdr result) fold)
					   0
					 (string-width (cadr result))))))
		     (if (<= line-length column)
			 (setq result (ew-rcons* result ,nl)
			       column 0
			       fold nil)
		       (setq fold result)))
		 (setq fold result))
	       (setq tmp (sref line (match-beginning 0))
		     result (ew-rcons* result (char-to-string tmp))
		     column (+ column (char-width tmp))
		     start (match-end 0)))
	     (when (< start end)
	       (setq tmp (substring line start)
		     result (ew-rcons* result tmp)
		     column (+ column (string-width tmp))))
	     (when (and (< line-length column) fold)
	       (setcdr fold (cons (car fold) (cdr fold)))
	       (setcar fold ,nl))
	     (apply 'concat (nreverse result))))
	 (lambda (fold) (setq column 1) nil)
	 (lambda (nl) (setq column 0) nil)))))

(ew-fold-define ew-crlf-fold ew-crlf-line-convert "\r\n")
(ew-fold-define ew-lf-fold ew-crlf-line-convert "\n")

(defun ew-crlf-refold (string start-column line-length)
  (ew-crlf-fold (ew-crlf-unfold string) start-column line-length))

(defun ew-lf-refold (string start-column line-length)
  (ew-lf-fold (ew-lf-unfold string) start-column line-length))
