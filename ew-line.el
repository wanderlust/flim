(require 'lex)
(require 'ew-util)
(provide 'ew-line)

(put 'ew-line-generic 'lisp-indent-function 1)
(put 'ew-line-convert 'lisp-indent-function 1)

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

(defun ew-cut-cr-lf (str)
  (let ((i 0) (j 0) (l (length str)) result)
    (while (< j l)
      (when (member (aref str j) '(?\r ?\n))
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

(defmacro ew-line-generic-define ()
  (let ((str (make-symbol "str"))
	(others-fun (make-symbol "others-fun"))
	(fold-fun (make-symbol "fold-fun"))
	(crlf-fun (make-symbol "crlf-fun"))
	(bare-cr-fun (make-symbol "bare-cr-fun"))
	(bare-lf-fun (make-symbol "bare-lf-fun"))
	(p (make-symbol "p"))
	(q (make-symbol "q"))
	(r (make-symbol "r")))
    `(defun ew-line-generic
       (,str ,others-fun ,fold-fun ,crlf-fun ,bare-cr-fun ,bare-lf-fun)
       (let ((,p 0) (,q (length ,str)) ,r)
	 (while (< ,p ,q)
	   (setq ,r ,p)
	   (lex-scan-unibyte ,str ,p ,q
	     ((+ [^ "\r\n"]) (when ,others-fun (funcall ,others-fun ,r ,p)))
	     ((?\r ?\n [" \t"]) (when ,fold-fun (funcall ,fold-fun ,r ,p)))
	     ((?\r ?\n) (when ,crlf-fun (funcall ,crlf-fun ,r ,p)))
	     ((?\r) (when ,bare-cr-fun (funcall ,bare-cr-fun ,r ,p)))
	     ((?\n) (when ,bare-lf-fun (funcall ,bare-lf-fun ,r ,p)))
	     (() (error "something wrong"))))
	 ,q))))

(ew-line-generic-define)

(defmacro ew-line-convert-define ()
  (let ((str (make-symbol "str"))
	(others-fun (make-symbol "others-fun"))
	(fold-fun (make-symbol "fold-fun"))
	(crlf-fun (make-symbol "crlf-fun"))
	(bare-cr-fun (make-symbol "bare-cr-fun"))
	(bare-lf-fun (make-symbol "bare-lf-fun"))
	(index (make-symbol "index"))
	(result (make-symbol "result"))
	(start (make-symbol "starx"))
	(end (make-symbol "end")))
    `(defun ew-line-convert
       (,str ,others-fun ,fold-fun ,crlf-fun ,bare-cr-fun ,bare-lf-fun)
       (let ((,index 0) ,result)
	 (when (> (ew-line-generic
		      ,str
		    ,@(mapcar
		       (lambda (fun)
			 `(when ,fun
			    (lambda (,start ,end)
			      (when (< ,index ,start)
				(setq ,result
				      (ew-rcons* ,result
						 (substring ,str ,index ,start))))
			      (setq ,result
				    (ew-rcons* ,result
					       (funcall ,fun
							(substring ,str ,start ,end)))
				    ,index ,end))))
		       (list others-fun fold-fun crlf-fun bare-cr-fun bare-lf-fun)))
		  ,index)
	   (setq ,result
		 (ew-rcons* ,result
			    (substring ,str ,index))))
	 (apply 'concat (nreverse ,result))))))

(ew-line-convert-define)
