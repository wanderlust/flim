(require 'ew-var)
(require 'ew-util)
(provide 'ew-data)

(defun ew-make-anchor (column str)
  (let ((anchor (make-symbol str)))
    (put anchor 'anchor anchor)
    (put anchor 'type 'ew:*anchor*)
    (put anchor 'prev-frag anchor)
    (put anchor 'next-frag anchor)
    (put anchor 'column column)
    (put anchor 'line-length 0)
    anchor))

(defun ew-terminate (anchor)
  (let ((frag (get anchor 'prev-frag))
	(line-length (get anchor 'column)))
    (while (null (get frag 'line-length))
      (put frag 'line-length line-length)
      (setq frag (get frag 'prev-frag)))))

(defsubst ew-add-frag (anchor start end type)
  (let ((frag (make-symbol (substring (symbol-name anchor) start end))))
    (put frag 'anchor anchor)
    (put frag 'type type)
    (put frag 'prev-frag (get anchor 'prev-frag))
    (put frag 'next-frag anchor)
    (put (get anchor 'prev-frag) 'next-frag frag)
    (put anchor 'prev-frag frag)
    (put frag 'decode (or (get type 'decode) 'ew-decode-none))
    (if (string-match "\r\n\\(.*\r\n\\)*" (symbol-name frag))
	(let ((prev-line-length (+ (get anchor 'column) (match-beginning 0)))
	      (next-line-column (- (length (symbol-name frag)) (match-end 0)))
	      (tmp frag))
	  (while (null (get tmp 'line-length))
	    (put tmp 'line-length prev-line-length)
	    (setq tmp (get tmp 'prev-frag)))
	  (put anchor 'column next-line-column))
      (put anchor 'column (+ (get anchor 'column) (length (symbol-name frag)))))
    frag))

;;; listup

(defun ew-frag-list (anchor)
  (let ((res ())
	(tmp (get anchor 'prev-frag)))
    (while (not (eq anchor tmp))
      (setq res (cons tmp res)
	    tmp (get tmp 'prev-frag)))
    res))

(defun ew-pair-list (anchor)
  (mapcar
   (lambda (frag)
     (cons (symbol-value (get frag 'type))
           frag))
   (ew-frag-list anchor)))

(defun ew-separate-eword (frag1 frag2 targets)
  (while (not (eq frag1 frag2))
    (when (and (memq (get frag1 'type) targets)
	       (string-match ew-encoded-word-regexp
			     (symbol-name frag1))
	       (or (< 0 (match-beginning 0))
		   (< (match-end 0) (length (symbol-name frag1)))))
      (let ((atom (symbol-name frag1))
	    (start (match-end 0))
	    result
	    frag)
	(when (< 0 (match-beginning 0))
	  (setq frag (make-symbol (substring atom 0 (match-beginning 0)))
		result (ew-rcons* result frag)))
	(setq frag (make-symbol (substring atom (match-beginning 0) (match-end 0)))
	      result (ew-rcons* result frag))
	(when (cdr result)
	  (put frag 'prev-frag (cadr result))
	  (put (cadr result) 'next-frag frag)
	  (setq frag (cadr result)))
	(put frag 'prev-frag (get frag1 'prev-frag))
	(put (get frag1 'prev-frag) 'next-frag frag)
	(while (string-match ew-encoded-word-regexp atom start)
	  (when (< start (match-beginning 0))
	    (setq frag (make-symbol (substring atom start (match-beginning 0)))
		  result (ew-rcons* result frag))
	    (put frag 'prev-frag (cadr result))
	    (put (cadr result) 'next-frag frag))
	  (setq frag (make-symbol (substring atom (match-beginning 0) (match-end 0)))
		result (ew-rcons* result frag)
		start (match-end 0))
	  (put frag 'prev-frag (cadr result))
	  (put (cadr result) 'next-frag frag))
	(when (< start (length (symbol-name frag1)))
	  (setq frag (make-symbol (substring atom start))
		result (ew-rcons* result frag))
	  (put frag 'prev-frag (cadr result))
	  (put (cadr result) 'next-frag frag))
	(setq frag (car result))
	(put frag 'next-frag (get frag1 'next-frag))
	(put (get frag1 'next-frag) 'prev-frag frag)
	(while result
	  (setq frag (car result)
		result (cdr result))
	  (put frag 'anchor (get frag1 'anchor))
	  (put frag 'type (get frag1 'type))
	  (put frag 'decode (get frag1 'decode))
	  (put frag 'line-length (get frag1 'line-length)))))
    (setq frag1 (get frag1 'next-frag))))

;;; phrase marking

(defun ew-mark-phrase (frag1 frag2)
  (when ew-decode-sticked-encoded-word
    (ew-separate-eword
     frag1 frag2
     (if ew-decode-quoted-encoded-word
	 '(ew:atom
	   ew:qs-texts)
       '(ew:atom)))
    (setq frag1 (get (get frag1 'prev-frag) 'next-frag)))
  (while (not (eq frag1 frag2))
    (unless (ew-comment-frag-p frag2)
      (put frag2 'decode 'ew-decode-phrase))
    (setq frag2 (get frag2 'prev-frag)))
  (unless (ew-comment-frag-p frag2)
    (put frag2 'decode 'ew-decode-phrase))
  (setq frag2 (get frag2 'prev-frag))
  (while (not (ew-token-last-frag-p frag2))
    (unless (ew-comment-frag-p frag2)
      (put frag2 'decode 'ew-decode-phrase))
    (setq frag2 (get frag2 'prev-frag))))

;;; frag predicate

(defun ew-token-last-frag-p (frag)
  (member (get frag 'type)
	  '(ew:*anchor*
	    ew:lt
	    ew:gt
	    ew:at
	    ew:comma
	    ew:semicolon
	    ew:colon
	    ew:dot
	    ew:atom
	    ew:qs-end
	    ew:dl-end)))

(defun ew-comment-frag-p (frag)
  (member (get frag 'type)
	  '(ew:cm-begin
	    ew:cm-end
	    ew:cm-nested-begin
	    ew:cm-nested-end
	    ew:cm-texts
	    ew:cm-wsp
	    ew:cm-fold
	    ew:cm-qfold
	    ew:cm-qpair)))

(defun ew-special-frag-p (frag)
  (member (get frag 'type)
	  '(ew:lt
	    ew:gt
	    ew:at
	    ew:comma
	    ew:semicolon
	    ew:colon
	    ew:dot
	    ew:qs-begin
	    ew:qs-end
	    ew:dl-begin
	    ew:dl-end
	    ew:cm-begin
	    ew:cm-end)))
