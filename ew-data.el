(provide 'ew-data)

(defun ew-make-anchor (column str)
  (let ((anchor (make-symbol str)))
    (put anchor 'anchor anchor)
    (put anchor 'prev-frag anchor)
    (put anchor 'next-frag anchor)
    (put anchor 'prev-token anchor)
    (put anchor 'next-token anchor)
    (put anchor 'column column)
    (put anchor 'line-length 0)
    anchor))

(defun ew-terminate (anchor)
  (let ((frag (get anchor 'prev-frag))
	(line-length (get anchor 'column)))
    (while (null (get frag 'line-length))
      (put frag 'line-length line-length)
      (setq frag (get frag 'prev-frag)))))

(defun ew-tokenize-frag (anchor frag)
  (put frag 'prev-token (get anchor 'prev-token))
  (put frag 'next-token anchor)
  (put (get anchor 'prev-token) 'next-token frag)
  (put anchor 'prev-token frag)
  frag)

(defun ew-add-frag (anchor start end type)
  (let ((frag (make-symbol (substring (symbol-name anchor) start end))))
    (put frag 'anchor anchor)
    (put frag 'start start)
    (put frag 'end end)
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

(defun ew-add-open (anchor start end type)
  (let ((frag (ew-add-frag anchor start end type)))
    (put frag 'prev-open (get anchor 'prev-open))
    (put anchor 'prev-open frag)
    frag))

(defun ew-add-close (anchor start end type)
  (let ((frag (ew-add-frag anchor start end type)))
    (put frag 'pair (get anchor 'prev-open))
    (put (get anchor 'prev-open) 'pair frag)
    (put anchor 'prev-open (get (get frag 'pair) 'prev-open))
    frag))
    
(defun ew-add-token (anchor start end type)
  (ew-tokenize-frag anchor (ew-add-frag anchor start end type)))

(defun ew-add-close-token (anchor start end type)
  (ew-tokenize-frag anchor (ew-add-close anchor start end type)))

;;; listup

(defun ew-frag-list (anchor)
  (let ((res ())
	(tmp (get anchor 'prev-frag)))
    (while (not (eq anchor tmp))
      (setq res (cons tmp res)
	    tmp (get tmp 'prev-frag)))
    res))

(defun ew-token-list (anchor)
  (let ((res ())
	(tmp (get anchor 'prev-token)))
    (while (not (eq anchor tmp))
      (setq res (cons tmp res)
	    tmp (get tmp 'prev-token)))
    res))

(defun ew-pair-list (anchor)
  (mapcar
   (lambda (frag)
     (cons (symbol-value (get frag 'type))
           frag))
   (ew-frag-list anchor)))

;;; phrase marking

(defun ew-mark-phrase (frag1 frag2)
  (while (not (eq frag1 frag2))
    (unless (ew-comment-frag-p frag2)
      (put frag2 'decode 'ew-decode-phrase))
    (setq frag2 (get frag2 'prev-frag)))
  (unless (ew-comment-frag-p frag2)
    (put frag2 'decode 'ew-decode-phrase))
  (setq frag2 (get frag2 'prev-frag))
  (while (not (get frag2 'prev-token))
    (unless (ew-comment-frag-p frag2)
      (put frag2 'decode 'ew-decode-phrase))
    (setq frag2 (get frag2 'prev-frag))))

;;; frag predicate

(defun ew-comment-frag-p (frag)
  (member (get frag 'type)
	  '(ew:raw-cm-begin-tok
	    ew:raw-cm-end-tok
	    ew:raw-cm-nested-begin-tok
	    ew:raw-cm-nested-end-tok
	    ew:raw-cm-texts-tok
	    ew:raw-cm-wsp-tok
	    ew:raw-cm-fold-tok
	    ew:raw-cm-qfold-tok
	    ew:raw-cm-qpair-tok)))

(defun ew-special-frag-p (frag)
  (or (eq frag (get frag 'anchor))
      (member (get frag 'type)
	      '(ew:raw-lt-tok
		ew:raw-gt-tok
		ew:raw-at-tok
		ew:raw-comma-tok
		ew:raw-semicolon-tok
		ew:raw-colon-tok
		ew:raw-dot-tok
		ew:raw-qs-begin-tok
		ew:raw-qs-end-tok
		ew:raw-dl-begin-tok
		ew:raw-dl-end-tok
		ew:raw-cm-begin-tok
		ew:raw-cm-end-tok))))
