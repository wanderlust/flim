;;;
;;; $Id$
;;;

;;; @ variables
;;;

(defvar base64-external-encoder '("mmencode")
  "*list of base64 encoder program name and its arguments.")

(defvar base64-external-decoder '("mmencode" "-u")
  "*list of base64 decoder program name and its arguments.")

(defvar base64-internal-encoding-limit 1000
  "*limit size to use internal base64 encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defvar base64-internal-decoding-limit 1000
  "*limit size to use internal base64 decoder.
If size of input to decode is larger than this limit,
external decoder is called.")


;;; @ internal base64 decoder/encoder
;;;	based on base64 decoder by Enami Tsugutomo

;;; @@ convert from/to base64 char
;;;

(defun base64-num-to-char (n)
  (cond ((eq n nil) ?=)
	((< n 26) (+ ?A n))
	((< n 52) (+ ?a (- n 26)))
	((< n 62) (+ ?0 (- n 52)))
	((= n 62) ?+)
	((= n 63) ?/)
	(t (error "not a base64 integer %d" n))))

(defun base64-char-to-num (c)
  (cond ((and (<= ?A c) (<= c ?Z)) (- c ?A))
	((and (<= ?a c) (<= c ?z)) (+ (- c ?a) 26))
	((and (<= ?0 c) (<= c ?9)) (+ (- c ?0) 52))
	((= c ?+) 62)
	((= c ?/) 63)
	((= c ?=) nil)
	(t (error "not a base64 character %c" c))))


;;; @@ encode/decode one base64 unit
;;;

(defun base64-mask (i n) (logand i (1- (ash 1 n))))

(defun base64-encode-1 (a &optional b &optional c)
  (cons (ash a -2)
	(cons (logior (ash (base64-mask a 2) (- 6 2))
		      (if b (ash b -4) 0))
	      (if b
		  (cons (logior (ash (base64-mask b 4) (- 6 4))
				(if c (ash c -6) 0))
			(if c
			    (cons (base64-mask c (- 6 0))
				  nil)))))))

(defun base64-decode-1 (a b &optional c &optional d)
  (cons (logior (ash a 2) (ash b (- 2 6)))
	(if c (cons (logior (ash (base64-mask b 4) 4)
			    (base64-mask (ash c (- 4 6)) 4))
		    (if d (cons (logior (ash (base64-mask c 2) 6) d)
				nil))))))

(defun base64-encode-chars (a &optional b &optional c)
  (mapcar (function base64-num-to-char) (base64-encode-1 a b c)))

(defun base64-decode-chars (&rest args)
  (apply (function base64-decode-1)
	 (mapcar (function base64-char-to-num) args)
	 ))


;;; @@ encode/decode base64 string
;;;

(defun base64-encode-string (string)
  (let* ((es (mapconcat
	      (function
	       (lambda (pack)
		 (mapconcat (function char-to-string)
			    (apply (function base64-encode-chars) pack)
			    "")
		 ))
	      (pack-sequence string 3)
	      ""))
	 (m (mod (length es) 4))
	 )
    (concat es (cond ((= m 3) "=")
		     ((= m 2) "==")
		     ))
    ))

(defun base64-decode-string (string)
  (mapconcat (function
	      (lambda (pack)
		(mapconcat (function char-to-string)
			   (apply (function base64-decode-chars) pack)
			   "")
		))
	     (pack-sequence string 4)
	     ""))


;;; @ encode/decode base64 region
;;;

(defun base64-internal-decode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	(replace-match "")
	)
      (let ((str (buffer-substring (point-min)(point-max))))
	(delete-region (point-min)(point-max))
	(insert (base64-decode-string str))
	))))

(defun base64-internal-encode-region (beg end)
  (save-excursion
    (let* ((str (base64-encode-string (buffer-substring beg end)))
	   (len (length str))
	   (i 0)
	   (j (if (>= len 76)
		  76
		len))
	   )
      (delete-region beg end)
      (goto-char beg)
      (while (< j len)
	(insert (substring str i j))
	(insert "\n")
	(setq i j)
	(setq j (+ i 76))
	)
      (insert (substring str i))
      )))

(cond ((boundp 'MULE)
       (define-program-coding-system
	 nil (car base64-external-encoder) *noconv*)
       (define-program-coding-system
	 nil (car base64-external-decoder) *noconv*)
       )
      ((boundp 'NEMACS)
       (define-program-kanji-code
	 nil (car base64-external-encoder) 0)
       (define-program-kanji-code
	 nil (car base64-external-decoder) 0)
       ))

(defun base64-external-encode-region (beg end)
  (save-excursion
    (let ((selective-display nil) ;Disable ^M to nl translation.
	  (mc-flag nil)      ;Mule
	  (kanji-flag nil))  ;NEmacs
      (apply (function call-process-region)
	     beg end (car base64-external-encoder)
	     t t nil (cdr base64-external-encoder))
      )))

(defun base64-external-decode-region (beg end)
  (save-excursion
    (let ((selective-display nil) ;Disable ^M to nl translation.
	  (mc-flag nil)      ;Mule
	  (kanji-flag nil))  ;NEmacs
      (apply (function call-process-region)
	     beg end (car base64-external-decoder)
	     t t nil (cdr base64-external-decoder))
      )))

(defun base64-encode-region (beg end)
  (interactive "r")
  (if (and base64-internal-encoding-limit
	   (> (- end beg) base64-internal-encoding-limit))
      (base64-external-encode-region beg end)
    (base64-internal-encode-region beg end)
    ))

(defun base64-decode-region (beg end)
  (interactive "r")
  (if (and base64-internal-decoding-limit
	   (> (- end beg) base64-internal-decoding-limit))
      (base64-external-decode-region beg end)
    (base64-internal-decode-region beg end)
    ))


;;; @ etc
;;;

(defun base64-encoded-length (string)
  (let ((len (length string)))
    (* (+ (/ len 3)
	  (if (= (mod len 3) 0) 0 1)
	  ) 4)
    ))

(defun pack-sequence (seq size)
  "Split sequence SEQ into SIZE elements packs,
and return list of packs. [mel-b; tl-seq function]"
  (let ((len (length seq)) (p 0) obj
	unit (i 0)
	dest)
    (while (< p len)
      (setq obj (elt seq p))
      (setq unit (cons obj unit))
      (setq i (1+ i))
      (if (= i size)
	  (progn
	    (setq dest (cons (reverse unit) dest))
	    (setq unit nil)
	    (setq i 0)
	    ))
      (setq p (1+ p))
      )
    (if unit
	(setq dest (cons (reverse unit) dest))
      )
    (reverse dest)
    ))

(provide 'mel-b)
