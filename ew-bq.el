(require 'ccl)
(require 'emu)

(provide 'ew-bq)

;;;

(defvar ew-ccl-use-symbol
  (eval-when-compile
    (define-ccl-program ew-ccl-identity
      '(1 ((read r0) (loop (write-read-repeat r0)))))
    (condition-case nil
	(progn
	  (make-coding-system
	   'ew-ccl-identity 4 ?I
	   "Identity coding system for byte-compile time checking"
	   '(ew-ccl-identity . ew-ccl-identity))
	  t)
      (error nil))))

(defvar ew-ccl-untrusted-eof-block
  (eval-when-compile
    (let ((status (make-vector 9 nil)))
      (ccl-execute-on-string
       (ccl-compile
        '(0 (read r0) (r0 = 1)))
       status
       "")
      (= (aref status 0) 0))))

(defun ew-make-ccl-coding-system (coding-system mnemonic doc-string decoder encoder)
  (make-coding-system
   coding-system 4 mnemonic doc-string
   (if ew-ccl-use-symbol
       (cons decoder encoder)
     (cons (symbol-value decoder) (symbol-value encoder)))))

;;;

(eval-when-compile

(defconst ew-ccl-4-table
  '(  0   1   2   3))

(defconst ew-ccl-16-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15))

(defconst ew-ccl-64-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63))

(defconst ew-ccl-256-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
     64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
     80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
     96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
    112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
    128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143
    144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159
    160 161 162 163 164 165 166 167 168 169 170 171 172 173 174 175
    176 177 178 179 180 181 182 183 184 185 186 187 188 189 190 191
    192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207
    208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223
    224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239
    240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255))

(defconst ew-ccl-256-to-16-table
  '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
      0   1   2   3   4   5   6   7   8   9 nil nil nil nil nil nil
    nil  10  11  12  13  14  15 nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(defconst ew-ccl-16-to-256-table
  '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?A ?B ?C ?D ?E ?F))

(defconst ew-ccl-high-table
  (vconcat
   (mapcar
    (lambda (v) (nth (lsh v -4) ew-ccl-16-to-256-table))
    ew-ccl-256-table)))

(defconst ew-ccl-low-table
  (vconcat
   (mapcar
    (lambda (v) (nth (logand v 15) ew-ccl-16-to-256-table))
    ew-ccl-256-table)))

(defconst ew-ccl-u-raw
  (append
   "0123456789"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "abcdefghijklmnopqrstuvwxyz"
   "!@#$%&'()*+,-./:;<>@[\\]^`{|}~"
   ()))

(defconst ew-ccl-c-raw
  (append
   "0123456789"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "abcdefghijklmnopqrstuvwxyz"
   "!@#$%&'*+,-./:;<>@[]^`{|}~"
   ()))

(defconst ew-ccl-p-raw
  (append
   "0123456789"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "abcdefghijklmnopqrstuvwxyz"
   "!*+-/"
   ()))

(defconst ew-ccl-256-to-64-table
  '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil  62 nil nil nil  63
     52  53  54  55  56  57  58  59  60  61 nil nil nil   t nil nil
    nil   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14
     15  16  17  18  19  20  21  22  23  24  25 nil nil nil nil nil
    nil  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
     41  42  43  44  45  46  47  48  49  50  51 nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(defconst ew-ccl-64-to-256-table
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
    ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z ?a ?b ?c ?d ?e ?f
    ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v
    ?w ?x ?y ?z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?+ ?/))

(defconst ew-ccl-qp-table
  [enc enc enc enc enc enc enc enc enc wsp enc enc enc cr  enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   wsp raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw enc raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw
   raw raw raw raw raw raw raw raw raw raw raw raw raw raw raw enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc
   enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc enc])

)

(define-ccl-program ew-ccl-decode-q
  (eval-when-compile
    `(1
      ((loop
	(read-branch
	 r0
	 ,@(mapcar
	    (lambda (r0)
	      (cond
	       ((= r0 ?_)
		`(write-repeat ? ))
	       ((= r0 ?=)
		`((loop
		   (read-branch
		    r1
		    ,@(mapcar
		       (lambda (v)
			 (if (integerp v)
			     `((r0 = ,v) (break))
			   '(repeat)))
		       ew-ccl-256-to-16-table)))
		  (loop
		   (read-branch
		    r1
		    ,@(mapcar
		       (lambda (v)
			 (if (integerp v)
			     `((write r0 ,(vconcat
					   (mapcar
					    (lambda (r0)
					      (logior (lsh r0 4) v))
					    ew-ccl-16-table)))
			       (break))
			   '(repeat)))
		       ew-ccl-256-to-16-table)))
		  (repeat)))
	       (t
		`(write-repeat ,r0))))
	    ew-ccl-256-table)))))))

(define-ccl-program ew-ccl-encode-uq
  (eval-when-compile
    `(3
      (loop
       (loop
	(read-branch
	 r0
	 ,@(mapcar
	    (lambda (r0)
	      (cond
	       ((= r0 32) `(write-repeat ?_))
	       ((member r0 ew-ccl-u-raw) `(write-repeat ,r0))
	       (t '(break))))
	    ew-ccl-256-table)))
       (write ?=)
       (write r0 ,ew-ccl-high-table)
       (write r0 ,ew-ccl-low-table)
       (repeat)))))

(define-ccl-program ew-ccl-encode-cq
  (eval-when-compile
    `(3
      (loop
       (loop
	(read-branch
	 r0
	 ,@(mapcar
	    (lambda (r0)
	      (cond
	       ((= r0 32) `(write-repeat ?_))
	       ((member r0 ew-ccl-c-raw) `(write-repeat ,r0))
	       (t '(break))))
	    ew-ccl-256-table)))
       (write ?=)
       (write r0 ,ew-ccl-high-table)
       (write r0 ,ew-ccl-low-table)
       (repeat)))))

(define-ccl-program ew-ccl-encode-pq
  (eval-when-compile
    `(3
      (loop
       (loop
	(read-branch
	 r0
	 ,@(mapcar
	    (lambda (r0)
	      (cond
	       ((= r0 32) `(write-repeat ?_))
	       ((member r0 ew-ccl-p-raw) `(write-repeat ,r0))
	       (t '(break))))
	    ew-ccl-256-table)))
       (write ?=)
       (write r0 ,ew-ccl-high-table)
       (write r0 ,ew-ccl-low-table)
       (repeat)))))

(eval-when-compile
(defun ew-ccl-decode-b-bit-ex (v)
  (logior
   (lsh (logand v (lsh 255 16)) -16)
   (logand v (lsh 255 8))
   (lsh (logand v 255) 16)))

(defconst ew-ccl-decode-b-0-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (ew-ccl-decode-b-bit-ex (lsh v 18))
	(lsh 1 24)))
    ew-ccl-256-to-64-table)))

(defconst ew-ccl-decode-b-1-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (ew-ccl-decode-b-bit-ex (lsh v 12))
	(lsh 1 25)))
    ew-ccl-256-to-64-table)))

(defconst ew-ccl-decode-b-2-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (ew-ccl-decode-b-bit-ex (lsh v 6))
	(lsh 1 26)))
    ew-ccl-256-to-64-table)))

(defconst ew-ccl-decode-b-3-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (ew-ccl-decode-b-bit-ex v)
	(lsh 1 27)))
    ew-ccl-256-to-64-table)))

)

(define-ccl-program ew-ccl-decode-b
  `(1
    (loop
     (read r0 r1 r2 r3)
     (r4 = r0 ,ew-ccl-decode-b-0-table)
     (r5 = r1 ,ew-ccl-decode-b-1-table)
     (r4 |= r5)
     (r5 = r2 ,ew-ccl-decode-b-2-table)
     (r4 |= r5)
     (r5 = r3 ,ew-ccl-decode-b-3-table)
     (r4 |= r5)
     (if (r4 & ,(lognot (1- (lsh 1 24))))
	 ((loop
	   (if (r4 & ,(lsh 1 24))
	       ((r0 = r1) (r1 = r2) (r2 = r3) (read r3)
		(r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		(r5 = r3 ,ew-ccl-decode-b-3-table)
		(r4 |= r5)
		(repeat))
	     (break)))
	  (loop
	   (if (r4 & ,(lsh 1 25))
	       ((r1 = r2) (r2 = r3) (read r3)
		(r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		(r5 = r3 ,ew-ccl-decode-b-3-table)
		(r4 |= r5)
		(repeat))
	     (break)))
	  (loop
	   (if (r2 != ?=)
	       (if (r4 & ,(lsh 1 26))
		   ((r2 = r3) (read r3)
		    (r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		    (r5 = r3 ,ew-ccl-decode-b-3-table)
		    (r4 |= r5)
		    (repeat))
		 ((r6 = 0)
		  (break)))
	     ((r6 = 1)
	      (break))))
	  (loop
	   (if (r3 != ?=)
	       (if (r4 & ,(lsh 1 27))
		   ((read r3)
		    (r4 = r3 ,ew-ccl-decode-b-3-table)
		    (repeat))
		 (break))
	     ((r6 |= 2)
	      (break))))
	  (r4 = r0 ,ew-ccl-decode-b-0-table)
	  (r5 = r1 ,ew-ccl-decode-b-1-table)
	  (r4 |= r5)
	  (branch
	   r6
	   ;; BBBB
	   ((r5 = r2 ,ew-ccl-decode-b-2-table)
	    (r4 |= r5)
	    (r5 = r3 ,ew-ccl-decode-b-3-table)
	    (r4 |= r5)
	    (r4 >8= 0)
	    (write r7)
	    (r4 >8= 0)
	    (write r7)
	    (write-repeat r4))
	   ;; error: BB=B 
	   ((write r4)
	    (end))
	   ;; BBB=
	   ((r5 = r2 ,ew-ccl-decode-b-2-table)
	    (r4 |= r5)
	    (r4 >8= 0)
	    (write r7)
	    (write r4)
	    (end))
	   ;; BB==
	   ((write r4)
	    (end))))
       ((r4 >8= 0)
	(write r7)
	(r4 >8= 0)
	(write r7)
	(write-repeat r4))))))

;; ew-ccl-encode-b works only 20.3 or later because CCL_EOF_BLOCK
;; is not executed on 20.2 (or former?).
(define-ccl-program ew-ccl-encode-b
  (eval-when-compile
    `(2
      (loop
       (r2 = 0)
       (read-branch
	r1
	,@(mapcar
	   (lambda (r1)
	     `((write ,(nth (lsh r1 -2) ew-ccl-64-to-256-table))
	       (r0 = ,(logand r1 3))))
	   ew-ccl-256-table))
       (r2 = 1)
       (read-branch
	r1
	,@(mapcar
	   (lambda (r1)
	     `((write r0 ,(vconcat
			   (mapcar
			    (lambda (r0)
			      (nth (logior (lsh r0 4)
					   (lsh r1 -4))
				   ew-ccl-64-to-256-table))
			    ew-ccl-4-table)))
	       (r0 = ,(logand r1 15))))
	   ew-ccl-256-table))
       (r2 = 2)
       (read-branch
	r1
	,@(mapcar
	   (lambda (r1)
	     `((write r0 ,(vconcat
			   (mapcar
			    (lambda (r0)
			      (nth (logior (lsh r0 2)
					   (lsh r1 -6))
				   ew-ccl-64-to-256-table))
			    ew-ccl-16-table)))))
	   ew-ccl-256-table))
       (r1 &= 63)
       (write r1 ,(vconcat
		   (mapcar
		    (lambda (r1)
		      (nth r1 ew-ccl-64-to-256-table))
		    ew-ccl-64-table)))
       (repeat))
      (branch
       r2
       (end)
       ((write r0 ,(vconcat
		    (mapcar
		     (lambda (r0)
		       (nth (lsh r0 4) ew-ccl-64-to-256-table))
		     ew-ccl-4-table)))
	(write "=="))
       ((write r0 ,(vconcat
		    (mapcar
		     (lambda (r0)
		       (nth (lsh r0 2) ew-ccl-64-to-256-table))
		     ew-ccl-16-table)))
	(write ?=)))
      )))

;;;

;; ew-ccl-encode-base64 does not works on 20.2 by same reason of ew-ccl-encode-b
(define-ccl-program ew-ccl-encode-base64
  (eval-when-compile
    `(2
      ((r3 = 0)
       (loop
	(r2 = 0)
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (r1)
	      `((write ,(nth (lsh r1 -2) ew-ccl-64-to-256-table))
		(r0 = ,(logand r1 3))))
	    ew-ccl-256-table))
	(r2 = 1)
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (r1)
	      `((write r0 ,(vconcat
			    (mapcar
			     (lambda (r0)
			       (nth (logior (lsh r0 4)
					    (lsh r1 -4))
				    ew-ccl-64-to-256-table))
			     ew-ccl-4-table)))
		(r0 = ,(logand r1 15))))
	    ew-ccl-256-table))
	(r2 = 2)
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (r1)
	      `((write r0 ,(vconcat
			    (mapcar
			     (lambda (r0)
			       (nth (logior (lsh r0 2)
					    (lsh r1 -6))
				    ew-ccl-64-to-256-table))
			     ew-ccl-16-table)))))
	    ew-ccl-256-table))
	(r1 &= 63)
	(write r1 ,(vconcat
		    (mapcar
		     (lambda (r1)
		       (nth r1 ew-ccl-64-to-256-table))
		     ew-ccl-64-table)))
	(r3 += 1)
	(if (r3 == 19) ; 4 * 19 = 76 --> line break.
	    ((write "\r\n")
	     (r3 = 0)))
	(repeat)))
      (branch
       r2
       (if (r0 > 0) (write "\r\n"))
       ((write r0 ,(vconcat
		    (mapcar
		     (lambda (r0)
		       (nth (lsh r0 4) ew-ccl-64-to-256-table))
		     ew-ccl-4-table)))
	(write "==\r\n"))
       ((write r0 ,(vconcat
		    (mapcar
		     (lambda (r0)
		       (nth (lsh r0 2) ew-ccl-64-to-256-table))
		     ew-ccl-16-table)))
	(write "=\r\n")))
      )))

;; ew-ccl-encode-quoted-printable does not works on 20.2 by same reason of ew-ccl-encode-b
(define-ccl-program ew-ccl-encode-quoted-printable
  (eval-when-compile
    `(4
      ((r6 = 0) ; column
       (r5 = 0) ; previous character is white space
       (r4 = 0)
       (read r0)
       (loop ; r6 <= 75
	(loop
	 (loop
	  (branch
	   r0
	   ,@(mapcar
	      (lambda (r0)
		(let ((tmp (aref ew-ccl-qp-table r0)))
		  (cond
		   ((eq tmp 'raw) '((r3 = 0) (break))) ; RAW
		   ((eq tmp 'enc) '((r3 = 1) (break))) ; ENC
		   ((eq tmp 'wsp) '((r3 = 2) (break))) ; WSP
		   ((eq tmp 'cr) '((r3 = 3) (break))) ; CR
		   )))
	      ew-ccl-256-table)))
	 (branch
	  r3
	  ;; r0:r3=RAW
	  (if (r6 < 75)
	      ((r6 += 1)
	       (r5 = 0)
	       (r4 = 1)
	       (write-read-repeat r0))
	    (break))
	  ;; r0:r3=ENC
	  ((r5 = 0)
	   (if (r6 < 73)
	       ((r6 += 3)
		(write "=")
		(write r0 ,ew-ccl-high-table)
		(r4 = 2)
		(write-read-repeat r0 ,ew-ccl-low-table))
	     (if (r6 > 73)
		 ((r6 = 3)
		  (write "=\r\n=")
		  (write r0 ,ew-ccl-high-table)
		  (r4 = 3)
		  (write-read-repeat r0 ,ew-ccl-low-table))
	       (break))))
	  ;; r0:r3=WSP
	  ((r5 = 1)
	   (if (r6 < 75)
	       ((r6 += 1)
		(r4 = 4)
		(write-read-repeat r0))
	     ((r6 = 1)
	      (write "=\r\n")
	      (r4 = 5)
	      (write-read-repeat r0))))
	  ;; r0:r3=CR
	  ((if ((r6 > 73) & r5)
	       ((r6 = 0)
		(r5 = 0)
		(write "=\r\n")))
	   (break))))
	;; r0:r3={RAW,ENC,CR}
	(loop
	 (if (r0 == ?\r)
	     ;; r0=\r:r3=CR
	     ((r4 = 6)
	      (read r0)
	      ;; CR:r3=CR r0
	      (if (r0 == ?\n)
		  ;; CR:r3=CR r0=LF
		  (if r5
		      ;; r5=WSP ; CR:r3=CR r0=LF
		      ((r6 = 0)
		       (r5 = 0)
		       (write "=\r\n\r\n")
		       (r4 = 7)
		       (read r0)
		       (break))
		    ;; r5=noWSP ; CR:r3=CR r0=LF
		    ((r6 = 0)
		     (r5 = 0)
		     (write "\r\n")
		     (r4 = 8)
		     (read r0)
		     (break)))
		;; CR:r3=CR r0=noLF
		(if (r6 < 73)
		    ((r6 += 3)
		     (r5 = 0)
		     (write "=0D")
		     (break))
		  (if (r6 == 73)
                      (if (r0 == ?\r)
                          ;; CR:r3=CR r0=CR
                          ((r4 = 9)
                           (read r0)
                           ;; CR:r3=CR CR r0
                           (if (r0 == ?\n)
                               ;; CR:r3=CR CR LF
                               ((r6 = 0)
                                (r5 = 0)
                                (write "=0D\r\n")
                                (r4 = 10)
                                (read r0)
                                (break))
                             ;; CR:r3=CR CR noLF
                             ((r6 = 6)
                              (r5 = 0)
                              (write "=\r\n=0D=0D")
                              (break))))
                        ;; CR:r3=CR r0=noLFnorCR
                        ((r6 = 3)
                         (r5 = 0)
                         (write "=\r\n=0D")
                         (break)))
		    ((r6 = 3)
		     (r5 = 0)
		     (write "=\r\n=0D")
		     (break))))))
	   ;; r0:r3={RAW,ENC}
	   ((r4 = 11)
	    (read r1)
	    ;; r0:r3={RAW,ENC} r1
	    (if (r1 == ?\r)
		;; r0:r3={RAW,ENC} r1=CR
		((r4 = 12)
		 (read r1)
		 ;; r0:r3={RAW,ENC} CR r1
		 (if (r1 == ?\n)
		     ;; r0:r3={RAW,ENC} CR r1=LF
		     ((r6 = 0)
		      (r5 = 0)
		      (branch
		       r3
		       ;; r0:r3=RAW CR r1=LF
		       ((write r0)
			(write "\r\n")
			(r4 = 13)
			(read r0)
			(break))
		       ;; r0:r3=ENC CR r1=LF
		       ((write ?=)
			(write r0 ,ew-ccl-high-table)
			(write r0 ,ew-ccl-low-table)
			(write "\r\n")
			(r4 = 14)
			(read r0)
			(break))))
		   ;; r0:r3={RAW,ENC} CR r1=noLF
		   ((branch
		     r3
		     ;; r0:r3=RAW CR r1:noLF
		     ((r6 = 4)
		      (r5 = 0)
		      (write "=\r\n")
		      (write r0)
		      (write "=0D")
		      (r0 = r1)
		      (break))
		     ;; r0:r3=ENC CR r1:noLF
		     ((r6 = 6)
		      (r5 = 0)
		      (write "=\r\n=")
		      (write r0 ,ew-ccl-high-table)
		      (write r0 ,ew-ccl-low-table)
		      (write "=0D")
		      (r0 = r1)
		      (break))))
		   ))
	      ;; r0:r3={RAW,ENC} r1:noCR
	      ((branch
		r3
		;; r0:r3=RAW r1:noCR
		((r6 = 1)
		 (r5 = 0)
		 (write "=\r\n")
		 (write r0)
		 (r0 = r1)
		 (break))
		;; r0:r3=ENC r1:noCR
		((r6 = 3)
		 (r5 = 0)
		 (write "=\r\n=")
		 (write r0 ,ew-ccl-high-table)
		 (write r0 ,ew-ccl-low-table)
		 (r0 = r1)
		 (break))))))))
	(repeat)))
      (;(write "[EOF:") (write r4 ,ew-ccl-high-table) (write r4 ,ew-ccl-low-table) (write "]")
       (branch
	r4
	;; 0: (start) ;
	(end)
	;; 1: RAW ;
	(end)
	;; 2: r0:r3=ENC ;
	(end)
	;; 3: SOFTBREAK r0:r3=ENC ;
	(end)
	;; 4: r0:r3=WSP ;
	((write "=\r\n") (end))
	;; 5: SOFTBREAK r0:r3=WSP ;
	((write "=\r\n") (end))
	;; 6: ; r0=\r:r3=CR
	(if (r6 <= 73)
	    ((write "=0D") (end))
	   ((write "=\r\n=0D") (end)))
	;; 7: r5=WSP SOFTBREAK CR:r3=CR r0=LF ;
	(end)
	;; 8: r5=noWSP CR:r3=CR r0=LF ;
	(end)
	;; 9: (r6=73) ; CR:r3=CR r0=CR
        ((write "=\r\n=0D=0D") (end))
	;; 10: (r6=73) CR:r3=CR CR LF ;
        (end)
	;; 11: ; r0:r3={RAW,ENC}
	(branch
	 r3
	 ((write r0) (end))
	 ((write "=")
	  (write r0 ,ew-ccl-high-table)
	  (write r0 ,ew-ccl-low-table)
	  (end)))
	;; 12: ; r0:r3={RAW,ENC} r1=CR
	(branch
	 r3
	 ((write "=\r\n")
	  (write r0)
	  (write "=0D")
	  (end))
	 ((write "=\r\n=")
	  (write r0 ,ew-ccl-high-table)
	  (write r0 ,ew-ccl-low-table)
	  (write "=0D")
	  (end)))
	;; 13: r0:r3=RAW CR LF ;
	(end)
	;; 14: r0:r3=ENC CR LF ;
	(end)
	))
      )))

(define-ccl-program ew-ccl-decode-quoted-printable
  (eval-when-compile
    `(1
      ((read r0)
       (loop
	(branch
	 r0
	 ,@(mapcar
	    (lambda (r0)
	      (let ((tmp (aref ew-ccl-qp-table r0)))
		(cond
		 ((or (eq tmp 'raw) (eq tmp 'wsp)) `(write-read-repeat r0))
		 ((eq r0 ?=)
		  ;; r0='='
		  `((read r0)
		    ;; '=' r0
		    (r1 = (r0 == ?\t))
		    (if ((r0 == ? ) | r1)
			;; '=' r0:[\t ]
			;; Skip transport-padding.
			;; It should check CR LF after
			;; transport-padding.
			(loop
			 (read-if (r0 == ?\t)
				  (repeat)
				  (if (r0 == ? )
				      (repeat)
				    (break)))))
		    ;; '=' [\t ]* r0:[^\t ]
		    (branch
		     r0
		     ,@(mapcar
			(lambda (r0)
			  (cond
			   ((eq r0 ?\r)
			    ;; '=' [\t ]* r0='\r'
			    `((read r0)
			      ;; '=' [\t ]* '\r' r0
			      (if (r0 == ?\n)
				  ;; '=' [\t ]* '\r' r0='\n'
				  ;; soft line break found.
				  ((read r0)
				   (repeat))
				;; '=' [\t ]* '\r' r0:[^\n]
				;; invalid input ->
				;; output "=\r" and rescan from r0.
				((write "=\r")
				 (repeat)))))
			   ((setq tmp (nth r0 ew-ccl-256-to-16-table))
			    ;; '=' [\t ]* r0:[0-9A-F]
			    `(r0 = ,tmp))
			   (t
			    ;; '=' [\t ]* r0:[^\r0-9A-F]
			    ;; invalid input ->
			    ;; output "=" and rescan from r0.
			    `((write ?=)
			      (repeat)))))
			ew-ccl-256-table))
		    ;; '=' [\t ]* r0:[0-9A-F]
		    (read-branch
		     r1
		     ,@(mapcar
			(lambda (r1)
			  (if (setq tmp (nth r1 ew-ccl-256-to-16-table))
			      ;; '=' [\t ]* [0-9A-F] r1:[0-9A-F]
			      `(write-read-repeat
				r0
				,(vconcat
				  (mapcar
				   (lambda (r0)
				     (logior (lsh r0 4) tmp))
				   ew-ccl-16-table)))
			    ;; '=' [\t ]* [0-9A-F] r1:[^0-9A-F]
			    ;; invalid input
			    `(r2 = 0)	; nop
			    ))
			ew-ccl-256-table))
                    ;; '=' [\t ]* [0-9A-F] r1:[^0-9A-F]
                    ;; invalid input
		    (write ?=)
		    (write r0 ,(vconcat ew-ccl-16-to-256-table))
		    (write r1)
		    (read r0)
		    (repeat)))
		 ((eq tmp 'cr)
		  ;; r0='\r'
		  `((read r0)
		    ;; '\r' r0
		    (if (r0 == ?\n)
			;; '\r' r0='\n'
			;; hard line break found.
			((write ?\r)
			 (write-read-repeat r0))
		      ;; '\r' r0:[^\n]
		      ;; invalid control character (bare CR) found.
		      ;; -> ignore it and rescan from r0.
		      (repeat))))
		 (t
		  ;; r0:[^\t\r -~]
		  ;; invalid character found.
		  ;; -> ignore.
		  `((read r0)
		    (repeat))))))
	    ew-ccl-256-table)))))))

;;;

(ew-make-ccl-coding-system
 'ew-ccl-uq ?Q "MIME Q-encoding in unstructured field"
 'ew-ccl-decode-q 'ew-ccl-encode-uq)

(ew-make-ccl-coding-system
 'ew-ccl-cq ?Q "MIME Q-encoding in comment"
 'ew-ccl-decode-q 'ew-ccl-encode-cq)

(ew-make-ccl-coding-system
 'ew-ccl-pq ?Q "MIME Q-encoding in phrase"
 'ew-ccl-decode-q 'ew-ccl-encode-pq)

(ew-make-ccl-coding-system
 'ew-ccl-b ?B "MIME B-encoding"
 'ew-ccl-decode-b 'ew-ccl-encode-b)

(ew-make-ccl-coding-system
 'ew-ccl-quoted-printable ?Q "MIME Quoted-Printable-encoding"
 'ew-ccl-decode-quoted-printable 'ew-ccl-encode-quoted-printable)

(ew-make-ccl-coding-system
 'ew-ccl-base64 ?B "MIME Base64-encoding"
 'ew-ccl-decode-b 'ew-ccl-encode-base64)

;;;
(require 'mel)
(defvar ew-bq-use-mel nil)

(defun ew-encode-uq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-uq))

(defun ew-encode-cq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-cq))

(defun ew-encode-pq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-pq))

(if ew-bq-use-mel
    (defalias 'ew-decode-q 'q-encoding-decode-string)
  (defun ew-decode-q (str)
    (string-as-unibyte (decode-coding-string str 'ew-ccl-uq))))

(if (or ew-bq-use-mel base64-dl-module ew-ccl-untrusted-eof-block)
    (defalias 'ew-encode-b 'base64-encode-string)
  (defun ew-encode-b (str)
    (encode-coding-string (string-as-unibyte str) 'ew-ccl-b)))

(if (or ew-bq-use-mel base64-dl-module)
    (defalias 'ew-decode-b 'base64-decode-string)
  (defun ew-decode-b (str)
    (string-as-unibyte (decode-coding-string str 'ew-ccl-b))))

'(

(ew-encode-uq "\000\037 !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~\177\200\377")
(ew-encode-cq "\000\037 !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~\177\200\377")
(ew-encode-pq "\000\037 !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~\177\200\377")
(ew-encode-b "\000\037 !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~\177\200\377")

(ew-decode-q "a_b=20c")
(ew-decode-q "=92=A4=A2")
(ew-decode-b "SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=")

(let ((i 1000))
  (while (< 0 i)
    (setq i (1- i))
    (ew-decode-b
     "AB8gISIjJCUmJygpKissLS4vMDk6Ozw9Pj9AQVpbXF1eX2Bhent8fX5/gP8=")))

(let ((i 1000))
  (while (< 0 i)
    (setq i (1- i))
    (ew-decode-q
     "=00=1F_!=22#$%&'=28=29*+,-./09:;<=3D>=3F@AZ[=5C]^=5F`az{|}~=7F=80=FF")))

(require 'mel)

(let ((i 1000))
  (while (< 0 i)
    (setq i (1- i))
    (base64-decode-string
     "AB8gISIjJCUmJygpKissLS4vMDk6Ozw9Pj9AQVpbXF1eX2Bhent8fX5/gP8=")))

(let ((i 1000))
  (while (< 0 i)
    (setq i (1- i))
    (q-encoding-decode-string
     "=00=1F_!=22#$%&'=28=29*+,-./09:;<=3D>=3F@AZ[=5C]^=5F`az{|}~=7F=80=FF")))

(let (a b) ; CCL
  (setq a (current-time))
  (let ((i 1000))
    (while (< 0 i)
      (setq i (1- i))
      (ew-decode-b
       "AB8gISIjJCUmJygpKissLS4vMDk6Ozw9Pj9AQVpbXF1eX2Bhent8fX5/gP8=")))
  (setq b (current-time))
  (elp-elapsed-time a b))

(let (a b) ; Emacs Lisp
  (setq a (current-time))
  (let ((i 1000))
    (while (< 0 i)
      (setq i (1- i))
      (base64-decode-string
       "AB8gISIjJCUmJygpKissLS4vMDk6Ozw9Pj9AQVpbXF1eX2Bhent8fX5/gP8=")))
  (setq b (current-time))
  (elp-elapsed-time a b))

(let (a b) ; DL
  (setq a (current-time))
  (let ((i 1000))
    (while (< 0 i)
      (setq i (1- i))
      (decode-base64-string
       "AB8gISIjJCUmJygpKissLS4vMDk6Ozw9Pj9AQVpbXF1eX2Bhent8fX5/gP8=")))
  (setq b (current-time))
  (elp-elapsed-time a b))

(let ((i 100000) (status (make-vector 9 nil)) a b)
  (setq a (current-time))
  (while (< 0 i)
    (setq i (1- i))
    (ccl-execute-on-string
     ew-ccl-decode-b ; or ew-ccl-decode-b-2 or -3
     status
     "AB8gISIjJCUmJygpKissLS4vMDk6Ozw9Pj9AQVpbXF1eX2Bhent8fX5/gP8="))
  (setq b (current-time))
  (elp-elapsed-time a b))


)
