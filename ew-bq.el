(require 'ccl)
(require 'emu)

(provide 'ew-bq)

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
  (append "!@#$%&'()*+,-./0123456789:;<>@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^`abcdefghijklmnopqrstuvwxyz{|}~" ()))

(defconst ew-ccl-c-raw
  (append "!@#$%&'*+,-./0123456789:;<>@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^`abcdefghijklmnopqrstuvwxyz{|}~" ()))

(defconst ew-ccl-p-raw
  (append "!*+-/0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" ()))

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
    `(1
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
    `(1
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
    `(1
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

(define-ccl-program ew-ccl-decode-b
  (eval-when-compile
    `(1
      (loop
       (loop
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (v)
	      (cond
	       ((or (eq v nil) (eq v t)) '(repeat))
	       (t `((r0 = ,(lsh v 2)) (break)))))
	    ew-ccl-256-to-64-table)))
       (loop
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (v)
	      (cond
	       ((or (eq v nil) (eq v t)) '(repeat))
	       ((= (lsh v -4) 0) `((write r0) (r0 = ,(lsh (logand v 15) 4)) (break)))
	       (t `((write (r0 | ,(lsh v -4))) (r0 = ,(lsh (logand v 15) 4)) (break)))))
	    ew-ccl-256-to-64-table)))
       (loop
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (v)
	      (cond
	       ((eq v nil) '(repeat))
	       ((eq v t) '(end))
	       ((= (lsh v -2) 0) `((write r0) (r0 = ,(lsh (logand v 3) 6)) (break)))
	       (t `((write (r0 | ,(lsh v -2))) (r0 = ,(lsh (logand v 3) 6)) (break)))))
	    ew-ccl-256-to-64-table)))
       (loop
	(read-branch
	 r1
	 ,@(mapcar
	    (lambda (v)
	      (cond
	       ((eq v nil) '(repeat))
	       ((eq v t) '(end))
	       (t `((write (r0 | ,v)) (break)))))
	    ew-ccl-256-to-64-table)))
       (repeat)))))

(eval-and-compile

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

)

;;;

(make-coding-system 'ew-ccl-uq 4 ?Q "Q-encoding in unstructured field"
		    (cons ew-ccl-decode-q ew-ccl-encode-uq))

(make-coding-system 'ew-ccl-cq 4 ?Q "Q-encoding in comment"
		    (cons ew-ccl-decode-q ew-ccl-encode-cq))

(make-coding-system 'ew-ccl-pq 4 ?Q "Q-encoding in phrase"
		    (cons ew-ccl-decode-q ew-ccl-encode-pq))

(make-coding-system 'ew-ccl-b 4 ?B "B-encoding"
		    (cons ew-ccl-decode-b ew-ccl-encode-b))

;;;

(eval-and-compile

(defconst ew-ccl-encode-b-is-broken
  (eval-when-compile
    (not (string= (ccl-execute-on-string ew-ccl-encode-b (make-vector 9 nil) "a")
		  "YQ=="))))
)

;;;

(defun ew-encode-uq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-uq))

(defun ew-encode-cq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-cq))

(defun ew-encode-pq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-pq))

(defun ew-decode-q (str)
  (string-as-unibyte (decode-coding-string str 'ew-ccl-uq)))

(require 'mel)
(if (or base64-dl-module ew-ccl-encode-b-is-broken)
    (defalias 'ew-encode-b 'base64-encode-string)
  (defun ew-encode-b (str)
    (encode-coding-string (string-as-unibyte str) 'ew-ccl-b)))

(if base64-dl-module
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
)
