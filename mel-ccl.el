(require 'ccl)
(require 'emu)


;;; @ constants
;;;

(eval-when-compile

(defconst mel-ccl-4-table
  '(  0   1   2   3))

(defconst mel-ccl-16-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15))

(defconst mel-ccl-28-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27))

(defconst mel-ccl-64-table
  '(  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
     16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
     32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
     48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63))

(defconst mel-ccl-256-table
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

(defconst mel-ccl-256-to-16-table
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

(defconst mel-ccl-16-to-256-table
  (mapcar 'char-int "0123456789ABCDEF"))

(defconst mel-ccl-high-table
  (vconcat
   (mapcar
    (lambda (v) (nth (lsh v -4) mel-ccl-16-to-256-table))
    mel-ccl-256-table)))

(defconst mel-ccl-low-table
  (vconcat
   (mapcar
    (lambda (v) (nth (logand v 15) mel-ccl-16-to-256-table))
    mel-ccl-256-table)))

(defconst mel-ccl-u-raw
  (mapcar
   'char-int
   "0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
!@#$%&'()*+,-./:;<>@[\\]^`{|}~"))

(defconst mel-ccl-c-raw
  (mapcar
   'char-int
   "0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
!@#$%&'*+,-./:;<>@[]^`{|}~"))

(defconst mel-ccl-p-raw
  (mapcar
   'char-int
   "0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
!*+-/"))

(defconst mel-ccl-256-to-64-table
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

(defconst mel-ccl-64-to-256-table
  (mapcar
   'char-int
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz\
0123456789\
+/"))

(defconst mel-ccl-qp-table
  [enc enc enc enc enc enc enc enc enc wsp lf  enc enc cr  enc enc
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


;;; @ CCL programs
;;;

;;; Q

(define-ccl-program mel-ccl-decode-q
  `(1
    ((loop
      (read-branch
       r0
       ,@(mapcar
          (lambda (r0)
            (cond
             ((= r0 (char-int ?_))
              `(write-repeat ? ))
             ((= r0 (char-int ?=))
              `((loop
                 (read-branch
                  r1
                  ,@(mapcar
                     (lambda (v)
                       (if (integerp v)
                           `((r0 = ,v) (break))
                         '(repeat)))
                     mel-ccl-256-to-16-table)))
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
                                          mel-ccl-16-table)))
                             (break))
                         '(repeat)))
                     mel-ccl-256-to-16-table)))
                (repeat)))
             (t
              `(write-repeat ,r0))))
          mel-ccl-256-table))))))

(eval-when-compile

(defun mel-ccl-encode-q-generic (raw)
  `(3
    (loop
     (loop
      (read-branch
       r0
       ,@(mapcar
          (lambda (r0)
            (cond
             ((= r0 32) `(write-repeat ?_))
             ((member r0 raw) `(write-repeat ,r0))
             (t '(break))))
          mel-ccl-256-table)))
     (write ?=)
     (write r0 ,mel-ccl-high-table)
     (write r0 ,mel-ccl-low-table)
     (repeat))))

;; On xemacs, generated program counts iso-8859-1 8bit character as 6bytes.
(defun mel-ccl-count-q-length (raw)
  `(0
    ((r0 = 0)
     (loop
      (read-branch
       r1
       ,@(mapcar
	  (lambda (r1)
	    (if (or (= r1 32) (member r1 raw))
		'((r0 += 1) (repeat))
	      '((r0 += 3) (repeat))))
	  mel-ccl-256-table))))))

)

(define-ccl-program mel-ccl-encode-uq
  (mel-ccl-encode-q-generic mel-ccl-u-raw))
(define-ccl-program mel-ccl-encode-cq
  (mel-ccl-encode-q-generic mel-ccl-c-raw))
(define-ccl-program mel-ccl-encode-pq
  (mel-ccl-encode-q-generic mel-ccl-p-raw))

(define-ccl-program mel-ccl-count-uq
  (mel-ccl-count-q-length mel-ccl-u-raw))
(define-ccl-program mel-ccl-count-cq
  (mel-ccl-count-q-length mel-ccl-c-raw))
(define-ccl-program mel-ccl-count-pq
  (mel-ccl-count-q-length mel-ccl-p-raw))

;;; B/Base64

(eval-when-compile

(defun mel-ccl-decode-b-bit-ex (v)
  (logior
   (lsh (logand v (lsh 255 16)) -16)
   (logand v (lsh 255 8))
   (lsh (logand v 255) 16)))

(defconst mel-ccl-decode-b-0-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex (lsh v 18))
	(lsh 1 24)))
    mel-ccl-256-to-64-table)))

(defconst mel-ccl-decode-b-1-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex (lsh v 12))
	(lsh 1 25)))
    mel-ccl-256-to-64-table)))

(defconst mel-ccl-decode-b-2-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex (lsh v 6))
	(lsh 1 26)))
    mel-ccl-256-to-64-table)))

(defconst mel-ccl-decode-b-3-table
  (vconcat
   (mapcar
    (lambda (v)
      (if (integerp v)
	  (mel-ccl-decode-b-bit-ex v)
	(lsh 1 27)))
    mel-ccl-256-to-64-table)))

)

(define-ccl-program mel-ccl-decode-b
  `(1
    (loop
     (read r0 r1 r2 r3)
     (r4 = r0 ,mel-ccl-decode-b-0-table)
     (r5 = r1 ,mel-ccl-decode-b-1-table)
     (r4 |= r5)
     (r5 = r2 ,mel-ccl-decode-b-2-table)
     (r4 |= r5)
     (r5 = r3 ,mel-ccl-decode-b-3-table)
     (r4 |= r5)
     (if (r4 & ,(lognot (1- (lsh 1 24))))
	 ((loop
	   (if (r4 & ,(lsh 1 24))
	       ((r0 = r1) (r1 = r2) (r2 = r3) (read r3)
		(r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		(r5 = r3 ,mel-ccl-decode-b-3-table)
		(r4 |= r5)
		(repeat))
	     (break)))
	  (loop
	   (if (r4 & ,(lsh 1 25))
	       ((r1 = r2) (r2 = r3) (read r3)
		(r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		(r5 = r3 ,mel-ccl-decode-b-3-table)
		(r4 |= r5)
		(repeat))
	     (break)))
	  (loop
	   (if (r2 != ?=)
	       (if (r4 & ,(lsh 1 26))
		   ((r2 = r3) (read r3)
		    (r4 >>= 1) (r4 &= ,(logior (lsh 7 24)))
		    (r5 = r3 ,mel-ccl-decode-b-3-table)
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
		    (r4 = r3 ,mel-ccl-decode-b-3-table)
		    (repeat))
		 (break))
	     ((r6 |= 2)
	      (break))))
	  (r4 = r0 ,mel-ccl-decode-b-0-table)
	  (r5 = r1 ,mel-ccl-decode-b-1-table)
	  (r4 |= r5)
	  (branch
	   r6
	   ;; BBBB
	   ((r5 = r2 ,mel-ccl-decode-b-2-table)
	    (r4 |= r5)
	    (r5 = r3 ,mel-ccl-decode-b-3-table)
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
	   ((r5 = r2 ,mel-ccl-decode-b-2-table)
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

(eval-when-compile

;; Generated CCL program works not properly on 20.2 because CCL_EOF_BLOCK
;; is not executed.
(defun mel-ccl-encode-base64-generic (&optional quantums-per-line output-crlf terminate-with-newline)
  `(2
    ((r3 = 0)
     (loop
      (r2 = 0)
      (read-branch
       r1
       ,@(mapcar
          (lambda (r1)
            `((write ,(nth (lsh r1 -2) mel-ccl-64-to-256-table))
              (r0 = ,(logand r1 3))))
          mel-ccl-256-table))
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
                                  mel-ccl-64-to-256-table))
                           mel-ccl-4-table)))
              (r0 = ,(logand r1 15))))
          mel-ccl-256-table))
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
                                  mel-ccl-64-to-256-table))
                           mel-ccl-16-table)))))
          mel-ccl-256-table))
      (r1 &= 63)
      (write r1 ,(vconcat
                  (mapcar
                   (lambda (r1)
                     (nth r1 mel-ccl-64-to-256-table))
                   mel-ccl-64-table)))
      (r3 += 1)
      ,@(when quantums-per-line
	  `((if (r3 == ,quantums-per-line)
		((write ,(if output-crlf "\r\n" "\n"))
		 (r3 = 0)))))
      (repeat)))
    (branch
     r2
     ,(if terminate-with-newline
	  `(if (r3 > 0) (write ,(if output-crlf "\r\n" "\n")))
	`(r0 = 0))
     ((write r0 ,(vconcat
                  (mapcar
                   (lambda (r0)
                     (nth (lsh r0 4) mel-ccl-64-to-256-table))
                   mel-ccl-4-table)))
      (write ,(if terminate-with-newline
		  (if output-crlf "==\r\n" "==\n")
		"==")))
     ((write r0 ,(vconcat
                  (mapcar
                   (lambda (r0)
                     (nth (lsh r0 2) mel-ccl-64-to-256-table))
                   mel-ccl-16-table)))
      (write ,(if terminate-with-newline
		  (if output-crlf "=\r\n" "=\n")
		"="))))
    ))
)

(define-ccl-program mel-ccl-encode-b
  (mel-ccl-encode-base64-generic))

;; 19 * 4 = 76
(define-ccl-program mel-ccl-encode-base64-crlf-crlf
  (mel-ccl-encode-base64-generic 19 t))

(define-ccl-program mel-ccl-encode-base64-crlf-lf
  (mel-ccl-encode-base64-generic 19 nil))

;; Quoted-Printable

(eval-when-compile

;; Generated CCL program works not properly on 20.2 because CCL_EOF_BLOCK
;; is not executed.
(defun mel-ccl-encode-quoted-printable-generic (input-crlf output-crlf)
  `(4
    ((r6 = 0)				; column
     (r5 = 0)				; previous character is white space
     (r4 = 0)
     (read r0)
     (loop				; r6 <= 75
      (loop
       (loop
        (branch
         r0
         ,@(mapcar
            (lambda (r0)
              (let ((tmp (aref mel-ccl-qp-table r0)))
                (cond
                 ((eq tmp 'raw) '((r3 = 0) (break))) ; RAW
                 ((eq tmp 'enc) '((r3 = 1) (break))) ; ENC
                 ((eq tmp 'wsp) '((r3 = 2) (break))) ; WSP
                 ((eq tmp 'cr) (if input-crlf
                                   '((r3 = 3) (break)) ; CR
                                 '((r3 = 1) (break)))) ; ENC
                 ((eq tmp 'lf) (if input-crlf
                                   '((r3 = 1) (break)) ; ENC
                                 '((r3 = 3) (break)))) ; CRLF
                 )))
            mel-ccl-256-table)))
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
              (write r0 ,mel-ccl-high-table)
              (r4 = 2)
              (write-read-repeat r0 ,mel-ccl-low-table))
           (if (r6 > 73)
               ((r6 = 3)
                (write ,(if output-crlf "=\r\n=" "=\n="))
                (write r0 ,mel-ccl-high-table)
                (r4 = 3)
                (write-read-repeat r0 ,mel-ccl-low-table))
             (break))))
        ;; r0:r3=WSP
        ((r5 = 1)
         (if (r6 < 75)
             ((r6 += 1)
              (r4 = 4)
              (write-read-repeat r0))
           ((r6 = 1)
            (write ,(if output-crlf "=\r\n" "=\n"))
            (r4 = 5)
            (write-read-repeat r0))))
        ;; r0:r3=CR/CRLF
        ,(if input-crlf
             ;; r0:r3=CR
             `((if ((r6 > 73) & r5)
                   ((r6 = 0)
                    (r5 = 0)
                    (write ,(if output-crlf "=\r\n" "=\n"))))
               (break))
           ;; r0:r3=CRLF
           `(if r5
                ;; WSP ; r0:r3=CRLF
                ((r5 = 0)
                 (r6 = 0)
                 (write ,(if output-crlf "=\r\n" "=\n"))
                 ,@(if output-crlf '((write ?\r)) '())
                 (write-read-repeat r0))
              ;; noWSP ; r0:r3=CRLF
              ((r5 = 0)
	       (r6 = 0)
	       ,@(if output-crlf '((write ?\r)) '())
	       (write-read-repeat r0)))
	   )))
      ;; r0:r3={RAW,ENC,CR}
      (loop
       ,(funcall
         (lambda (after-cr after-raw-enc)
           (if input-crlf
               `(if (r0 == ?\r)
                    ,after-cr
                  ,after-raw-enc)
             after-raw-enc))
         ;; r0=\r:r3=CR
         `((r4 = 6)
           (read r0)
           ;; CR:r3=CR r0
           (if (r0 == ?\n)
               ;; CR:r3=CR r0=LF
               (if r5
                   ;; r5=WSP ; CR:r3=CR r0=LF
                   ((r6 = 0)
                    (r5 = 0)
                    (write ,(if output-crlf "=\r\n\r\n" "=\n\n"))
                    (r4 = 7)
                    (read r0)
                    (break))
                 ;; r5=noWSP ; CR:r3=CR r0=LF
                 ((r6 = 0)
                  (r5 = 0)
                  (write ,(if output-crlf "\r\n" "\n"))
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
                             (write ,(if output-crlf "=0D\r\n" "=0D\n"))
                             (r4 = 10)
                             (read r0)
                             (break))
                          ;; CR:r3=CR CR noLF
                          ((r6 = 6)
                           (r5 = 0)
                           (write ,(if output-crlf "=\r\n=0D=0D" "=\n=0D=0D"))
                           (break))))
                     ;; CR:r3=CR r0=noLFnorCR
                     ((r6 = 3)
                      (r5 = 0)
                      (write ,(if output-crlf "=\r\n=0D" "=\n=0D"))
                      (break)))
                 ((r6 = 3)
                  (r5 = 0)
                  (write ,(if output-crlf "=\r\n=0D" "=\n=0D"))
                  (break))))))
         (funcall
          (lambda (after-newline after-cr-nolf after-nonewline)
            (if input-crlf
                ;; r0:r3={RAW,ENC}
                `((r4 = 11)
                  (read r1)
                  ;; r0:r3={RAW,ENC} r1
                  (if (r1 == ?\r)
                      ;; r0:r3={RAW,ENC} r1=CR
                      ((r4 = 12)
                       (read r1)
                       ;; r0:r3={RAW,ENC} CR r1
                       (if (r1 == ?\n)
                           ;; r0:r3=RAW CR r1=LF
                           ,after-newline
                         ;; r0:r3=RAW CR r1=noLF
                         ,after-cr-nolf))
                    ;; r0:r3={RAW,ENC} r1:noCR
                    ,after-nonewline))
              ;; r0:r3={RAW,ENC}
              `((r4 = 11)
                (read r1)
                ;; r0:r3={RAW,ENC} r1
                (if (r1 == ?\n)
                    ;; r0:r3={RAW,ENC} r1=CRLF
                    ,after-newline
                  ;; r0:r3={RAW,ENC} r1:noCRLF
                  ,after-nonewline))))
          ;; r0:r3={RAW,ENC} CR r1=LF
          ;; r0:r3={RAW,ENC} r1=CRLF
          `((r6 = 0)
            (r5 = 0)
            (branch
             r3
             ;; r0:r3=RAW CR r1=LF
             ;; r0:r3=RAW r1=CRLF
             ((write r0)
              (write ,(if output-crlf "\r\n" "\n"))
              (r4 = 13)
              (read r0)
              (break))
             ;; r0:r3=ENC CR r1=LF
             ;; r0:r3=ENC r1=CRLF
             ((write ?=)
              (write r0 ,mel-ccl-high-table)
              (write r0 ,mel-ccl-low-table)
              (write ,(if output-crlf "\r\n" "\n"))
              (r4 = 14)
              (read r0)
              (break))))
          ;; r0:r3={RAW,ENC} CR r1=noLF
          `((branch
             r3
             ;; r0:r3=RAW CR r1:noLF
             ((r6 = 4)
              (r5 = 0)
              (write ,(if output-crlf "=\r\n" "=\n"))
              (write r0)
              (write "=0D")
              (r0 = r1)
              (break))
             ;; r0:r3=ENC CR r1:noLF
             ((r6 = 6)
              (r5 = 0)
              (write ,(if output-crlf "=\r\n=" "=\n="))
              (write r0 ,mel-ccl-high-table)
              (write r0 ,mel-ccl-low-table)
              (write "=0D")
              (r0 = r1)
              (break))))
          ;; r0:r3={RAW,ENC} r1:noCR
          ;; r0:r3={RAW,ENC} r1:noCRLF
          `((branch
             r3
             ;; r0:r3=RAW r1:noCR
             ;; r0:r3=RAW r1:noCRLF
             ((r6 = 1)
              (r5 = 0)
              (write ,(if output-crlf "=\r\n" "=\n"))
              (write r0)
              (r0 = r1)
              (break))
             ;; r0:r3=ENC r1:noCR
             ;; r0:r3=ENC r1:noCRLF
             ((r6 = 3)
              (r5 = 0)
              (write ,(if output-crlf "=\r\n=" "=\n="))
              (write r0 ,mel-ccl-high-table)
              (write r0 ,mel-ccl-low-table)
              (r0 = r1)
              (break)))))))
      (repeat)))
    ;; EOF
    (					;(write "[EOF:") (write r4 ,mel-ccl-high-table) (write r4 ,mel-ccl-low-table) (write "]")
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
      ((write ,(if output-crlf "=\r\n" "=\n")) (end))
      ;; 5: SOFTBREAK r0:r3=WSP ;
      ((write ,(if output-crlf "=\r\n" "=\n")) (end))
      ;; 6: ; r0=\r:r3=CR
      (if (r6 <= 73)
          ((write "=0D") (end))
	((write ,(if output-crlf "=\r\n=0D" "=\n=0D")) (end)))
      ;; 7: r5=WSP SOFTBREAK CR:r3=CR r0=LF ;
      (end)
      ;; 8: r5=noWSP CR:r3=CR r0=LF ;
      (end)
      ;; 9: (r6=73) ; CR:r3=CR r0=CR
      ((write ,(if output-crlf "=\r\n=0D=0D" "=\n=0D=0D")) (end))
      ;; 10: (r6=73) CR:r3=CR CR LF ;
      (end)
      ;; 11: ; r0:r3={RAW,ENC}
      (branch
       r3
       ((write r0) (end))
       ((write "=")
        (write r0 ,mel-ccl-high-table)
        (write r0 ,mel-ccl-low-table)
        (end)))
      ;; 12: ; r0:r3={RAW,ENC} r1=CR
      (branch
       r3
       ;; ; r0:r3=RAW r1=CR
       ((write ,(if output-crlf "=\r\n" "=\n"))
        (write r0)
        (write "=0D")
        (end))
       ;; ; r0:r3=ENC r1=CR
       ((write ,(if output-crlf "=\r\n=" "=\n="))
        (write r0 ,mel-ccl-high-table)
        (write r0 ,mel-ccl-low-table)
        (write "=0D")
        (end)))
      ;; 13: r0:r3=RAW CR LF ;
      ;; 13: r0:r3=RAW CRLF ;
      (end)
      ;; 14: r0:r3=ENC CR LF ;
      ;; 14: r0:r3=ENC CRLF ;
      (end)
      ))
    ))

(defun mel-ccl-decode-quoted-printable-generic (input-crlf output-crlf)
  `(1
    ((read r0)
     (loop
      (branch
       r0
       ,@(mapcar
          (lambda (r0)
            (let ((tmp (aref mel-ccl-qp-table r0)))
              (cond
               ((eq tmp 'raw) `(write-read-repeat r0))
               ((eq tmp 'wsp) (if (eq r0 (char-int ? ))
                                  `(r1 = 1)
                                `(r1 = 0)))
               ((eq tmp 'cr)
                (if input-crlf
                    ;; r0='\r'
                    `((read r0)
                      ;; '\r' r0
                      (if (r0 == ?\n)
                          ;; '\r' r0='\n'
                          ;; hard line break found.
                          ,(if output-crlf
                               '((write ?\r)
                                 (write-read-repeat r0))
                             '(write-read-repeat r0))
                        ;; '\r' r0:[^\n]
                        ;; invalid control character (bare CR) found.
                        ;; -> ignore it and rescan from r0.
                        (repeat)))
                  ;; r0='\r'
                  ;; invalid character (bare CR) found.
                  ;; -> ignore.
                  `((read r0)
                    (repeat))))
               ((eq tmp 'lf)
                (if input-crlf
                    ;; r0='\n'
                    ;; invalid character (bare LF) found.
                    ;; -> ignore.
                    `((read r0)
                      (repeat))
                  ;; r0='\r\n'
                  ;; hard line break found.
                  (if output-crlf
                      '((write ?\r)
                        (write-read-repeat r0))
                    '(write-read-repeat r0))))
               ((eq r0 (char-int ?=))
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
                         ((eq r0 (char-int ?\r))
                          (if input-crlf
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
                                  ;; output "=" and rescan from r0.
                                  ((write "=")
                                   (repeat))))
                            ;; '=' [\t ]* r0='\r'
                            ;; invalid input (bare CR found) -> 
                            ;; output "=" and rescan from next.
                            `((write ?=)
                              (read r0)
                              (repeat))))
                         ((eq r0 (char-int ?\n))
                          (if input-crlf
                              ;; '=' [\t ]* r0='\n'
                              ;; invalid input (bare LF found) -> 
                              ;; output "=" and rescan from next.
                              `((write ?=)
                                (read r0)
                                (repeat))
                            ;; '=' [\t ]* r0='\r\n'
                            ;; soft line break found.
                            `((read r0)
                              (repeat))))
                         ((setq tmp (nth r0 mel-ccl-256-to-16-table))
                          ;; '=' [\t ]* r0:[0-9A-F]
                          ;; upper nibble of hexadecimal digit found.
                          `((r1 = r0)
			    (r0 = ,tmp)))
                         (t
                          ;; '=' [\t ]* r0:[^\r0-9A-F]
                          ;; invalid input ->
                          ;; output "=" and rescan from r0.
                          `((write ?=)
                            (repeat)))))
                      mel-ccl-256-table))
                  ;; '=' [\t ]* r1:r0:[0-9A-F]
                  (read-branch
                   r2
                   ,@(mapcar
                      (lambda (r2)
                        (if (setq tmp (nth r2 mel-ccl-256-to-16-table))
                            ;; '=' [\t ]* r1:r0:[0-9A-F] r2:[0-9A-F]
                            `(write-read-repeat
                              r0
                              ,(vconcat
                                (mapcar
                                 (lambda (r0)
                                   (logior (lsh r0 4) tmp))
                                 mel-ccl-16-table)))
                          ;; '=' [\t ]* r1:r0:[0-9A-F] r2:[^0-9A-F]
                          ;; invalid input
                          `(r3 = 0)	; nop
                          ))
                      mel-ccl-256-table))
                  ;; '=' [\t ]* r1:r0:[0-9A-F] r2:[^0-9A-F]
                  ;; invalid input ->
                  ;; output "=" with hex digit and rescan from r2.
                  (write ?=)
                  (r0 = r2)
                  (write-repeat r1)))
               (t
                ;; r0:[^\t\r -~]
                ;; invalid character found.
                ;; -> ignore.
                `((read r0)
                  (repeat))))))
          mel-ccl-256-table))
      ;; r1[0]:[\t ]
      (loop
       ,@(apply
	  'append
	  (mapcar
	   (lambda (regnum)
	     (let ((reg (aref [r1 r2 r3 r4 r5] regnum)))
	       (apply
		'append
		(mapcar
		 (lambda (bit)
		   (if (= bit 0)
		       (if (= regnum 0)
			   nil
			 `((read r0)
			   (if (r0 == ?\t)
			       (,reg = 0)
			     (if (r0 == ?\ )
				 (,reg = 1)
			       ((r6 = ,(+ (* regnum 28) bit))
				(break))))))
		     `((read r0)
		       (if (r0 == ?\ )
			   (,reg |= ,(lsh 1 bit))
			 (if (r0 != ?\t)
			     ((r6 = ,(+ (* regnum 28) bit))
			      (break)))))))
		 mel-ccl-28-table))))
	   '(0 1 2 3 4)))
       ;; white space buffer exhaust.
       ;; error: line length limit (76bytes) violation.
       ;; -> ignore these white spaces.
       (repeat))
      ,(if input-crlf
           `(if (r0 == ?\r)
                ((read r0)
                 (if (r0 == ?\n)
                     ;; trailing white spaces found.
                     ;; -> ignore these white spacs.
                     ((write ,(if output-crlf "\r\n" "\n"))
                      (read r0)
                      (repeat))
                   ;; [\t ]* \r r0:[^\n]
                   ;; error: bare CR found.
                   ;; -> output white spaces and ignore bare CR.
                   ))
              ;; [\t ]* r0:[^\r]
              ;; middle white spaces found.
              )
         `(if (r0 == ?\n)
              ;; trailing white spaces found.
              ;; -> ignore these white spacs.
              ((write ,(if output-crlf "\r\n" "\n"))
               (read r0)
               (repeat))
            ;; [\t ]* r0:[^\n]
            ;; middle white spaces found.
            ))
      ,@(apply
	 'append
	 (mapcar
	  (lambda (regnum)
	    (let ((reg (aref [r1 r2 r3 r4 r5] regnum)))
	      (apply
	       'append
	       (mapcar
		(lambda (bit)
		  `((if (,reg & ,(lsh 1 bit))
			(write ?\ )
		      (write ?\t))
		    (if (r6 == ,(+ (* regnum 28) bit 1))
			(repeat))))
		mel-ccl-28-table))))
	  '(0 1 2 3 4)))
      (repeat)
      ))))

)

(define-ccl-program mel-ccl-encode-quoted-printable-crlf-crlf
  (mel-ccl-encode-quoted-printable-generic t t))

(define-ccl-program mel-ccl-encode-quoted-printable-crlf-lf
  (mel-ccl-encode-quoted-printable-generic t nil))

(define-ccl-program mel-ccl-encode-quoted-printable-lf-crlf
  (mel-ccl-encode-quoted-printable-generic nil t))

(define-ccl-program mel-ccl-encode-quoted-printable-lf-lf
  (mel-ccl-encode-quoted-printable-generic nil nil))

(define-ccl-program mel-ccl-decode-quoted-printable-crlf-crlf
  (mel-ccl-decode-quoted-printable-generic t t))

(define-ccl-program mel-ccl-decode-quoted-printable-crlf-lf
  (mel-ccl-decode-quoted-printable-generic t nil))

(define-ccl-program mel-ccl-decode-quoted-printable-lf-crlf
  (mel-ccl-decode-quoted-printable-generic nil t))

(define-ccl-program mel-ccl-decode-quoted-printable-lf-lf
  (mel-ccl-decode-quoted-printable-generic nil nil))


;;; @ coding system
;;;

(make-ccl-coding-system
 'mel-ccl-uq-rev ?Q "MIME Q-encoding in unstructured field (reversed)"
 'mel-ccl-encode-uq 'mel-ccl-decode-q)

(make-ccl-coding-system
 'mel-ccl-cq-rev ?Q "MIME Q-encoding in comment (reversed)"
 'mel-ccl-encode-cq 'mel-ccl-decode-q)

(make-ccl-coding-system
 'mel-ccl-pq-rev ?Q "MIME Q-encoding in phrase (reversed)"
 'mel-ccl-encode-pq 'mel-ccl-decode-q)

(make-ccl-coding-system
 'mel-ccl-b-rev ?B "MIME B-encoding (reversed)"
 'mel-ccl-encode-b 'mel-ccl-decode-b)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-crlf-crlf-rev
 ?Q "MIME Quoted-Printable-encoding (reversed)"
 'mel-ccl-encode-quoted-printable-crlf-crlf
 'mel-ccl-decode-quoted-printable-crlf-crlf)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-lf-crlf-rev
 ?Q "MIME Quoted-Printable-encoding (LF encoding) (reversed)"
 'mel-ccl-encode-quoted-printable-crlf-lf
 'mel-ccl-decode-quoted-printable-lf-crlf)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-crlf-lf-rev
 ?Q "MIME Quoted-Printable-encoding (LF internal) (reversed)"
 'mel-ccl-encode-quoted-printable-lf-crlf
 'mel-ccl-decode-quoted-printable-crlf-lf)

(make-ccl-coding-system
 'mel-ccl-quoted-printable-lf-lf-rev
 ?Q "MIME Quoted-Printable-encoding (LF encoding) (LF internal) (reversed)"
 'mel-ccl-encode-quoted-printable-lf-lf
 'mel-ccl-decode-quoted-printable-lf-lf)

(make-ccl-coding-system
 'mel-ccl-base64-crlf-rev
 ?B "MIME Base64-encoding (reversed)"
 'mel-ccl-encode-base64-crlf-crlf
 'mel-ccl-decode-b)

(make-ccl-coding-system
 'mel-ccl-base64-lf-rev
 ?B "MIME Base64-encoding (LF encoding) (reversed)"
 'mel-ccl-encode-base64-crlf-lf
 'mel-ccl-decode-b)


;;; @ B
;;;

(unless (and (boundp 'ccl-encoder-eof-block-is-broken)
	     ccl-encoder-eof-block-is-broken)

  (defun base64-ccl-encode-string (string)
    "Encode STRING with base64 encoding."
    (decode-coding-string string 'mel-ccl-base64-lf-rev))

  (defun base64-ccl-encode-region (start end)
    "Encode region from START to END with base64 encoding."
    (interactive "r")
    (decode-coding-region start end 'mel-ccl-base64-lf-rev))

  (defun base64-ccl-insert-encoded-file (filename)
    "Encode contents of file FILENAME to base64, and insert the result."
    (interactive (list (read-file-name "Insert encoded file: ")))
    (let ((coding-system-for-read 'mel-ccl-b-rev))
      (insert-file-contents filename)))

  )

(defun base64-ccl-decode-string (string)
  "Decode base64 encoded STRING"
  (encode-coding-string string 'mel-ccl-b-rev))

(defun base64-ccl-decode-region (start end)
  "Decode base64 encoded the region from START to END."
  (interactive "r")
  (encode-coding-region start end 'mel-ccl-b-rev))

(defun base64-ccl-write-decoded-region (start end filename)
  "Decode the region from START to END and write out to FILENAME."
  (interactive
    (list (region-beginning) (region-end)
          (read-file-name "Write decoded region to file: ")))
  (let ((coding-system-for-write 'mel-ccl-b-rev))
    (write-region start end filename)))


;;; @ quoted-printable
;;;

(unless (and (boundp 'ccl-encoder-eof-block-is-broken)
	     ccl-encoder-eof-block-is-broken)

  (defun quoted-printable-ccl-encode-string (string)
    "Encode STRING with quoted-printable encoding."
    (decode-coding-string
     string
     'mel-ccl-quoted-printable-lf-lf-rev))

  (defun quoted-printable-ccl-encode-region (start end)
    "Encode the region from START to END with quoted-printable
encoding."
    (interactive "r")
    (decode-coding-region start end 'mel-ccl-quoted-printable-lf-lf-rev))

  (defun quoted-printable-ccl-insert-encoded-file (filename)
    "Encode contents of the file named as FILENAME, and insert it."
    (interactive (list (read-file-name "Insert encoded file: ")))
    (let ((coding-system-for-read 'mel-ccl-quoted-printable-lf-lf-rev))
      (insert-file-contents filename)))

  )

(defun quoted-printable-ccl-decode-string (string)
  "Decode quoted-printable encoded STRING."
  (encode-coding-string
   string
   'mel-ccl-quoted-printable-lf-lf-rev))

(defun quoted-printable-ccl-decode-region (start end)
  "Decode the region from START to END with quoted-printable
encoding."
  (interactive "r")
  (encode-coding-region start end 'mel-ccl-quoted-printable-lf-lf-rev))

(defun quoted-printable-ccl-write-decoded-region
  (start end filename)
  "Decode quoted-printable encoded current region and write out to FILENAME."
  (interactive
   (list (region-beginning) (region-end)
         (read-file-name "Write decoded region to file: ")))
  (let ((coding-system-for-write 'mel-ccl-quoted-printable-lf-lf-rev))
    (write-region start end filename)))


;;; @ Q
;;;

(defun q-encoding-ccl-encode-string (string &optional mode)
  "Encode STRING to Q-encoding of encoded-word, and return the result.
MODE allows `text', `comment', `phrase' or nil.  Default value is
`phrase'."
  (decode-coding-string
   string
   (cond
    ((eq mode 'text) 'mel-ccl-uq-rev)
    ((eq mode 'comment) 'mel-ccl-cq-rev)
    (t 'mel-ccl-pq-rev))))

(defun q-encoding-ccl-decode-string (string)
  "Decode Q encoded STRING and return the result."
  (encode-coding-string
   string
   'mel-ccl-uq-rev))

(unless running-xemacs
  (defun q-encoding-ccl-encoded-length (string &optional mode)
    (let ((status [nil nil nil nil nil nil nil nil nil]))
      (fillarray status nil)
      (ccl-execute-on-string
       (cond
	((eq mode 'text) 'mel-ccl-count-uq)
	((eq mode 'comment) 'mel-ccl-count-cq)
	(t 'mel-ccl-count-pq))
       status
       string)
      (aref status 0)))
  )

;;; @ end
;;;

(provide 'mel-ccl)

'(
(let ((str0 "a\f \t\r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\f\r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\f\r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\f\r
                                                                          \r
                                                                           \r
                                                                            \r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \r
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa \r
bbb \r
bbbb\r
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\rccc\r
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\r\r\nccc\r
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\r\rccc\r
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\rccc\r
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd\r\neee\r
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd\reee\r
ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddeee\r
")
      str1 encoded decoded)
  (setq str1 (ew-crlf-to-lf str0)
	encoded
	(list
	 (decode-coding-string
	  str0
	  'mel-ccl-quoted-printable-crlf-crlf-rev)
	 (decode-coding-string
	  str0
	  'mel-ccl-quoted-printable-lf-crlf-rev)
	 (decode-coding-string
	  str1
	  'mel-ccl-quoted-printable-crlf-lf-rev)
	 (decode-coding-string
	  str1
	  'mel-ccl-quoted-printable-lf-lf-rev))
	decoded
	(list
	 (encode-coding-string
	  (nth 0 encoded)
	  'mel-ccl-quoted-printable-crlf-crlf-rev)
	 (encode-coding-string
	  (nth 1 encoded)
	  'mel-ccl-quoted-printable-lf-crlf-rev)
	 (encode-coding-string
	  (nth 2 encoded)
	  'mel-ccl-quoted-printable-crlf-lf-rev)
	 (encode-coding-string
	  (nth 3 encoded)
	  'mel-ccl-quoted-printable-lf-lf-rev)))
  (list
   (string= str0 (nth 0 decoded))
   (string= str0 (nth 1 decoded))
   (string= str1 (nth 2 decoded))
   (string= str1 (nth 3 decoded))))

;; for xemacs
(defun make-ccl-coding-system (name mnemonic doc-string decoder encoder)
  (make-coding-system
   name 'ccl doc-string
   (list 'mnemonic (char-to-string mnemonic)
         'decode (symbol-value decoder)
         'encode (symbol-value encoder))))

)
