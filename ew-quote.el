;;; quoted encoded word library

(require 'ew-var)
(require 'ew-util)
(require 'ew-line)
(provide 'ew-quote)

;; This library provides functions operating strings embedding
;; unencodable encoded words.

;;;type   embedded-pattern     denoted-pattern
;; Type-0 =?(QQQ)*A?B?C?=      =?(Q)*A?B?C?=
;; Type-1 =?(QQQ)*QA?B?C?=     Decode =?(Q)*A?B?C?= as encoded-word
;; Type-2 =?(QQQ)*QQA?B?C?=    =?(Q)*A?B?C?

;; Q : quoting character '+'.
;; A : token. But it does not start with quoting character.
;; B : token.
;; C : encoded-text.

(eval-and-compile
  (defconst ew-quoting-char ?+))
(defconst ew-quoting-chars-regexp
  (eval-when-compile
    (concat (regexp-quote (char-to-string ew-quoting-char)) "*")))

(defconst ew-type2-regexp
  (eval-when-compile
    (require 'ew-var)
    (concat (regexp-quote "=?")
            "\\(" ew-token-regexp "\\)"
            (regexp-quote "?")
            "\\(" ew-token-regexp "\\)"
            (regexp-quote "?")
            "\\(" ew-encoded-text-regexp "\\)"
            (regexp-quote "?")
            "\\'")))

;;;

(defun ew-quoting-char-seq (num)
  (make-string num ew-quoting-char))

(defun ew-quote (str)
  (ew-quote-concat str))

(defun ew-concat (&rest args)
  (apply 'ew-quote-concat (mapcar 'list args)))

(defun ew-quote-concat (&rest args)
  (let (result raws tmp)
    (while args
      (setq tmp (car args))
      (cond
       ((stringp tmp)
	;; raw string
	(setq raws (cons tmp raws)))
       ((listp tmp)
	;; quoted encoded word embedding strings
	(let (str start eword-start charset-start quoting-end eword-end l q r)
	  (while tmp
	    (setq str (car tmp)
		  start 0)
	    (while (string-match ew-encoded-word-regexp str start)
	      (setq eword-start (match-beginning 0)
		    charset-start (match-beginning 1)
		    eword-end (match-end 0))
	      (string-match ew-quoting-chars-regexp str charset-start)
	      (setq quoting-end (match-end 0)
		    l (- quoting-end charset-start)
		    q (/ l 3)
		    r (% l 3))
	      (cond
	       ((= r 0) ; Type-0
		(setq raws
		      (ew-rcons*
		       raws
		       (substring str start charset-start)
		       (ew-quoting-char-seq q)
		       (substring str quoting-end eword-end))))
	       ((= r 2) ; Type-2
		(setq raws
		      (ew-rcons*
		       raws
		       (substring str start charset-start)
		       (ew-quoting-char-seq q)
		       (substring str quoting-end (1- eword-end)))))
	       ((= r 1) ; Type-1
		(setq raws
		      (ew-rcons*
		       raws
		       (substring str start eword-start))
		      result
		      (ew-rcons*
		       result
		       (ew-quote-sole (apply 'concat (nreverse raws)) t)
		       (substring str eword-start eword-end))
		      raws ())))
	      (setq start eword-end))
	    (setq raws (ew-rcons* raws (substring str start))
		  tmp (cdr tmp)))))
       (t
	(error "ew-quote-concat: %s" tmp)))
      (setq args (cdr args)))
    (setq result
	  (ew-rcons*
	   result
	   (ew-quote-sole (apply 'concat (nreverse raws)) nil)))
    (apply 'concat (nreverse result))))

(defun ew-quote-sole (str gen-type2)
  (let (result (start 0) charset-start quoting-end eword-end l)
    (while (string-match ew-encoded-word-regexp str start)
      (setq charset-start (match-beginning 1)
	    eword-end (match-end 0))
      (string-match ew-quoting-chars-regexp str charset-start)
      (setq quoting-end (match-end 0)
	    l (* (- quoting-end charset-start) 3)
	    result
	    (ew-rcons*
	     result
	     (substring str start charset-start)
	     (ew-quoting-char-seq l)
	     (substring str quoting-end eword-end))
	    start eword-end))
    (if (and gen-type2
	     (string-match ew-type2-regexp str start))
	(progn
	  (setq charset-start (match-beginning 1)
		eword-end (match-end 0))
	  (string-match ew-quoting-chars-regexp str charset-start)
	  (setq quoting-end (match-end 0)
		l (* (- quoting-end charset-start) 3)
		result
		(ew-rcons*
		 result
		 (substring str start charset-start)
		 (ew-quoting-char-seq (+ l 2))
		 (substring str quoting-end eword-end)
		 "=")))
      (setq result (ew-rcons* result (substring str start))))
    (apply 'concat (nreverse result))))

(defun ew-quote-eword (charset encoding encoded-text)
  (string-match ew-quoting-chars-regexp charset)
  (concat
   "=?+" ; Type-1
   (ew-quoting-char-seq (* (- (match-end 0) (match-beginning 0)) 3))
   (substring charset (match-end 0))
   "?"
   encoding
   "?"
   encoded-text
   "?="))

(defun ew-encode-crlf (str)
  (if ew-remove-bare-crlf
      (ew-crlf-line-convert str nil nil (lambda (nl) ""))
    (let ((sstart 0)
	  (mstart 0)
	  (end (length str)) result ms me)
      (while (string-match "\\(\r\n\\)+" str mstart)
	(setq ms (match-beginning 0)
	      me (match-end 0))
	(setq mstart me)
	(when (and (< me end)
		   (member (aref str me) '(?\t ?\ )))
	  (setq me (- me 2)))
	(when (< ms me)
	  (setq result (ew-rcons* result
				  (substring str sstart ms)
				  "=?+US-ASCII?Q?")
		sstart me)
	  (while (< ms me)
	    (setq result (ew-rcons* result "=0D=0A")
		  ms (+ ms 2)))
	  (setq result (ew-rcons* result "?="))))
      (when (< sstart end)
	(setq result (ew-rcons* result
				(substring str sstart))))
      (apply 'concat (nreverse result)))))

'(
(ew-quote-concat "aaa=?A?B?C?=ccc") ;"aaa=?A?B?C?=ccc"
(ew-quote-concat "aaa=?+A?B?C?=ccc") ;"aaa=?+++A?B?C?=ccc"
(ew-quote-concat '("aaa=?A?B?C?=ccc")) ;"aaa=?A?B?C?=ccc"
(ew-quote-concat '("aaa=?+++A?B?C?=ccc")) ;"aaa=?+++A?B?C?=ccc"
(ew-quote-concat "aaa=?+A?B" "?C?=ccc") ;"aaa=?+++A?B?C?=ccc"
(ew-quote-concat "a=?+A?B?C?" '("=?+US-ASCII?Q?z?=")) ;"a=?+++++A?B?C?==?+US-ASCII?Q?z?="
(ew-quote-concat "a=?+A?B?C?=?+D?E?F?" '("=?+US-ASCII?Q?z?=")) ;"a=?+++A?B?C?=?+D?E?F?=?+US-ASCII?Q?z?="
(ew-quote-concat "a=?+A?B?C?=?+D?E?F?=?+G?H?I?" '("=?+US-ASCII?Q?z?=")) ;"a=?+++A?B?C?=?+D?E?F?=?+++++G?H?I?==?+US-ASCII?Q?z?="
(ew-quote-concat '("a=?++A?B?C?==?+++A?B?C?=c")) ;"a=?A?B?C?=?+A?B?C?=c"
)
