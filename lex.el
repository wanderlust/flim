(require 'emu)
(require 'rx)
(require 'automata)
(provide 'lex)

(put 'lex-scan-multibyte 'lisp-indent-function 3)
(put 'lex-scan-unibyte 'lisp-indent-function 3)

;;;

(eval-and-compile

;; As a result of profiling, CCL is slower than Emacs-Lisp, sigh...
(setq lex-ccl-execute nil)

(defvar lex-ccl-execute
  (eval-when-compile
    (or (when (fboundp 'ccl-execute-on-substring) 'ccl-execute-on-substring)
	(when (fboundp 'ccl-execute-on-string) 'ccl-execute-on-string))))

(defvar lex-ccl-use-name
  (eval-when-compile
    (and
     lex-ccl-execute
     (condition-case nil
	 (progn
	   (register-ccl-program 'test-ccl (ccl-compile '(0 (r0 = 1))))
	   (ccl-execute-with-args 'test-ccl)
	   t)
       (error nil)))))

(when lex-ccl-execute
  (require 'ccl))
)

;;; user interface macro

;;; multibyte

(defvar lex-scan-multibyte-str-var (make-symbol "str"))
(defvar lex-scan-multibyte-ptr-var (make-symbol "ptr"))
(defvar lex-scan-multibyte-end-var (make-symbol "end"))
(defvar lex-scan-multibyte-mch-var (make-symbol "mch"))

(defmacro lex-scan-multibyte-read (pc)
  `(if (< ,lex-scan-multibyte-ptr-var ,lex-scan-multibyte-end-var)
       (setq ,pc (sref ,lex-scan-multibyte-str-var ,lex-scan-multibyte-ptr-var)
	     ,lex-scan-multibyte-ptr-var (char-next-index ,pc ,lex-scan-multibyte-ptr-var)
	     ,pc (char-int ,pc))
     (lex-fail)))

(defmacro lex-scan-multibyte-save ()
  `(setq ,lex-scan-multibyte-mch-var ,lex-scan-multibyte-ptr-var))

(defmacro lex-scan-multibyte (str start end &rest clauses)
  (if (not start) (setq start 0))
  (if (not end) (setq end `(length ,lex-scan-multibyte-str-var)))
  (let ((id 1) (rx ()) (acts ()) tmp code
	(restore-code (if (symbolp start) `(setq ,start ,lex-scan-multibyte-mch-var))))
    (while (consp clauses)
      (setq rx (cons (rx-con (caar clauses) (rx-act id)) rx)
	    acts (cons (cons id (cons restore-code (cdar clauses))) acts)
	    id (1+ id)
	    clauses (cdr clauses)))
    (setq rx (rx-alt rx)
	  tmp (rx-categolize-char (rx-desugar rx)))
    `(let* ((,lex-scan-multibyte-str-var ,str)
	    (,lex-scan-multibyte-ptr-var ,start)
	    (,lex-scan-multibyte-end-var ,end)
	    ,lex-scan-multibyte-mch-var)
       ,(lex-gen-machine (lex-automata (car tmp)) (cdr tmp) acts 'lex-scan-multibyte-read 'lex-scan-multibyte-save))))

;;; unibyte

(defvar lex-scan-unibyte-str-var (make-symbol "str"))
(defvar lex-scan-unibyte-ptr-var (make-symbol "ptr"))
(defvar lex-scan-unibyte-end-var (make-symbol "end"))
(defvar lex-scan-unibyte-mch-var (make-symbol "mch"))

(defmacro lex-scan-unibyte-read (pc)
  `(if (< ,lex-scan-unibyte-ptr-var ,lex-scan-unibyte-end-var)
       (setq ,pc (aref ,lex-scan-unibyte-str-var ,lex-scan-unibyte-ptr-var)
	     ,lex-scan-unibyte-ptr-var (1+ ,lex-scan-unibyte-ptr-var)
	     ,pc (char-int ,pc))
     (lex-fail)))

(defmacro lex-scan-unibyte-save ()
  `(setq ,lex-scan-unibyte-mch-var ,lex-scan-unibyte-ptr-var))

(defmacro lex-scan-unibyte (str start end &rest clauses)
  (if (not start) (setq start 0))
  (if (not end) (setq end `(length ,lex-scan-unibyte-str-var)))
  (let ((id 1) (rx ()) (acts ()) tmp code
	(restore-code (if (symbolp start) `(setq ,start ,lex-scan-unibyte-mch-var))))
    (while (consp clauses)
      (setq rx (cons (rx-con (caar clauses) (rx-act id)) rx)
	    acts (cons (cons id (cons restore-code (cdar clauses))) acts)
	    id (1+ id)
	    clauses (cdr clauses)))
    (setq rx (rx-alt rx)
	  tmp (rx-categolize-char (rx-desugar rx)))
    `(let* ((,lex-scan-unibyte-str-var ,str)
	    (,lex-scan-unibyte-ptr-var ,start)
	    (,lex-scan-unibyte-end-var ,end)
	    ,lex-scan-unibyte-mch-var)
       ,(lex-gen-machine (lex-automata (car tmp)) (cdr tmp) acts 'lex-scan-unibyte-read 'lex-scan-unibyte-save))))

;;; automata generation

(defun lex-automata (rx)
  (let* ((rx (rx-simplify rx))
	 (stack (list rx))		; list of rx
	 (table (list (rx-cons* rx 0 (lex-make-box (list 'd1 'd2)))))
					; list of (rx id . box-for-reverse-links)
	 (states ())			; list of (id act trans . box-for-reverse-links)
					;   where trans = list of (pc id . box-for-reverse-links)
	 (next-id 1)
	 tbl-ent box id pcs act pc trans  rx-stepped p)
    (while (consp stack)
      (setq rx (car stack)
	    stack (cdr stack)
	    tbl-ent (assoc rx table)
	    id (cadr tbl-ent)
	    box (cddr tbl-ent)
	    pcs (rx-head-pcs rx)
	    act (rx-head-act rx)
	    trans ())
      (while (consp pcs)
	(setq pc (car pcs)
	      pcs (cdr pcs)
	      rx-stepped (rx-step rx pc)
	      p (assoc rx-stepped table))
	(if p
	    (progn
	      (setq trans (cons (cons pc (cdr p)) trans))
	      (lex-add-box (cddr p) id))
	  (setq p (rx-cons* rx-stepped next-id (lex-make-box (list id)))
		trans (cons (cons pc (cdr p)) trans)
		table (cons p table)
		next-id (1+ next-id)
		stack (cons rx-stepped stack))))
      (setq states
	    (cons (rx-cons* id act trans box)
		  states)))
    states))

;;; automata coding

(defvar lex-pc-var (make-symbol "pc"))
(defvar lex-act-var (make-symbol "act"))
(defvar lex-escape-tag (make-symbol "esc"))

(defun lex-gen-machine (states cs acts read-macro save-macro)
  `(let (,lex-pc-var ,lex-act-var)
     ,(if (and lex-ccl-execute
	       (eq read-macro 'lex-scan-unibyte-read)
	       (eq save-macro 'lex-scan-unibyte-save))
	  (lex-gen-ccl-unibyte-automata states cs)
	(lex-gen-automata states cs read-macro save-macro))
     ,(lex-gen-action acts)))

(defun lex-gen-automata (states cs read-macro save-macro)
  `(catch ',lex-escape-tag
     (automata
      ,lex-pc-var 0
      ,@(mapcar
	 (lambda (s) (lex-gen-state s cs read-macro save-macro))
	 states))))

(defun lex-gen-state (s cs read-macro save-macro)
  (let ((id (nth 0 s))
	(act (nth 1 s))
	(trans (nth 2 s)))
    `(,id
      (progn
	,@(if act
	      `((lex-match ,(cdr act)) (,save-macro))
	    ())
	,@(if (consp trans) `((,read-macro ,lex-pc-var))))
      (lex-fail)
      ,@(mapcar
	 (lambda (tr) `(,(let ((l (member (car tr) cs)))
			   (if (null (cdr l))
			       (natset-seg (car l))
			     (natset-seg (car l) (1- (cadr l)))))
			,(cadr tr)))
	 trans))))

(defun lex-gen-action (acts)
  `(automata-branch
    ,lex-act-var ,(apply 'natset-single (mapcar 'car acts)) automata-never-fail
    ,@(mapcar
       (lambda (act) `(,(natset-single (car act)) nil ,@(cdr act)))
       acts)))

;;; CCL version automata generation

(defun lex-gen-ccl-unibyte-automata (states cs)
  ;; read-macro is lex-scan-unibyte-read
  ;; save-macro is lex-scan-unibyte-save
  (let ((name (make-symbol "ccl-prog-name"))
	(frag-vector (make-vector 1 nil))
		       )
    `(let ((frag ,frag-vector)
	   (status [nil nil nil nil nil nil nil nil nil])
	   (prog (eval-when-compile
		   (ccl-compile
		    ',(lex-gen-ccl-unibyte-automata-program states cs)))))
       (unless (aref frag 0)
	 (register-ccl-program
	  ',name prog)
	 (aset frag 0 t))
       (aset status 0 nil)                       ; r0: pc
       (aset status 1 0)                         ; r1: state
       (aset status 2 ,lex-scan-unibyte-ptr-var) ; r2: ptr
       (aset status 3 ,lex-scan-unibyte-ptr-var) ; r3: start
       (aset status 4 ,lex-scan-unibyte-end-var) ; r4: end
       (aset status 5 nil)                       ; r5: mch
       (aset status 6 0)                         ; r6: act
       (aset status 7 nil)                       ; r7
       (aset status 8 nil)                       ; ic
       ,(if (eval-when-compile (eq lex-ccl-execute 'ccl-execute-on-string))
	    `(ccl-execute-on-string
	      ,(if (eval-when-compile lex-ccl-use-name) `',name `prog)
	      status
	      ,lex-scan-unibyte-str-var)
	  `(ccl-execute-on-substring
	    ,(if (eval-when-compile lex-ccl-use-name) `',name `prog)
	    status
	    ,lex-scan-unibyte-str-var
	    ,lex-scan-unibyte-ptr-var
	    ,lex-scan-unibyte-end-var))
       (setq ,lex-scan-unibyte-ptr-var (aref status 2))
       (when (< 0 (aref status 6))
	 (setq ,lex-act-var (aref status 6)
	       ,lex-scan-unibyte-mch-var (aref status 5))))))

(defun lex-gen-ccl-unibyte-automata-program (states cs)
  `(0
    (,@(eval-when-compile
	 (when (eq lex-ccl-execute 'ccl-execute-on-string)
	   '((loop
	      (if (r3 > 0)
		  ((r3 -= 1)
		   (read r0)
		   (repeat))
		(break))))))
     (loop
      (branch r1
        ,@(mapcar
	   (lambda (s) (lex-gen-ccl-unibyte-automata-state 
			(nth 0 s) (cdr (nth 1 s)) (nth 2 s)
			cs))
	   (sort states
		 (lambda (a b) (< (car a) (car b))))))))))

(defun lex-gen-ccl-unibyte-automata-state (id act trans cs)
  `(,@(when act
	`((r5 = r2)
	  (r6 = ,act)))
    ,@(if (consp trans)
	  `((if (r4 <= r2)
		(end)
	      ((read r0)
	       (r2 += 1)
	       ,(apply
		 'natset-gen-ccl-branch
		 'r0
		 '(end)
		 (mapcar
		  (lambda (tr) (cons
				(let ((l (member (car tr) cs)))
				  (if (null (cdr l))
				      (natset-seg (car l))
				    (natset-seg (car l) (1- (cadr l)))))
				`((r1 = ,(cadr tr))
				  (repeat))))
		  trans))
	       (repeat))))
	'((end)))))

;;; internal macros

(defmacro lex-match (id)
  `(setq ,lex-act-var ',id))
(defmacro lex-fail ()
  `(throw ',lex-escape-tag nil))

;;; utilities

(defun lex-make-box (val)
  (list val))
(defalias 'lex-box-ref 'car)

(defun lex-add-box (box val)
  (if (not (member val (car box)))
      (setcar box (cons val (car box)))))

;;; testing
'(
  
  (mapcar (lambda (v) (set v (intern (symbol-name (symbol-value v)))))
	  '(lex-pc-var
            lex-act-var
            lex-escape-tag
            lex-scan-multibyte-str-var
            lex-scan-multibyte-ptr-var
            lex-scan-multibyte-end-var
            lex-scan-multibyte-mch-var
            lex-scan-unibyte-str-var
            lex-scan-unibyte-ptr-var
            lex-scan-unibyte-end-var
            lex-scan-unibyte-mch-var))

  (lex-scan-multibyte
   "aaa" 0 3
   (?a 'a))

(let* ((str "abcdef\ndeefx\r\n jfdks\r")
       (p 15))
  (cons
   (lex-scan-unibyte str p nil
     (()
      'error)
     (((* [^ "\r\n"])
       (* (+ ?\r) [^ "\r\n"] (* [^ "\r"]))
       (* ?\r)
       (?\r ?\n [" \t"]))
      'line-fold)
     (((* [^ "\r\n"])
       (* (+ ?\r) [^ "\r\n"] (* [^ "\r"]))
       (* ?\r)
       (?\r ?\n))
      'line-crlf)
     (((* [^ "\r\n"])
       (* (+ ?\r) [^ "\r\n"] (* [^ "\r"]))
       (* ?\r))
      'line))
   p))

(ew-crlf-line-convert "abcdef\ndeefx\r\n jfdks\r"
  (lambda (a) (format "[L:%s]" a))
  (lambda (a) (format "[F:%s]" a))
  (lambda (a) (format "[N:%s]" a)))


)

