(require 'emu)
(require 'rx)
(require 'automata)
(provide 'lex)

(put 'lex-scan-multibyte 'lisp-indent-function 3)
(put 'lex-scan-unibyte 'lisp-indent-function 3)

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
     (catch ',lex-escape-tag
       (automata
	,lex-pc-var 0
	,@(mapcar
	   (lambda (s) (lex-gen-state s cs read-macro save-macro))
	   states)))
     (automata-branch
      ,lex-act-var ,(apply 'natset-single (mapcar 'car acts)) automata-never-fail
      ,@(mapcar
	 (lambda (act) `(,(natset-single (car act)) nil ,@(cdr act)))
	 acts))))

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

;;; internal macros

(defmacro lex-match (id)
  `(setq ,lex-act-var ',id))
(defmacro lex-fail ()
  `(throw ',lex-escape-tag nil))

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

)
