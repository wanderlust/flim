;;; regular expression

;;; core
;; rx ::= []                  {}
;;      | ()                  {""}
;;      | (* . rx)            closure
;;      | (| . rxs)           alternative
;;      | (rx . rx)           concatination
;;      | (cc c1 c2 c3 ...)   character class (natset)
;;; sugar
;;      | (+ . rx)            positive closure
;;      | "..."               string
;;      | c                   character
;;      | (non-ascii)         (cc 128)
;;      | [ range ... ]
;;      | [ ^ range ... ]
;;; internal
;;      | pc                  primitive character class
;;      | (act . int)         action

;; range ::= c
;;        | "..."
;;        | (c1 c2)           [c1 c2]
;;        | (c1 . c2)         [c1 c2)
;;        | (c)               [c1 inf)
;;        | non-ascii

(require 'natset)
(require 'automata)
(provide 'rx)

(defun rx-empty-p (rx) (equal rx []))
(defun rx-null-p (rx) (equal rx ()))
(defun rx-act-p (rx) (and (consp rx) (eq (car rx) 'act)))
(defun rx-cc-p (rx) (and (consp rx) (eq (car rx) 'cc)))
(defalias 'rx-pc-p 'integerp)

(defun rx-clo-p (rx) (and (consp rx) (eq (car rx) '*)))
(defun rx-alt-p (rx) (and (consp rx) (eq (car rx) '|)))
(defun rx-con-p (rx) (and (consp rx) (or (null (car rx)) (not (symbolp (car rx))))))

(defun rx-clo (rx)
  (cond
   ((rx-empty-p rx) ())
   ((rx-null-p rx) rx)
   ((rx-act-p rx) rx)
   ((rx-clo-p rx) rx)
   (t (cons '* rx))))
(defun rx-alt (rxs)
  (cond
   ((null rxs) [])
   ((null (cdr rxs)) (car rxs))
   (t (cons '| rxs))))
(defun rx-alt2 (r1 r2)
  (cond
   ((rx-empty-p r1) r2)
   ((rx-empty-p r2) r1)
   ((equal r1 r2) r1)
   (t (list '| r1 r2))))
(defun rx-con (r1 r2)
  (cond
   ((rx-empty-p r1) [])
   ((rx-empty-p r2) [])
   ((rx-null-p r1) r2)
   ((rx-null-p r2) r1)
   ((and (rx-act-p r1) (rx-act-p r2)) r2)
   (t (cons r1 r2))))
(defun rx-act (obj) (cons 'act obj))
(defun rx-cc (cs) (cons 'cc cs))

;;; regular expression preprocessing

(defun rx-range-to-ns (range)
  (cond
   ((char-or-char-int-p range)
    (natset-single (char-int range)))
   ((stringp range)
    (let ((ns (natset-empty)) (chars (string-to-int-list range)))
      (while chars
	(setq ns (natset-union ns (natset-single (car chars)))
	      chars (cdr chars)))
      ns))
   ((eq range 'non-ascii)
    (natset-seg 128))
   ((and (consp range)
	 (null (cdr range))
	 (char-or-char-int-p (car range)))
    (natset-seg (car range)))
   ((and (consp range)
	 (consp (cdr range))
	 (null (cddr range))
	 (char-or-char-int-p (car range))
	 (char-or-char-int-p (cadr range)))
    (natset-seg (char-int (car range)) (char-int (cadr range))))
   ((and (consp range)
	 (char-or-char-int-p (car range))
	 (char-or-char-int-p (cdr range)))
    (natset-seg (char-int (car range)) (1- (char-int (cdr range)))))
   (t (error "not range %s" range))))

(defun rx-vcc-to-rx (vcc)
  (let ((l (append vcc ())) neg ns)
    (if (eq (car l) '^)
	(setq l (cdr l)
	      neg t))
    (setq l (mapcar 'rx-range-to-ns l))
    (setq ns (natset-empty))
    (while l
      (setq ns (natset-union ns (car l))
	    l (cdr l)))
    (if neg (setq ns (natset-negate ns)))
    (if (natset-empty-p ns)
	[]
      (rx-cc ns))))

(defun rx-desugar (rx)
  (cond
   ((stringp rx) (rx-desugar (string-to-int-list rx)))
   ((vectorp rx) (rx-vcc-to-rx rx))
   ((char-or-char-int-p rx) (rx-cc (natset-single (char-int rx))))
   ((and (consp rx) (eq (car rx) '+)) (let ((r (rx-desugar (cdr rx)))) (rx-con r (rx-clo r))))
   ((and (consp rx) (eq (car rx) 'non-ascii)) (rx-cc (natset-seg 128)))
   ((and (consp rx) (eq (car rx) 'any)) (rx-cc (natset-full)))
   ((rx-empty-p rx) rx)
   ((rx-null-p rx) rx)
   ((rx-act-p rx) rx)
   ((rx-cc-p rx) rx)
   ((rx-clo-p rx) (rx-clo (rx-desugar (cdr rx))))
   ((rx-alt-p rx) (rx-alt (mapcar 'rx-desugar (cdr rx))))
   ((rx-con-p rx) (rx-con (rx-desugar (car rx)) (rx-desugar (cdr rx))))
   (t (error "not rx %s" rx))))

(defun rx-collect-cc (rx &optional cs)
  (cond
   ((rx-empty-p rx) cs)
   ((rx-null-p rx) cs)
   ((rx-act-p rx) cs)
   ((rx-cc-p rx) (append (cdr rx) cs))
   ((rx-clo-p rx) (rx-collect-cc (cdr rx) cs))
   ((rx-alt-p rx)
    (setq rx (cdr rx))
    (while (consp rx)
      (setq cs (rx-collect-cc (car rx) cs)
	    rx (cdr rx)))
    cs)
   ((rx-con-p rx) (rx-collect-cc (car rx) (rx-collect-cc (cdr rx) cs)))
   (t (error "not rx %s" rx))))

(defun rx-cc-to-pc (rx cs)
  (cond
   ((rx-empty-p rx) rx)
   ((rx-null-p rx) rx)
   ((rx-act-p rx) rx)
   ((rx-cc-p rx)
    (setq rx (cdr rx))
    (let (res)
      (while (and (consp rx) (consp (cdr rx)))
	(let ((start (car rx)) (end (cadr rx)))
	  (setq res (rx-filter (lambda (c) (and (<= start c) (< c end))) cs res)
		rx (cddr rx))))
      (if (consp rx)
	  (let ((start (car rx)))
	    (setq res (rx-filter (lambda (c) (<= start c)) cs res))))
      (rx-alt (rx-sort-int res))))
   ((rx-clo-p rx) (rx-clo (rx-cc-to-pc (cdr rx) cs)))
   ((rx-alt-p rx) (rx-alt (mapcar (lambda (r) (rx-cc-to-pc r cs)) (cdr rx))))
   ((rx-con-p rx) (rx-con (rx-cc-to-pc (car rx) cs) (rx-cc-to-pc (cdr rx) cs)))
   (t (error "not rx %s" rx))))

(defun rx-categolize-char (rx)
  (let ((cs (rx-sort-int (rx-collect-cc rx))))
    (cons
     (rx-cc-to-pc rx cs)
     cs)))

;;; simplification

(defun rx-nullable-p (rx)
  (cond
   ((rx-empty-p rx) nil)
   ((rx-null-p rx) t)
   ((rx-act-p rx) t)
   ((rx-pc-p rx) nil)
   ((rx-clo-p rx) t)
   ((rx-alt-p rx)
    (setq rx (cdr rx))
    (while (and (consp rx) (not (rx-nullable-p (car rx))))
      (setq rx (cdr rx)))
    (consp rx))
   ((rx-con-p rx)
    (and (rx-nullable-p (car rx)) (rx-nullable-p (cdr rx))))
   (t (error "not rx %s" rx))))

(defun rx-simplify (rx)
  (cond
   ((rx-empty-p rx) rx)
   ((rx-null-p rx) rx)
   ((rx-act-p rx) rx)
   ((rx-pc-p rx) rx)
   ((rx-clo-p rx)
    (rx-clo (rx-simplify (cdr rx))))
   ((rx-alt-p rx)
    (let ((in (cdr rx)) (out ())
	  already-simplified-list already-simplified)
      (while (consp in)
	(setq rx (car in)
	      in (cdr in)
	      already-simplified (car already-simplified-list)
	      already-simplified-list (cdr already-simplified-list))
	(if (rx-alt-p rx)
	    (setq in (append (cdr rx) in))
	  (progn
	    (setq rx (if already-simplified rx (rx-simplify rx)))
	    (cond
	     ((rx-empty-p rx)) ; [] is identity element for alternation.
	     ((rx-alt-p rx)
	      (setq in (append (cdr rx) in)
		    already-simplified-list (append (make-list (length (cdr rx)) nil) already-simplified-list)))
	     ((not (member rx out))
	      (setq out (cons rx out)))))))
      (rx-alt (rx-sort-rx (reverse out)))))
   ((rx-con-p rx)
    (catch 'return
      (let ((in (list (car rx) (cdr rx))) (out ())
	    already-simplified-list already-simplified)
	(while (consp in)
	  (setq rx (car in)
		in (cdr in)
		already-simplified (car already-simplified-list)
		already-simplified-list (cdr already-simplified-list))
	  (if (rx-con-p rx)
	      (setq in (rx-cons* (car rx) (cdr rx) in))
	    (progn
	      (setq rx (if already-simplified rx (rx-simplify rx)))
	      (cond
	       ((rx-empty-p rx) ; [] is zero element for concatination.
		(throw 'return []))
	       ((rx-null-p rx)) ; () is identity element for concatination.
	       ((rx-con-p rx)
		(setq in (rx-cons* (car rx) (cdr rx) in))
		      already-simplified-list (rx-cons* t t already-simplified-list))
	       (t
		(setq out (cons rx out)))))))
	(if (= (length out) 1)
	    (car out)
	  (nreverse out)))))
   (t (error "not rx %s" rx))))

;;; head property

(defun rx-head-pcs (rx &optional res)
  (cond
   ((rx-empty-p rx) res)
   ((rx-null-p rx) res)
   ((rx-act-p rx) res)
   ((rx-pc-p rx) (if (member rx res) res (cons rx res)))
   ((rx-clo-p rx) (rx-head-pcs (cdr rx) res))
   ((rx-alt-p rx)
    (setq rx (cdr rx))
    (while (consp rx)
      (setq res (rx-head-pcs (car rx) res)
	    rx (cdr rx)))
    res)
   ((rx-con-p rx)
    (setq res (rx-head-pcs (car rx) res))
    (if (rx-nullable-p (car rx))
	(setq res (rx-head-pcs (cdr rx) res)))
    res)
   (t (error "not rx %s" rx))))

(defun rx-head-act (rx &optional res)
  (cond
   ((rx-empty-p rx) res)
   ((rx-null-p rx) res)
   ((rx-act-p rx) (rx-better-act rx res))
   ((rx-pc-p rx) res)
   ((rx-clo-p rx) (rx-head-act (cdr rx) res))
   ((rx-alt-p rx)
    (setq rx (cdr rx))
    (while (consp rx)
      (setq res (rx-head-act (car rx) res)
	    rx (cdr rx)))
    res)
   ((rx-con-p rx)
    (setq res (rx-head-act (car rx) res))
    (if (rx-nullable-p (car rx))
	(setq res (rx-head-act (cdr rx) res)))
    res)
   (t (error "not rx %s" rx))))

;;; stepping

(defun rx-step-internal (rx pc)
  (cond
   ((rx-empty-p rx) [])
   ((rx-null-p rx) [])
   ((rx-act-p rx) [])
   ((rx-pc-p rx) (if (= rx pc) () []))
   ((rx-clo-p rx) (rx-con (rx-step-internal (cdr rx) pc) rx))
   ((rx-alt-p rx) (rx-alt (mapcar (lambda (r) (rx-step-internal r pc)) (cdr rx))))
   ((rx-con-p rx)
    (if (rx-nullable-p (car rx))
	(rx-alt2
	 (rx-con (rx-step-internal (car rx) pc) (cdr rx))
	 (rx-step-internal (cdr rx) pc))
      (rx-con (rx-step-internal (car rx) pc) (cdr rx))))
   (t (error "not rx %s" rx))))

(defun rx-step (rx &rest pcs)
  (while (consp pcs)
    (setq rx (rx-simplify (rx-step-internal rx (car pcs)))
	  pcs (cdr pcs)))
  rx)

;;; utilities    

(defun rx-better-act (a1 a2)
  (cond
   ((null a2) a1)
   ((< (cdr a1) (cdr a2)) a1)
   (t a2)))

(defun rx-cons* (elt &rest lst)
  (if (null lst)
      elt
    (cons elt (apply 'rx-cons* (car lst) (cdr lst)))))

(defun rx-filter (fun lst &optional rest)
  (if (null lst)
      rest
    (if (funcall fun (car lst))
	(cons (car lst) (rx-filter fun (cdr lst) rest))
      (rx-filter fun (cdr lst) rest))))

(defun rx-cmp-index (rx)
  (cond
   ((rx-null-p rx) (list 0))
   ((rx-act-p rx) (list 1 (cdr rx)))
   ((rx-empty-p rx) (list 2))
   ((rx-clo-p rx) (list 3 (cdr rx)))
   ((rx-alt-p rx) (cons 4 (cdr rx)))
   ((rx-con-p rx) (list 5 (car rx) (cdr rx)))
   ((rx-pc-p rx) (list 6 rx))
   (t (error "not rx %s" rx))))

(defun rx-cmp-int (i1 i2)
  (cond
   ((< i1 i2) -1)
   ((> i1 i2) 1)
   (t 0)))

(defun rx-cmp-rx (r1 r2)
  (let ((i1 (rx-cmp-index r1)) (i2 (rx-cmp-index r2)))
     (cond
      ((< (car i1) (car i2)) -1)
      ((> (car i1) (car i2)) 1)
      (t (setq i1 (cdr i1)
	       i2 (cdr i2))
	 (catch 'result
	   (while (and (consp i1) (consp i2))
	     (let ((r (if (and (integerp (car i1)) (integerp (car i2)))
			  (rx-cmp-int (car i1) (car i2))
			(rx-cmp-rx (car i1) (car i2)))))
	       (if (not (zerop r))
		   (throw 'result r)
		 (setq i1 (cdr i1)
		       i2 (cdr i2)))))
	   (if (null i1) (if (null i2) 0 -1) 1))))))

(defun rx-sort-rx (l &optional res)
  (if (null l)
      res
    (let ((e (car l)) lt gt cmp)
      (setq l (cdr l))
      (while (consp l)
	(setq cmp (rx-cmp-rx (car l) e))
	(cond
	 ((< cmp 0) (setq lt (cons (car l) lt)))
	 ((< 0 cmp) (setq gt (cons (car l) gt))))
	(setq l (cdr l)))
      (rx-sort-rx lt (cons e (rx-sort-rx gt res))))))

(defun rx-sort-int (l &optional res)
  (if (null l)
      res
    (let ((e (car l)) lt gt)
      (setq l (cdr l))
      (while (consp l)
	(cond
	 ((< (car l) e) (setq lt (cons (car l) lt)))
	 ((< e (car l)) (setq gt (cons (car l) gt))))
	(setq l (cdr l)))
      (rx-sort-int lt (cons e (rx-sort-int gt res))))))

