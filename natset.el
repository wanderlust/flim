(provide 'natset)

;;; pacage for set of natural number.
;; (natural number includes zero.)

;;; predicates

(defun natset-empty-p (ns)
  "Returns t if NS is empty."
  (equal ns ()))

(defun natset-full-p (ns)
  "Returns t if NS is full."
  (equal ns '(0)))

(defun natset-closed-p (ns)
  "Returns t if NS is closed."
  (= (logand (length ns) 1) 0))

(defun natset-open-p (ns)
  "Returns t if NS is open."
  (= (logand (length ns) 1) 1))

(defun natset-has-p (ns i)
  "Returns t if I is in NS."
  (not (natset-empty-p (natset-intersection (natset-single i) ns))))

(defun natset-has-intersection-p (ns1 ns2)
  "Returns t if the intersection of NS1 and NS2 is not empty."
  (not (natset-empty-p (natset-intersection ns1 ns2))))

(defun natset-include-p (ns1 ns2)
  "Returns t if NS1 includes NS2."
  (equal ns1 (natset-union ns1 ns2)))

;;; accessor

(defun natset-start (ns)
  "Returns start element in NS."
  (if (natset-empty-p ns)
      (error "natset empty" ns))
  (car ns))

;;; primitive constructor

(defun natset-empty ()
  "Returns a empty set.
{}"
  ())

(defun natset-full ()
  "Returns a full set.
{i | 0 <= i}"
  '(0))

(defun natset-single (&rest elts)
  "Returns a set contains singleton elements.
{i | i in ELTS}"
  (let ((ns (natset-empty)))
    (while elts
      (setq ns (natset-union ns (natset-seg (car elts) (car elts)))
	    elts (cdr elts)))
    ns))

(defun natset-seg (start &optional end)
  "Returns a set contains one segment.
{i | START <= i and i <= END}

If END is nil, Return the set {i | START <= i}"
  (if end
      (list start (1+ end))
    (list start)))

;;; complex constructor

(defun natset-start-set (ns)
  "Returns a set contains start boundaries for NS.
{i | NS does not contains i-1 and NS contains i}"
  (let ((res ()))
    (while ns
      (setq res (cons (1+ (car ns)) (cons (car ns) res))
	    ns (cddr ns)))
    (nreverse res)))

(defun natset-end-set (ns)
  "Returns a set contains end boundaries for NS.
{i | NS contains i-1 and NS does not contains i}"
  (let ((res ()))
    (setq ns (cdr ns))
    (while ns
      (setq res (cons (1+ (car ns)) (cons (car ns) res))
	    ns (cddr ns)))
    (nreverse res)))

(defun natset-boundary-set (ns)
  "Returns a set contains start and end boundaries for NS.
{i | NS contains i-1 xor NS does not contains i}"
  (natset-union (natset-start-set ns) (natset-end-set ns)))

(defun natset-minmax (ns)
  "Returns a set contains a range from minimum to maximam of NS.
{i | There exists j, k in NS, j <= i <= k}"
  (cond
   ((null ns) ())
   ((natset-open-p ns) (list (car ns)))
   (t
    (list (car ns) (nth (1- (length ns)) ns)))))

;;; set operation

(defun natset-negate (ns)
  "Returns negated set.
{i | 0 <= i and NS does not contains i}"
  (if (and (consp ns) (= (car ns) 0))
      (cdr ns)
    (cons 0 ns)))

(defun natset-union (&rest nss)
  "Returns unioned set.
{i | There exists ns in NSS s.t ns contains i}"
  (let ((ns (natset-empty)))
    (while nss
      (setq ns (natset-union2 ns (car nss))
	    nss (cdr nss)))
    ns))

(defun natset-intersection (&rest nss)
  "Returns intersectioned set.
{i | For all ns in NSS, ns contains i}"
  (natset-negate (apply 'natset-union (mapcar 'natset-negate nss))))

(defun natset-sub (ns &rest nss)
  "Returns subtracted set.
{i | NS contains i and for all ns in NSS, ns does not contains i}"
  (setq ns (natset-intersection ns (natset-negate (apply 'natset-union nss)))))

;;; enumeration

(defun natset-enum (ns)
  (if (natset-open-p ns)
      (error "natset open" ns))
  (let ((res ()) i j)
    (while ns
      (setq i (car ns)
	    j (cadr ns)
	    ns (cddr ns))
      (while (< i j)
	(setq res (cons i res)
	      i (1+ i))))
    (nreverse res)))

;;; code generation

(defun natset-take-seg (ns)
  (cond
   ((null ns) (error "NS empty" ns))
   ((null (cdr ns)) (cons ns ()))
   (t (cons (list (car ns) (cadr ns)) (cddr ns)))))

(defun natset-valid-filter (ns valid)
  "Returns a filtered set R.
R includes intersection between VALID and NS.
R does not include intersecton between VALID and negated NS.
Element does not contained in VALID is unspecified."
  (let* ((res (natset-intersection valid ns))
	 (len (length res))
	 (u-set (natset-negate valid))
	 tmp1 tmp2 tmpl)
    (while u-set
      (setq tmp1 (natset-take-seg u-set))
      (setq tmp2 (natset-union (car tmp1) res)
	    tmpl (length tmp2))
      (if (or (< tmpl len) (and (= tmpl len) (equal 0 (car tmp2))))
	  (setq res tmp2
		len (length tmp2)))
      (setq u-set (cdr tmp1)))
    res))

(defun natset-gen-pred-exp (ns var &optional valid)
  "Returns a expression to test value of variable VAR is in NS or not.

If VALID is not nil, the condition value of VAR is in VALID is assumed.
It is impossible to set VALID to empty set because empty set is represented as nil."
  (if valid (setq ns (natset-valid-filter ns valid)))
  (cond
   ((null ns) nil)
   ((= (car ns) 0) (natset-gen-pred-exp-internal (cdr ns) var nil 0))
   (t (natset-gen-pred-exp-internal ns var t 0))))

(defun natset-gen-ccl-branch (reg fail &rest clauses)
  (let* ((natsets (mapcar 'car clauses)))
    (let ((range (apply 'natset-union natsets)) tmp)
      (unless (natset-empty-p range)
	(setq natsets (cons (natset-negate range)
			    natsets)
	      clauses (cons (cons (car natsets)
				  fail)
			    clauses)))
      (setq range (natset-full)
	    tmp natsets)
      (while tmp
	(setcar tmp
		(natset-intersection
		 (car tmp)
		 range))
	(setq range (natset-sub range (car tmp))
	      tmp (cdr tmp))))
    (let ((b (natset-enum
	      (natset-sub
	       (apply
		'natset-union
		(mapcar
		 'natset-boundary-set
		 natsets))
	       (natset-single 0)))))
      (natset-gen-ccl-branch-internal reg 0 b clauses))))

(defun natset-gen-ccl-branch-internal (reg s b clauses)
  (cond
   ((null b)
    (cdr (natset-assoc s clauses)))
   ((null (cdr b))
    `(if (,reg < ,(car b))
	 ,(cdr (natset-assoc s clauses))
       ,(cdr (natset-assoc (car b) clauses))))
   (t
    (let* ((div (natset-divide (length b)))
	   (l (append b ()))
	   (g (nthcdr (1- div) l))
	   (m (cadr g)))
      (setq g (prog1 (cddr g) (setcdr g ())))
      `(if (,reg < ,m)
	   ,(natset-gen-ccl-branch-internal reg s l clauses)
	 ,(natset-gen-ccl-branch-internal reg m g clauses))))))

(defun natset-assoc (key alist)
  (catch 'return
    (while alist
      (when (natset-has-p (caar alist) key)
	(throw 'return (car alist)))
      (setq alist (cdr alist)))
    nil))

;;; internal primitive

(defun natset-union2 (ns1 ns2)
  (let (res start (end t))
    (while (and end (or (consp ns1) (consp ns2)))
      (if (and (consp ns1) (or (null ns2) (<= (car ns1) (car ns2))))
          (setq start (car ns1)
                end (cadr ns1)
                ns1 (cddr ns1))
        (setq start (car ns2)
              end (cadr ns2)
              ns2 (cddr ns2)))
      (while (and end
                  (or (and (consp ns1) (<= (car ns1) end))
                      (and (consp ns2) (<= (car ns2) end))))
        (if (and (consp ns1) (<= (car ns1) end))
	    (progn
	      (if (or (null (cadr ns1)) (< end (cadr ns1))) (setq end (cadr ns1)))
	      (setq ns1 (cddr ns1)))
	  (progn
	    (if (or (null (cadr ns2)) (< end (cadr ns2))) (setq end (cadr ns2)))
	    (setq ns2 (cddr ns2)))))
      (setq res (cons start res))
      (if end (setq res (cons end res))))
    (nreverse res)))

; n is greater or equal 2.
; returns one of 1 .. n-1
; (In reality, returns greatest 2^i - 1)
(defun natset-divide (n)
  (let ((l 2) tmp)
    (while (< (setq tmp (lsh l 1)) n)
      (setq l tmp))
    (1- l)))

(defun natset-gen-pred-exp-internal (ns var bool base)
  (cond
   ((null ns) (not bool))
   ((null (cdr ns))
    (if (<= (car ns) base)
	bool
      (if bool `(<= ,(car ns) ,var) `(< ,var ,(car ns)))))
   (t
    (let* ((div (natset-divide (length ns)))
	   (l (append ns ()))
	   (g (nthcdr (1- div) l))
	   (m (cadr g))
	   )
      (setq g (prog1 (cddr g) (setcdr g ())))
      `(if (< ,var ,m)
	   ,(natset-gen-pred-exp-internal l var bool base)
	 ,(natset-gen-pred-exp-internal
	   g var (if (= (logand div 1) 1) bool (not bool)) m))))))
