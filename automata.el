
(require 'digraph)
(require 'natset)
(provide 'automata)

(defvar automata-never-fail (make-symbol "automata-never-fail"))
(defvar automata-state-var (make-symbol "automata-state"))

(defmacro automata (in-var start-tag &rest clauses)
  (let* ((org-len (length clauses))
	 (org-graph (make-vector org-len nil))
	 (tag-to-org-alist nil)
	 forest org-to-forest forest-to-org
	 i j tmp trans)
    (setq tmp clauses
	  i 0)
    (while tmp
      (setq tag-to-org-alist (cons (cons (caar tmp) i) tag-to-org-alist)
	    i (1+ i)
	    tmp (cdr tmp)))
    (setq tmp clauses
	  i 0)
    (while tmp
      (setq trans (cddr (cdar tmp)))
      (while trans
	(setq j (cdr (assoc (cadr (car trans)) tag-to-org-alist)))
	(if (not (member j (aref org-graph i)))
	    (aset org-graph i (cons j (aref org-graph i))))
	(setq trans (cdr trans)))
      (setq i (1+ i)
	    tmp (cdr tmp)))
    ;;(error "%s" org-graph)
    (setq tmp (digraph-split-as-forest org-graph)
	  forest (aref tmp 0)
	  org-to-forest (aref tmp 1)
	  forest-to-org (aref tmp 2))
    (setq clauses
	  (mapcar
	   (lambda (c)
	     (let ((tag (car c))
		   (action (cadr c))
		   (fail (nth 2 c))
		   (trs (nthcdr 3 c)))
	       (setq trs
		     (mapcar
		      (lambda (next)
			(list
			 (apply
			  'natset-union
			  (mapcar
			   (lambda (tr) (if (equal (cadr tr) next) (car tr) (natset-empty)))
			   trs))
			 next))
		      (natset-enum (apply 'natset-single (mapcar 'cadr trs)))))
	       (cons tag (cons action (cons fail trs)))))
	   clauses))
    `(let ((,automata-state-var ,(cdr (assoc start-tag tag-to-org-alist))))
       ,@(automata-exp-seq
	  (automata-gen-state
	   in-var clauses
	   org-len
	   (list (cdr (assoc start-tag tag-to-org-alist)))
	   tag-to-org-alist
	   (aref org-to-forest (cdr (assoc start-tag tag-to-org-alist)))
	   forest org-to-forest forest-to-org)))))

(defun automata-gen-state (in-var clauses
				  org-len
				  start-states
				  tag-to-org-alist
				  forest-state forest org-to-forest forest-to-org)
  (let* ((org-states (aref forest-to-org forest-state))
	 (forest-states (digraph-descents forest forest-state))
	 (branch-length (+ (length org-states) (length forest-states)))
	 (branch-to-org
	  (vconcat
	   (mapcar 'list org-states)
	   (mapcar (lambda (forest-state)
		     (aref forest-to-org forest-state))
		   forest-states)))
	 (org-to-branch
	  (let ((org-to-branch (make-vector org-len nil))
		(i 0) tmp)
	    (while (< i branch-length)
	      (setq tmp (aref branch-to-org i))
	      (while tmp
		(aset org-to-branch (car tmp) i)
		(setq tmp (cdr tmp)))
	      (setq i (1+ i)))
	    org-to-branch))
	 (branch-to-forest
	  (vconcat
	   (make-list (length org-states) nil)
	   forest-states))
	 (branch-state-range
	  (vconcat
	   (mapcar 'natset-single org-states)
	   (mapcar (lambda (forest-state)
		     (apply 'natset-single
			    (aref forest-to-org forest-state)))
		   forest-states)))
	 (branch-descents
	  (vconcat
	   (mapcar (lambda (org-state)
		     (let* ((c (nth org-state clauses))
			    (trs (nthcdr 3 c)))
		       (apply 'natset-union
			      (mapcar (lambda (tr)
					(natset-single
					 (cdr (assoc (cadr tr) tag-to-org-alist))))
				      trs))))
		   org-states)
	   (mapcar (lambda (forest-state) ())
		   forest-states)))
	 (all-descents (apply 'natset-union (append branch-descents ())))
	 (branch-ascents
	  (let* ((branch-ascents (make-vector branch-length 0))
		 (i 0) j)
	    (while (< i branch-length)
	      (setq j 0)
	      (while (< j branch-length)
		(if (natset-has-intersection-p (aref branch-state-range i)
					       (aref branch-descents j))
		    (aset branch-ascents i
			  (1+ (aref branch-ascents i))))
		(setq j (1+ j)))
	      (setq i (1+ i)))
	    branch-ascents))
	 (start-inline nil)
	 (branch-inline
	  (let* ((branch-inline (make-vector branch-length nil))
		 (start-ns (apply 'natset-single start-states))
		 (i 0))
	    (while (< i branch-length)
	      (if (natset-has-intersection-p (aref branch-state-range i) start-ns)
		  (if (and (= (length start-states) 1)
			   (= (aref branch-ascents i) 0))
		      (progn
			(setq start-inline i)
			(aset branch-inline i t))
		    nil)
		(if (= (aref branch-ascents i) 1)
		    (aset branch-inline i t)))
	      (setq i (1+ i)))
	    branch-inline))
	 (branch-gen nil)
	 (_
	  (setq branch-gen
		(vconcat
		 (mapcar
		  (lambda (org-state)
		    (cons
		     (lambda (org-state)
		       `(,(natset-single org-state)
			 nil ; don't duplicate.
			 ,@(let* ((c (nth org-state clauses)))
			     (automata-exp-seq
			      (nth 1 c) ; action
			      (if (null (nthcdr 3 c))
				  (nth 2 c)
				`(automata-branch
				  ,in-var ; input variable
				  ,(natset-full) ; input is unpredictable.
				  ,(nth 2 c) ; fail action
				  ,@(let ((trs (nthcdr 3 c)))
				      (mapcar
				       (lambda (next-branch)
					 (let ((input-range 
						(apply 'natset-union
						       (mapcar
							(lambda (tr)
							  (if (member (cdr (assoc (cadr tr) tag-to-org-alist))
								      (aref branch-to-org next-branch))
							      (car tr)
							    (natset-empty)))
							trs))))
					   `(,input-range ; input range
					     ,(not (aref branch-inline next-branch)) ; duplicatable unless inlining.
					     ,(let ((goto-list (apply
								'append
								(mapcar
								 (lambda (tr)
								   (let ((range (natset-intersection (car tr) input-range)))
								     (if range
									 `((,range
									    t
									    (automata-goto
									     ,automata-state-var
									     ,org-state
									     ,(cdr (assoc (cadr tr) tag-to-org-alist)))))
								       ())))
								 trs))))
						(if (= (length goto-list) 1)
						    (car (cddr (car goto-list)))
						  `(automata-branch
						    ,in-var
						    ,input-range
						    ,automata-never-fail
						    ,@goto-list)))
					     ,@(if (aref branch-inline next-branch)
						   (automata-exp-seq
						    `(progn ,@(cddr (funcall (car (aref branch-gen next-branch))
									     (cdr (aref branch-gen next-branch))))))
						 ()))))
				       (natset-enum
					(apply 'natset-union
					       (mapcar
						(lambda (tr)
						  (natset-single
						   (aref org-to-branch
							 (cdr (assoc (cadr tr) tag-to-org-alist)))))
						trs))))
				      )))))))
		     org-state))
		  org-states)
		 (mapcar
		  (lambda (forest-state)
		    (cons
		     (lambda (forest-state)
		       `(,(natset-intersection
			   (apply 'natset-single (aref forest-to-org forest-state))
			   all-descents) ; state range
			 nil ; don't duplicate.
			 ,@(automata-exp-seq
			    (automata-gen-state
			     in-var clauses
			     org-len
			     (aref forest-to-org forest-state)
			     tag-to-org-alist
			     forest-state forest org-to-forest forest-to-org))))
		     forest-state))
		  forest-states))))
	 (branches
	  (let* ((branches ())
		 (i branch-length))
	    (while (< 0 i)
	      (setq i (1- i))
	      (if (not (aref branch-inline i))
		  (setq branches
			(cons
			 (funcall (car (aref branch-gen i))
				  (cdr (aref branch-gen i)))
			 branches))))
	    branches))
	 )
    ;;(error "err")
    (if start-inline
	(apply
	 'automata-seq-exp
	 `(progn ,@(cddr (funcall (car (aref branch-gen start-inline))
				  (cdr (aref branch-gen start-inline)))))
	 (cond
	  ((null branches) ())
	  ((null (cdr branches))
	   (cddr (car branches)))
	  (t
	   `((while t 
	       (automata-branch
		,automata-state-var ,(natset-full) ,automata-never-fail
		,@branches))))))
      (if (= (length branches) 1)
	  `(while t ,@(cddr (car branches)))
	`(while t ; ,branch-inline ,branch-state-range ,branch-descents ,branch-ascents
		(automata-branch
		 ,automata-state-var ,(natset-full) ,automata-never-fail
		 ,@branches))))))

(defun automata-seq-exp (&rest seq)
  (cond
   ((null seq) nil)
   ((null (cdr seq))
    (car seq))
   (t
    (setq seq
	  (apply
	   'append
	   (mapcar
	    (lambda (exp) (if (and (consp exp) (eq (car exp) 'progn))
			      (cdr exp)
			    (list exp)))
	    seq)))
    (let ((rseq (reverse seq)))
      (cons 'progn
	    (apply
	     'append
	     (apply
	      'append
	      (mapcar
	       (lambda (exp) (if (null exp) () (list exp)))
	       (nreverse (cdr rseq))))
	     (list (list (car rseq)))))))))

(defun automata-exp-seq (&rest seq)
  (let ((exp (apply 'automata-seq-exp seq)))
    (if (and (consp exp) (eq (car exp) 'progn))
	(cdr exp)
      (list exp))))

(defmacro automata-goto (var curr next)
  (if (eq curr next)
      nil
    `(setq ,var ,next)))

(defmacro automata-branch (var range fail &rest clauses)
  (when (eq fail automata-never-fail)
    (setq range (natset-intersection
		 (apply 'natset-union (mapcar 'car clauses))
		 range)))
  (let ((len (length clauses))
	ns-list dup-list body-list tmp ns)
    (setq tmp clauses
	  ns (natset-negate range))
    (while tmp
      (setq ns-list (cons (natset-sub (caar tmp) ns) ns-list)
	    dup-list (cons (cadr (car tmp)) dup-list)
	    body-list (cons (cddr (car tmp)) body-list)
	    ns (natset-union ns (caar tmp))
	    tmp (cdr tmp))
      (if (natset-empty-p (car ns-list))
	  (setq ns-list (cdr ns-list)
		dup-list (cdr dup-list)
		body-list (cdr body-list))))
    (setq ns-list (nreverse ns-list)
	  dup-list (nreverse dup-list)
	  body-list (nreverse body-list))
    (automata-branch-i var range fail ns-list dup-list body-list)))

(defun automata-branch-i (var range fail ns-list dup-list body-list)
  (cond
   ((null ns-list) fail)
   ((null (cdr ns-list))
    (if (natset-include-p (car ns-list) range)
	(apply 'automata-seq-exp (car body-list))
      `(if ,(natset-gen-pred-exp (car ns-list) var range)
	   ,(apply 'automata-seq-exp (car body-list))
	 ,fail)))
   (t
    (let (tmp tmpn tmpd cut)
      (setq tmpn ns-list
	    cut (natset-empty))
      (while tmpn
	(setq cut (natset-union cut (natset-boundary-set (car tmpn)))
	      tmpn (cdr tmpn)))
      (setq tmpn ns-list
	    tmpd dup-list)
      (while tmpn
	(if (not (car tmpd))
	    (setq tmp (natset-minmax (car tmpn))
		  tmp (natset-sub tmp (natset-start-set tmp))
		  cut (natset-sub cut tmp)))
	(setq tmpn (cdr tmpn)
	      tmpd (cdr tmpd)))
      (setq cut (natset-sub cut (natset-boundary-set (natset-minmax range))))
      (if (null (setq cut (natset-enum cut)))
	  `(if ,(natset-gen-pred-exp (car ns-list) var range)
	       ,(apply 'automata-seq-exp (car body-list))
	     ,(automata-branch-i var
				 (natset-sub range (car ns-list))
				 fail
				 (cdr ns-list)
				 (cdr dup-list)
				 (cdr body-list)))
	(let* ((mid (nth (/ (length cut) 2) cut))
	       (lower (natset-seg 0 (1- mid)))
	       (higher (natset-seg mid))
	       ns-list1 dup-list1 body-list1
	       ns-list2 dup-list2 body-list2
	       )
	  (while ns-list
	    (if (natset-has-intersection-p lower (car ns-list))
		(setq ns-list1 (cons (natset-intersection (car ns-list) lower) ns-list1)
		      dup-list1 (cons (car dup-list) dup-list1)
		      body-list1 (cons (car body-list) body-list1)))
	    (if (natset-has-intersection-p higher (car ns-list))
		(setq ns-list2 (cons (natset-intersection (car ns-list) higher) ns-list2)
		      dup-list2 (cons (car dup-list) dup-list2)
		      body-list2 (cons (car body-list) body-list2)))
	    (setq ns-list (cdr ns-list)
		  dup-list (cdr dup-list)
		  body-list (cdr body-list)))
	  ;;(if (or (null ns-list1) (null ns-list2)) (error "divide fail"))
	  `(if (< ,var ,mid)
	       ,(automata-branch-i var
				   (natset-intersection range lower)
				   fail ns-list1 dup-list1 body-list1)
	     ,(automata-branch-i var
				 (natset-intersection range higher)
				 fail ns-list2 dup-list2 body-list2))))))))


'(
(npp (macroexpand '
(automata pc 0
		(1
		 (progn
		   (lex-match 1)
		   (lex-scan-unibyte-save))
		 (lex-fail))
		(5
		 (progn
		   (lex-match 2)
		   (lex-scan-unibyte-save))
		 (lex-fail))
		(4
		 (progn
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((9 10) 5) ((32 33) 5))
		(2
		 (progn
		   (lex-match 3)
		   (lex-scan-unibyte-save)
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((13 14) 2)
		 ((0 9) 3) ((11 13) 3) ((14 32) 3) ((33) 3)
		 ((10 11) 4))
		(3
		 (progn
		   (lex-match 3)
		   (lex-scan-unibyte-save)
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((0 9) 3) ((10 11) 3) ((11 13) 3) ((14 32) 3) ((33) 3)
		 ((13 14) 2)
		 )
		(0
		 (progn
		   (lex-match 3)
		   (lex-scan-unibyte-save)
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((0 9) 3) ((10 11) 3) ((11 13) 3) ((14 32) 3) ((33) 3)
		 ((13 14) 2)
		 ((9 10) 1) ((32 33) 1)))))

(npp (macroexpand '
(automata pc 0
		(0
		 (progn
		   (lex-match 3)
		   (lex-scan-unibyte-save)
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((0 9) 3) ((10 11) 3) ((11 13) 3) ((14 32) 3) ((33) 3)
		 ((13 14) 2)
		 ((9 10) 1) ((32 33) 1))
		(1
		 (progn
		   (lex-match 1)
		   (lex-scan-unibyte-save))
		 (lex-fail))
		(2
		 (progn
		   (lex-match 3)
		   (lex-scan-unibyte-save)
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((13 14) 2)
		 ((0 9) 3) ((11 13) 3) ((14 32) 3) ((33) 3)
		 ((10 11) 4))
		(3
		 (progn
		   (lex-match 3)
		   (lex-scan-unibyte-save)
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((0 9) 3) ((10 11) 3) ((11 13) 3) ((14 32) 3) ((33) 3)
		 ((13 14) 2)
		 )
		(4
		 (progn
		   (lex-scan-unibyte-read pc))
		 (lex-fail)
		 ((9 10) 5) ((32 33) 5))
		(5
		 (progn
		   (lex-match 2)
		   (lex-scan-unibyte-save))
		 (lex-fail))
)))

(npp
(automata-gen-state
'pc
'((0 (progn (lex-match 3) (lex-scan-unibyte-save) (lex-scan-unibyte-read pc)) (lex-fail) ((9 10 32 33) 1) ((13 14) 2) ((0 9 10 13 14 32 33) 3))
  (1 (progn (lex-match 1) (lex-scan-unibyte-save)) (lex-fail))
  (2 (progn (lex-match 3) (lex-scan-unibyte-save) (lex-scan-unibyte-read pc)) (lex-fail) ((13 14) 2) ((0 9 11 13 14 32 33) 3) ((10 11) 4))
  (3 (progn (lex-match 3) (lex-scan-unibyte-save) (lex-scan-unibyte-read pc)) (lex-fail) ((13 14) 2) ((0 9 10 13 14 32 33) 3))
  (4 (progn (lex-scan-unibyte-read pc)) (lex-fail) ((9 10 32 33) 5))
  (5 (progn (lex-match 2) (lex-scan-unibyte-save)) (lex-fail)))
6
'(0)
'((5 . 5) (4 . 4) (3 . 3) (2 . 2) (1 . 1) (0 . 0))
0
[(4 1) (2) (3) nil nil]
[0 4 1 1 2 3]
[(0) (3 2) (4) (5) (1)]))
)