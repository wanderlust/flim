;;; directed graph package.

(defun digraph-make (&optional size)
  "Return newly allocated graph.
If SIZE is nil, it is assumed to 0."
  (unless size (setq size 0))
  (cons size (make-vector (max 1 size) nil)))

(defalias 'digraph-size 'car)

(defun digraph-new-node (g)
  "Allocate new node in G and return it."
  (unless (< (digraph-size g) (length (cdr g)))
    (setcdr g (vconcat (cdr g) (make-vector (length (cdr g)) nil))))
  (prog1
      (digraph-size g)
    (setcar g (1+ (digraph-size g)))))

(defun digraph-add-edge (g n1 n2)
  "Make edge from N1 to N2 in G."
  (unless (memq n2 (aref (cdr g) n1))
    (aset (cdr g) n1 (cons n2 (aref (cdr g) n1)))))

(defun digraph-descents (g n1)
  "Return nodes that has edge from N1 in G."
  (aref (cdr g) n1))

(put 'digraph-forall-node 'lisp-indent-function 2)
(defmacro digraph-forall-node (g-exp n-var &rest body)
  (let ((g (make-symbol "g"))
	(size (make-symbol "size")))
    `(let* ((,g ,g-exp)
	    (,n-var 0)
	    (,size (digraph-size ,g)))
       (while (< ,n-var ,size)
	 (progn
	   ,@body)
	 (setq ,n-var (1+ ,n-var))))))

(put 'digraph-forall-edge 'lisp-indent-function 3)
(defmacro digraph-forall-edge (g-exp i-var j-var &rest body)
  (let ((g (make-symbol "g"))
	(tmp (make-symbol "tmp"))
	(size (make-symbol "size")))
    `(let* ((,g ,g-exp)
	    (,size (digraph-size ,g))
	    (,i-var 0)
	    ,j-var ,tmp)
       (while (< ,i-var ,size)
	 (setq ,tmp (aref (cdr ,g) ,i-var))
	 (while ,tmp
	   (setq ,j-var (car ,tmp))
	   (progn
	     ,@body)
	   (setq ,tmp (cdr ,tmp)))
	 (setq ,i-var (1+ ,i-var))))))

(defun digraph-reverse (g)
  "Return newly allocated graph with reversed edge."
  (let* ((len (digraph-size g))
	 (a (cdr g))
         (rev (make-vector len nil))
         (i 0))
    (while (< i len)
      (let ((links (aref a i)))
        (while links
          (if (not (member i (aref rev (car links))))
              (aset rev (car links) (cons i (aref rev (car links)))))
          (setq links (cdr links))))
      (setq i (1+ i)))
    (cons len rev)))

(defun digraph-leaves (g)
  "Return list of leaves of G."
  (let* ((i (digraph-size g))
	 (a (cdr g))
	 (res ()))
    (while (< 0 i)
      (setq i (1- i))
      (if (null (aref a i))
          (setq res (cons i res))))
    res))

(defun digraph-roots (g)
  "Return list of roots of G."
  (digraph-leaves (digraph-reverse g)))

;;; topological sort

(defun digraph-tsort (g)
  "Sort nodes in a graph toporogicaly.

G is a graph and
result of digraph-tsort is list of lists of nodes.

If (nth n result) contains an node i, it represents the fact as
follows.

1. For all j in (nth n result) and j != i, node i depends to node j
and vice versa.

2. For all m < n and j in (nth m result), node i does not depend
to node j."
  (let* ((len (digraph-size g))
         (ord (make-vector len nil))
         (i 0)
         (res ()))
    (while (< i len)
      (if (not (aref ord i))
          (setq res (nth 3 (digraph-tsort-dfs (cdr g) len ord i 0 () res))))
      (setq i (1+ i)))
    res))

(defun digraph-tsort-dfs (dep len ord i id stk res)
  (aset ord i id)
  (let ((js (aref dep i))
        (m id)
        (nid (1+ id))
        (stk (cons i stk))
        (res res))
    (while js
      (let* ((j (car js)) (jo (aref ord j)))
        (if jo
            (setq m (if (< m jo) m jo))
          (let* ((tmp (digraph-tsort-dfs dep len ord j nid stk res))
                 (m0 (nth 0 tmp)))
            (setq m (if (< m m0) m m0)
                  nid (nth 1 tmp)
                  stk (nth 2 tmp)
                  res (nth 3 tmp)))))
      (setq js (cdr js)))
    (if (= m id)
        (let* ((p (member i stk))
               (nstk (cdr p))
               (tmp stk))
          (setcdr p ())
          (while tmp
            (aset ord (car tmp) len)
            (setq tmp (cdr tmp)))
          (list m nid nstk (cons stk res)))
      (list m nid stk res))))

;;; merge map

(defun digraph-make-merge-map (size)
  (make-vector size nil))

(defun digraph-mm-canonical-node-p (mm n)
  (not (integerp (aref mm n))))

(defun digraph-mm-canonicalize-node (mm n)
  (while (not (digraph-mm-canonical-node-p mm n))
    (setq n (aref mm n)))
  n)

(defun digraph-mm-push-info (mm n info)
  "Push additional information for N in MM."
  (setq n (digraph-mm-canonicalize-node mm n))
  (aset mm n (cons info (aref mm n))))

(defun digraph-mm-top-info (mm n)
  "Get a top information for N in MM."
  (setq n (digraph-mm-canonicalize-node mm n))
  (car (aref mm n)))

(defun digraph-mm-pop-info (mm n)
  "Pop an information for N in MM."
  (setq n (digraph-mm-canonicalize-node mm n))
  (aset mm n (cdr (aref mm n))))

(defun digraph-mm-merge-node (mm n m)
  "Merge N and M in MM.
The information for N is used for merged node."
  (setq n (digraph-mm-canonicalize-node mm n))
  (setq m (digraph-mm-canonicalize-node mm m))
  (unless (= n m)
    (aset mm m n)))

(defun digraph-mm-make-map (mm)
  (let ((i 0) (f-size (length mm)) (t-size 0))
    (while (< i f-size)
      (when (digraph-mm-canonical-node-p mm i)
	(digraph-mm-push-info mm i t-size)
	(setq t-size (1+ t-size)))
      (setq i (1+ i)))
    (let ((forward-map (make-vector f-size ()))
	  (reverse-map (make-vector t-size ()))
	  j)
      (setq i (1- f-size))
      (while (<= 0 i)
	(setq j (digraph-mm-top-info mm i))
	(aset forward-map i j)
	(aset reverse-map j (cons i (aref reverse-map j)))
	(setq i (1- i)))
      (setq i 0)
      (while (< i f-size)
	(when (digraph-mm-canonical-node-p mm i)
	  (digraph-mm-pop-info mm i))
	(setq i (1+ i)))
      (cons forward-map reverse-map))))

(defun digraph-concat-forward-map (map &rest maps)
  (while maps
    (setq map (vconcat (mapcar (lambda (n) (aref (car maps) n)) map))
	  maps (cdr maps)))
  map)

(defun digraph-concat-reverse-map (map &rest maps)
  (while maps
    (setq map
	  (vconcat
	   (mapcar
	    (lambda (elts)
	      (apply
	       'append
	       (mapcar (lambda (e) (aref (car maps) e)) elts)))
	    map))
	  maps (cdr maps)))
  map)

;; split

(defun digraph-split-as-dag (g)
  "Returns 3 element vector of follows.

0. Directed acyclic graph generated by mergeing each strongly connected
components in G as new nodes.

1. Map from a node in g to a node in result.

2. Map from a node in result to nodes in g."
  (let* ((tmp (digraph-tsort g))
	 (mm (digraph-make-merge-map (digraph-size g))))
    (while tmp
      (let ((n (caar tmp))
	    (nodes (cdar tmp)))
	(while nodes
	  (digraph-mm-merge-node mm n (car nodes))
	  (setq nodes (cdr nodes))))
      (setq tmp (cdr tmp)))
    (let* ((maps (digraph-mm-make-map mm))
	   (forward-map (car maps))
	   (reverse-map (cdr maps))
	   (new-g (digraph-make (length reverse-map))))
      (digraph-forall-edge g i j
	(unless (= (aref forward-map i) (aref forward-map j))
	  (digraph-add-edge new-g (aref forward-map i) (aref forward-map j))))
      (vector new-g forward-map reverse-map))))

(defun digraph-split-as-forest (g)
  "Returns 3 element vector of follows.

0. Tree generated by merging nodes which have common descent node.

1. Map from a node in g to a node in result.

2. Map from a node in result to nodes in g."
  (let* ((tmp (digraph-split-as-dag g))
         (d (aref tmp 0))
         (g-to-d (aref tmp 1))
         (d-to-g (aref tmp 2))
	 (mm (digraph-make-merge-map (digraph-size d)))
	 )
    (digraph-saf-dfs d mm '() (list (digraph-roots d)))
    (let* ((maps (digraph-mm-make-map mm))
	   (forward-map (car maps))
	   (reverse-map (cdr maps))
	   (f (digraph-make (length reverse-map))))
      (digraph-forall-edge d i j
	(unless (= (aref forward-map i) (aref forward-map j))
	  (digraph-add-edge f (aref forward-map i) (aref forward-map j))))
      (vector f
	      (digraph-concat-forward-map g-to-d forward-map)
	      (digraph-concat-reverse-map reverse-map d-to-g)))))

(defun digraph-saf-dfs (original mm node-stack descents-stack)
  (let ((status (make-vector (digraph-size original) 'unvisited)))
    (while descents-stack
      (if (null (car descents-stack))
	  (progn
	    (when node-stack
	      (aset status (car node-stack) 'visited)
	      (setq node-stack (cdr node-stack)))
	    (setq descents-stack (cdr descents-stack)))
	(let ((node (caar descents-stack))
	      (ascent (car node-stack)))
	  (setcar descents-stack (cdr (car descents-stack)))
	  (cond
	   ((eq (aref status node) 'visiting)
	    (error "not DAG"))
	   ((eq (aref status node) 'unvisited)
	    (digraph-mm-push-info mm node ascent)
	    (aset status node 'visiting)
	    (setq node-stack (cons node node-stack)
		  descents-stack (cons (digraph-descents original node) descents-stack)))
	   ((eq (aref status node) 'visited)
	    (let ((n node))
	      (while (and n (eq (aref status (setq n (digraph-mm-canonicalize-node mm n))) 'visited))
		(setq n (digraph-mm-top-info mm n)))
	      (when n
		(setq n (digraph-mm-canonicalize-node mm n)))
	      (let ((a (digraph-mm-canonicalize-node mm ascent))
		    (b (digraph-mm-canonicalize-node mm (digraph-mm-top-info mm node))))
		(while (and (not (eq a n)) (not (eq b n)))
		  (let ((a1 (digraph-mm-top-info mm a))
			(b1 (digraph-mm-top-info mm b)))
		    (digraph-mm-merge-node mm a b)
		    (unless a1
		      (digraph-mm-push-info mm a b1))
		    (setq a (and a1 (digraph-mm-canonicalize-node mm a1))
			  b (and b1 (digraph-mm-canonicalize-node mm b1)))))
		(when (and n (not (and (eq a n) (eq b n))))
		  (if (eq a n)
		      (while (not (eq n b))
			(let ((b1 (digraph-mm-canonicalize-node mm (digraph-mm-top-info mm b))))
			  (digraph-mm-merge-node mm n b)
			  (setq b b1)))
		    (while (not (eq n a))
		      (let ((a1 (digraph-mm-canonicalize-node mm (digraph-mm-top-info mm a))))
			(digraph-mm-merge-node mm n a)
			(setq a a1))))))))))))))

(provide 'digraph)
