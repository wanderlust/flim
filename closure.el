(provide 'closure)

(defmacro closure-make (fun &rest fvs)
  "Make closure from function FUN and free variable list FVS.
CAUTION: Do not assign to free variables."
  (let* ((funv (make-symbol "funv"))
	 (args (make-symbol "args")))
  `(list
    ,fun
    (lambda (,funv ,args ,@fvs)
      (apply ,funv ,args))
    ,@fvs)))

(defun closure-call (clo &rest args)
  "Call closure."
  (if (functionp clo)
      (apply clo args)
    (apply (cadr clo) (car clo) args (cddr clo))))

(defun closure-compose (c1 c2)
  "Compose C1 and C2.

If either C1 or C2 is nil, another one is returned.
If C1 and C2 is non-nil, C1 must be closure with one argument."
  (cond
   ((null c1) c2)
   ((null c2) c1)
   (t
    (closure-make
     (lambda (&rest args)
       (closure-call c1 (apply 'closure-call c2 args)))
     c1 c2))))

'(

(setq c1 (let ((a 1)) (closure-make (lambda (b) (+ a b)) a)))
(closure-call c1 2) ; => 3

(let ((a 1)) (setq plus1 (closure-make (lambda (b) (+ a b)) a)))
(let ((a 2)) (setq plus2 (closure-make (lambda (b) (+ a b)) a)))
(setq plus3 (closure-compose plus1 plus2))
(closure-call plus3 4) ; => 7

)