(provide 'closure)

;; closure is one of following forms.
;;  FUNCTION
;;  (WRAPPER FUNCTION FV1 . FVS)
;;  (PARTIAL-ARGS CLOSURE)

(defmacro closure-make (fun &rest fvs)
  "Make a closure from a function FUN and free variables FVS.
CAUTION: Do not assign to free variables."
  (if (null fvs)
      fun
    (let* ((funv (make-symbol "funv"))
	   (args (make-symbol "args")))
      `(list
	(lambda (,funv ,args ,@fvs)
	  (apply ,funv ,args))
	,fun
	,@fvs))))

(defmacro closure-partial-call (clo &rest args)
  "Call partially."
  `(list (list ,@args) ,clo))

(defun closure-call (clo &rest args)
  "Call closure."
  (while
      (and
       (not (functionp clo))
       (if (cddr clo)
	   (progn
	     (setq args (cons (cadr clo) (cons args (cddr clo)))
		   clo (car clo))
	     nil)
	 t))
    (setq args (append (car clo) args)
	  clo (cadr clo)))
  (apply clo args))

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

(closure-call (closure-partial-call (closure-partial-call '+ 1 2 3) 4 5 6) 7 8 9) ;=> 45

)