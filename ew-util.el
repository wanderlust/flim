(provide 'ew-util)

(defmacro ew-cons* (seed &rest rest)
  (setq rest (nreverse (cons seed rest))
	seed (car rest)
	rest (cdr rest))
  (while rest
    (setq seed `(cons ,(car rest) ,seed)
	  rest (cdr rest)))
  seed)
    
(defmacro ew-rcons* (seed &rest rest)
  (while rest
    (setq seed `(cons ,(car rest) ,seed)
	  rest (cdr rest)))
  seed)

(defmacro ew-rappend (a b)
  `(append (reverse ,b) ,a))
