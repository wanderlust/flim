;;; luna.el --- tiny OOP system kernel

;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: OOP

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(eval-when-compile (require 'cl))

(defmacro luna-find-class (name)
  "Return the luna-class of the given NAME."
  `(get ,name 'luna-class))

(defmacro luna-set-class (name class)
  `(put ,name 'luna-class ,class))

(defmacro luna-class-obarray (class)
  `(aref ,class 1))

(defmacro luna-class-parents (class)
  `(aref ,class 2))

(defmacro luna-class-number-of-slots (class)
  `(aref ,class 3))

(defmacro luna-define-class (type &optional parents slots)
  "Define TYPE as a luna-class.
If PARENTS is specified, TYPE inherits PARENTS.
Each parent must be name of luna-class (symbol).
If SLOTS is specified, TYPE will be defined to have them."
  `(luna-define-class-function ',type ',(append parents '(standard-object))
			       ',slots))

(defun luna-define-class-function (type &optional parents slots)
  (let ((oa (make-vector 31 0))
	(rest parents)
	parent name
	(i 2)
	b j)
    (while rest
      (setq parent (pop rest)
	    b (- i 2))
      (mapatoms (lambda (sym)
		  (when (setq j (get sym 'luna-slot-index))
		    (setq name (symbol-name sym))
		    (unless (intern-soft name oa)
		      (put (intern name oa) 'luna-slot-index (+ j b))
		      (setq i (1+ i))
		      )))
		(luna-class-obarray (luna-find-class parent)))
      )
    (setq rest slots)
    (while rest
      (setq name (symbol-name (pop rest)))
      (unless (intern-soft name oa)
	(put (intern name oa) 'luna-slot-index i)
	(setq i (1+ i))
	))
    (luna-set-class type (vector 'class oa parents i))
    ))

(defun luna-class-find-member (class member-name)
  (or (stringp member-name)
      (setq member-name (symbol-name member-name)))
  (or (intern-soft member-name (luna-class-obarray class))
      (let ((parents (luna-class-parents class))
	    ret)
	(while (and parents
		    (null
		     (setq ret (luna-class-find-member
				(luna-find-class (pop parents))
				member-name)))))
	ret)))

(defsubst luna-class-find-or-make-member (class member-name)
  (or (stringp member-name)
      (setq member-name (symbol-name member-name)))
  (intern member-name (luna-class-obarray class)))

(defmacro luna-class-slot-index (class slot-name)
  `(get (luna-class-find-member ,class ,slot-name) 'luna-slot-index))

(defmacro luna-slot-index (entity slot-name)
  `(luna-class-slot-index (luna-find-class (luna-class-name ,entity))
			  ,slot-name))

(defsubst luna-slot-value (entity slot)
  "Return the value of SLOT of ENTITY."
  (aref entity (luna-slot-index entity slot)))

(defsubst luna-set-slot-value (entity slot value)
  "Store VALUE into SLOT of ENTITY."
  (aset entity (luna-slot-index entity slot) value))

(defmacro luna-define-method (name &rest definition)
  "Define NAME as a method function of a class.

Usage of this macro follows:

  (luna-define-method NAME [METHOD-QUALIFIER] ARGLIST [DOCSTRING] BODY...) 

NAME is the name of method.

Optional argument METHOD-QUALIFIER must be :before or :after.  If it
is :before / :after, the method is called before / after a method of
parent class is finished.  ARGLIST is like an argument list of lambda,
but (car ARGLIST) must be specialized parameter.  (car (car ARGLIST))
is name of variable and \(nth 1 (car ARGLIST)) is name of class.

Optional argument DOCSTRING is the documentation of method.

BODY is the body of method."
  (let ((method-qualifier (pop definition))
	args specializer class self)
    (if (memq method-qualifier '(:before :after))
	(setq args (pop definition))
      (setq args method-qualifier
	    method-qualifier nil)
      )
    (setq specializer (car args)
	  class (nth 1 specializer)
	  self (car specializer))
    `(let ((func (lambda ,(if self
			      (cons self (cdr args))
			    (cdr args))
		   ,@definition))
	   (sym (luna-class-find-or-make-member
		 (luna-find-class ',class) ',name)))
       (fset sym func)
       (put sym 'luna-method-qualifier ,method-qualifier)
       )))

(put 'luna-define-method 'lisp-indent-function 'defun)

(def-edebug-spec luna-define-method
  (&define name [&optional &or ":before" ":after"]
	   ((arg symbolp)
	    [&rest arg]
	    [&optional ["&optional" arg &rest arg]]
	    &optional ["&rest" arg]
	    )
	   def-body))

(defun luna-class-find-parents-functions (class service)
  (let ((parents (luna-class-parents class))
	ret)
    (while (and parents
		(null
		 (setq ret (luna-class-find-functions
			    (luna-find-class (pop parents))
			    service)))))
    ret))

(defun luna-class-find-functions (class service)
  (let ((sym (luna-class-find-member class service)))
    (if (fboundp sym)
	(cond ((eq (get sym 'luna-method-qualifier) :before)
	       (cons (symbol-function sym)
		     (luna-class-find-parents-functions class service))
	       )
	      ((eq (get sym 'luna-method-qualifier) :after)
	       (nconc (luna-class-find-parents-functions class service)
		      (list (symbol-function sym)))
	       )
	      (t
	       (list (symbol-function sym))
	       ))
      (luna-class-find-parents-functions class service)
      )))

(defmacro luna-find-functions (entity service)
  `(luna-class-find-functions (luna-find-class (luna-class-name ,entity))
			      ,service))

(defsubst luna-send (entity message &rest args)
  "Send MESSAGE to ENTITY with ARGS, and return the result."
  (let ((functions (luna-find-functions entity message))
	ret)
    (while functions
      (setq ret (apply (car functions) args)
	    functions (cdr functions))
      )
    ret))

(defmacro luna-class-name (entity)
  "Return class-name of the ENTITY."
  `(aref ,entity 0))

(defmacro luna-set-class-name (entity name)
  `(aset ,entity 0 ,name))

(defmacro luna-get-obarray (entity)
  `(aref ,entity 1))

(defmacro luna-set-obarray (entity obarray)
  `(aset ,entity 1 ,obarray))

(defun luna-make-entity (type &rest init-args)
  "Make instance of luna-class TYPE and return it.
If INIT-ARGS is specified, it is used as initial values of the slots.
It must be plist and each slot name must have prefix `:'."
  (let* ((c (get type 'luna-class))
	 (v (make-vector (luna-class-number-of-slots c) nil)))
    (luna-set-class-name v type)
    (luna-set-obarray v (make-vector 7 0))
    (apply #'luna-send v 'initialize-instance v init-args)
    ))

(defsubst luna-arglist-to-arguments (arglist)
  (let (dest)
    (while arglist
      (let ((arg (car arglist)))
	(or (memq arg '(&optional &rest))
	    (setq dest (cons arg dest)))
	)
      (setq arglist (cdr arglist)))
    (nreverse dest)))

(defmacro luna-define-generic (name args &optional doc)
  "Define generic-function NAME.
ARGS is argument of and DOC is DOC-string."
  (if doc
      `(defun ,(intern (symbol-name name)) ,args
	 ,doc
	 (luna-send ,(car args) ',name
		    ,@(luna-arglist-to-arguments args))
	 )
    `(defun ,(intern (symbol-name name)) ,args
       (luna-send ,(car args) ',name
		  ,@(luna-arglist-to-arguments args))
       )))

(put 'luna-define-generic 'lisp-indent-function 'defun)

(defun luna-define-internal-accessors (class-name)
  "Define internal accessors for an entity of CLASS-NAME."
  (let ((entity-class (luna-find-class class-name))
	parents parent-class)
    (mapatoms
     (lambda (slot)
       (if (luna-class-slot-index entity-class slot)
	   (catch 'derived
	     (setq parents (luna-class-parents entity-class))
	     (while parents
	       (setq parent-class (luna-find-class (car parents)))
	       (if (luna-class-slot-index parent-class slot)
		   (throw 'derived nil))
	       (setq parents (cdr parents))
	       )
	     (eval
	      `(progn
		 (defmacro ,(intern (format "%s-%s-internal"
					    class-name slot))
		   (entity)
		   (list 'aref entity
			 ,(luna-class-slot-index entity-class
						 (intern (symbol-name slot)))
			 ))
		 (defmacro ,(intern (format "%s-set-%s-internal"
					    class-name slot))
		   (entity value)
		   (list 'aset entity
			 ,(luna-class-slot-index
			   entity-class (intern (symbol-name slot)))
			 value))
		 ))
	     )))
     (luna-class-obarray entity-class))))

(luna-define-class-function 'standard-object)

(luna-define-method initialize-instance ((entity standard-object)
					 &rest init-args)
  (let* ((c (luna-find-class (luna-class-name entity)))
	 (oa (luna-class-obarray c))
	 s i)
    (while init-args
      (setq s (intern-soft (substring (symbol-name (pop init-args)) 1) oa)
	    i (pop init-args))
      (if s
	  (aset entity (get s 'luna-slot-index) i)
	))
    entity))


;;; @ end
;;;

(provide 'luna)

;; luna.el ends here
