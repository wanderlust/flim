;;; luna.el --- tiny OOP system kernel

;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.

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

(defmacro luna-find-class (name)
  "Return the luna-class of the given NAME."
  `(get ,name 'luna-class))

(defmacro luna-set-class (name class)
  `(put ,name 'luna-class ,class))

(defmacro luna-class-obarray (class)
  `(aref ,class 1))

(defmacro luna-class-number-of-slots (class)
  `(aref ,class 3))

(defmacro luna-define-class (type &optional parents slots)
  "Define TYPE as a luna-class.
If PARENTS is specified, TYPE inherits PARENTS.
Each parent must be name of luna-class (symbol).
If SLOTS is specified, TYPE will be defined to have them."
  (let ((oa (make-vector 31 0))
	(rest parents)
	parent name
	(i 2))
    (while rest
      (setq parent (pop rest))
      (mapatoms (lambda (sym)
		  (when (get sym 'luna-member-index)
		    (setq name (symbol-name sym))
		    (unless (intern-soft name oa)
		      (put (intern name oa) 'luna-member-index i)
		      (setq i (1+ i))
		      )))
		(luna-class-obarray parent))
      )
    (setq rest slots)
    (while rest
      (setq name (symbol-name (pop rest)))
      (unless (intern-soft name oa)
	(put (intern name oa) 'luna-member-index i)
	(setq i (1+ i))
	))
    `(luna-set-class ',type
		     (vector 'class ,oa ,parents ,i))
    ))

(defmacro luna-class-name (entity)
  "Return class-name of the ENTITY."
  `(aref ,entity 0))

(defmacro luna-set-class-name (entity name)
  `(aset ,entity 0 ,name))

(defmacro luna-get-obarray (entity)
  `(aref ,entity 1))

(defmacro luna-set-obarray (entity obarray)
  `(aset ,entity 1 ,obarray))

(defmacro luna-make-entity (type &rest init-args)
  "Make instance of luna-class TYPE and return it.
If INIT-ARGS is specified, it is used as initial values of the slots.
It must be plist and each slot name must have prefix `:'."
  `(apply #'luna-make-entity-function ',type ',init-args))

(defsubst luna-make-entity-function (type &rest init-args)
  (let* ((c (get type 'luna-class))
	 (v (make-vector (luna-class-number-of-slots c) nil))
	 (oa (luna-class-obarray c))
	 s i)
    (luna-set-class-name v type)
    (luna-set-obarray v (make-vector 7 0))
    (while init-args
      (setq s (intern-soft (substring (symbol-name (pop init-args)) 1) oa)
	    i (pop init-args))
      (if s
	  (aset v (get s 'luna-member-index) i)
	))
    v))

(defsubst luna-class-find-member (class member-name)
  (or (stringp member-name)
      (setq member-name (symbol-name member-name)))
  (intern-soft member-name (luna-class-obarray class)))

(defsubst luna-class-find-or-make-member (class member-name)
  (or (stringp member-name)
      (setq member-name (symbol-name member-name)))
  (intern member-name (luna-class-obarray class)))

(defmacro luna-class-slot-index (class slot-name)
  `(get (luna-class-find-member ,class ,slot-name) 'luna-member-index))

(defmacro luna-slot-index (entity slot-name)
  `(luna-class-slot-index (luna-find-class (luna-class-name ,entity))
			  ,slot-name))

(defsubst luna-slot-value (entity slot-name)
  "Return value of SLOT-NAME of ENTITY."
  (aref entity (luna-slot-index entity slot-name)))

(defmacro luna-define-method (name args &rest body)
  "Define NAME as a method function of (nth 1 (car ARGS)) backend.

ARGS is like an argument list of lambda, but (car ARGS) must be
specialized parameter.  (car (car ARGS)) is name of variable and (nth
1 (car ARGS)) is name of backend."
  (let* ((specializer (car args))
	 (class (nth 1 specializer))
	 (self (car specializer)))
    `(let ((func (lambda ,(if self
			      (cons self (cdr args))
			    (cdr args))
		   ,@body)))
       (fset (luna-class-find-or-make-member (luna-find-class ',class) ',name)
	     func))))

(put 'luna-define-method 'lisp-indent-function 'defun)

(defmacro luna-class-find-function (class service)
  `(symbol-function (luna-class-find-member ,class ,service)))

(defmacro luna-find-function (entity service)
  `(luna-class-find-function (luna-find-class (luna-class-name ,entity))
			     ,service))

(defsubst luna-send (entity message &rest args)
  "Send MESSAGE to ENTITY with ARGS, and return the result."
  (apply (luna-find-function entity message)
	 entity args))

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
		    ,@(luna-arglist-to-arguments (cdr args)))
	 )
    `(defun ,(intern (symbol-name name)) ,args
       (luna-send ,(car args) ',name
		  ,@(luna-arglist-to-arguments (cdr args)))
       )))

(put 'luna-define-generic 'lisp-indent-function 'defun)


;;; @ end
;;;

(provide 'luna)

;; luna.el ends here
