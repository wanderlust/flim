;;; mmgeneric.el --- MIME generic entity module

;; Copyright (C) 1995,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: definition, MIME, multimedia, mail, news

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

(require 'luna)


;;; @ MIME entity
;;;

(autoload 'mime-entity-content-type "mime")
(autoload 'mime-parse-multipart "mime-parse")
(autoload 'mime-parse-encapsulated "mime-parse")
(autoload 'mime-parse-external "mime-parse")
(autoload 'mime-entity-content "mime")

(luna-define-class mime-entity ()
		   (location
		    content-type children parent
		    node-id
		    content-disposition encoding
		    ;; for other fields
		    original-header parsed-header))

(defalias 'mime-entity-representation-type-internal 'luna-class-name)
(defalias 'mime-entity-set-representation-type-internal 'luna-set-class-name)

(luna-define-internal-accessors 'mime-entity)

(luna-define-method mime-entity-fetch-field ((entity mime-entity)
					     field-name)
  (or (symbolp field-name)
      (setq field-name (intern (capitalize (capitalize field-name)))))
  (cdr (assq field-name
	     (mime-entity-original-header-internal entity))))

(luna-define-method mime-entity-children ((entity mime-entity))
  (let* ((content-type (mime-entity-content-type entity))
	 (primary-type (mime-content-type-primary-type content-type))
	 sub-type)
    (cond ((eq primary-type 'multipart)
	   (mime-parse-multipart entity))
	  ((eq primary-type 'message)
	   (setq sub-type (mime-content-type-subtype content-type))
	   (cond ((eq sub-type 'external-body)
		  (mime-parse-external entity))
		 ((memq sub-type '(rfc822 news))
		  (mime-parse-encapsulated entity)
		  ;; [tomo] Should we make a variable to specify
		  ;; encapsulated media-types?
		  ))))))

(luna-define-method mime-insert-text-content ((entity mime-entity))
  (insert
   (decode-mime-charset-string (mime-entity-content entity)
			       (or (mime-content-type-parameter
				    (mime-entity-content-type entity)
				    "charset")
				   default-mime-charset)
			       'CRLF)
   ))


;;; @ for mm-backend
;;;

(defmacro mm-expand-class-name (type)
  `(intern (format "mime-%s-entity" ,type)))

(defmacro mm-define-backend (type &optional parents)
  `(luna-define-class ,(mm-expand-class-name type)
		      ,(nconc (mapcar (lambda (parent)
					(mm-expand-class-name parent)
					)
				      parents)
			      '(mime-entity))))

(defmacro mm-define-method (name args &rest body)
  (or (eq name 'initialize-instance)
      (setq name (intern (format "mime-%s" name))))
  (let ((spec (car args)))
    (setq args
	  (cons (list (car spec)
		      (mm-expand-class-name (nth 1 spec)))
		(cdr args)))
    `(luna-define-method ,name ,args ,@body)
    ))

(put 'mm-define-method 'lisp-indent-function 'defun)

(def-edebug-spec mm-define-method
  (&define name ((arg symbolp)
		 [&rest arg]
		 [&optional ["&optional" arg &rest arg]]
		 &optional ["&rest" arg]
		 )
	   def-body))


;;; @ end
;;;

(provide 'mmgeneric)

;;; mmgeneric.el ends here
