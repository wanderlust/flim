;;; mime-def.el --- definition module about MIME

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

(require 'mcharset)

(eval-and-compile
  (defconst mime-library-product ["FLIM" (1 12 6) "Family-K.DŽòenmae"]
    "Product name, version number and code name of MIME-library package.")
  )

(defmacro mime-product-name (product)
  `(aref ,product 0))

(defmacro mime-product-version (product)
  `(aref ,product 1))

(defmacro mime-product-code-name (product)
  `(aref ,product 2))

(defconst mime-library-version
  (eval-when-compile
    (concat (mime-product-name mime-library-product) " "
	    (mapconcat #'number-to-string
		       (mime-product-version mime-library-product) ".")
	    " - \"" (mime-product-code-name mime-library-product) "\"")))


;;; @ variables
;;;

(require 'custom)

(eval-when-compile (require 'cl))

(defgroup mime nil
  "Emacs MIME Interfaces"
  :group 'news
  :group 'mail)

(custom-handle-keyword 'default-mime-charset :group 'mime
		       'custom-variable)

(defcustom mime-uuencode-encoding-name-list '("x-uue" "x-uuencode")
  "*List of encoding names for uuencode format."
  :group 'mime
  :type '(repeat string))


;;; @ required functions
;;;

(defsubst regexp-* (regexp)
  (concat regexp "*"))

(defsubst regexp-or (&rest args)
  (concat "\\(" (mapconcat (function identity) args "\\|") "\\)"))


;;; @ about STD 11
;;;

(eval-and-compile
  (defconst std11-quoted-pair-regexp "\\\\.")
  (defconst std11-non-qtext-char-list '(?\" ?\\ ?\r ?\n))
  (defconst std11-qtext-regexp
    (eval-when-compile
      (concat "[^" (apply #'string std11-non-qtext-char-list) "]"))))
(defconst std11-quoted-string-regexp
  (eval-when-compile
    (concat "\""
	    (regexp-*
	     (regexp-or std11-qtext-regexp std11-quoted-pair-regexp))
	    "\"")))


;;; @ about MIME
;;;

(defconst mime-tspecial-char-list
  '(?\] ?\[ ?\( ?\) ?< ?> ?@ ?, ?\; ?: ?\\ ?\" ?/ ?? ?=))
(defconst mime-token-regexp
  (eval-when-compile
    (concat "[^" mime-tspecial-char-list "\000-\040]+")))
(defconst mime-charset-regexp mime-token-regexp)

(defconst mime-media-type/subtype-regexp
  (concat mime-token-regexp "/" mime-token-regexp))


;;; @@ base64 / B
;;;

(defconst base64-token-regexp "[A-Za-z0-9+/]")
(defconst base64-token-padding-regexp "[A-Za-z0-9+/=]")

(defconst B-encoded-text-regexp
  (concat "\\(\\("
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-regexp
	  "\\)*"
	  base64-token-regexp
	  base64-token-regexp
	  base64-token-padding-regexp
	  base64-token-padding-regexp
          "\\)"))

;; (defconst eword-B-encoding-and-encoded-text-regexp
;;   (concat "\\(B\\)\\?" eword-B-encoded-text-regexp))


;;; @@ Quoted-Printable / Q
;;;

(defconst quoted-printable-hex-chars "0123456789ABCDEF")

(defconst quoted-printable-octet-regexp
  (concat "=[" quoted-printable-hex-chars
	  "][" quoted-printable-hex-chars "]"))

(defconst Q-encoded-text-regexp
  (concat "\\([^=?]\\|" quoted-printable-octet-regexp "\\)+"))

;; (defconst eword-Q-encoding-and-encoded-text-regexp
;;   (concat "\\(Q\\)\\?" eword-Q-encoded-text-regexp))


;;; @ Content-Type
;;;

(defsubst make-mime-content-type (type subtype &optional parameters)
  (list* (cons 'type type)
	 (cons 'subtype subtype)
	 (nreverse parameters))
  )

(defsubst mime-content-type-primary-type (content-type)
  "Return primary-type of CONTENT-TYPE."
  (cdr (car content-type)))

(defsubst mime-content-type-subtype (content-type)
  "Return primary-type of CONTENT-TYPE."
  (cdr (cadr content-type)))

(defsubst mime-content-type-parameters (content-type)
  "Return primary-type of CONTENT-TYPE."
  (cddr content-type))

(defsubst mime-content-type-parameter (content-type parameter)
  "Return PARAMETER value of CONTENT-TYPE."
  (cdr (assoc parameter (mime-content-type-parameters content-type))))


(defsubst mime-type/subtype-string (type &optional subtype)
  "Return type/subtype string from TYPE and SUBTYPE."
  (if type
      (if subtype
	  (format "%s/%s" type subtype)
	(format "%s" type))))


;;; @ Content-Disposition
;;;

(defsubst mime-content-disposition-type (content-disposition)
  "Return disposition-type of CONTENT-DISPOSITION."
  (cdr (car content-disposition)))

(defsubst mime-content-disposition-parameters (content-disposition)
  "Return disposition-parameters of CONTENT-DISPOSITION."
  (cdr content-disposition))

(defsubst mime-content-disposition-parameter (content-disposition parameter)
  "Return PARAMETER value of CONTENT-DISPOSITION."
  (cdr (assoc parameter (cdr content-disposition))))

(defsubst mime-content-disposition-filename (content-disposition)
  "Return filename of CONTENT-DISPOSITION."
  (mime-content-disposition-parameter content-disposition "filename"))


;;; @ MIME entity
;;;

(defmacro make-mime-entity-internal (representation-type location
				     &optional content-type
				     children parent node-id
				     ;; for NOV
				     decoded-subject decoded-from
				     date message-id references
				     chars lines
				     xref
				     ;; for other fields
				     original-header parsed-header
				     ;; for buffer representation
				     buffer
				     header-start header-end
				     body-start body-end)
  `(vector ,representation-type ,location
	   ,content-type nil nil ,children ,parent ,node-id
	   ;; for NOV
	   ,decoded-subject ,decoded-from
	   ,date ,message-id ,references
	   ,chars ,lines
	   ,xref
	   ;; for other fields
	   ,original-header ,parsed-header
	   ;; for buffer representation
	   ,buffer ,header-start ,header-end ,body-start ,body-end))

(defmacro mime-entity-representation-type-internal (entity)
  `(aref ,entity 0))
(defmacro mime-entity-set-representation-type-internal (entity type)
  `(aset ,entity 0 ,type))
(defmacro mime-entity-location-internal (entity)
  `(aref ,entity 1))
(defmacro mime-entity-set-location-internal (entity location)
  `(aset ,entity 1 ,location))

(defmacro mime-entity-content-type-internal (entity)
  `(aref ,entity 2))
(defmacro mime-entity-set-content-type-internal (entity type)
  `(aset ,entity 2 ,type))
(defmacro mime-entity-content-disposition-internal (entity)
  `(aref ,entity 3))
(defmacro mime-entity-set-content-disposition-internal (entity disposition)
  `(aset ,entity 3 ,disposition))
(defmacro mime-entity-encoding-internal (entity)
  `(aref ,entity 4))
(defmacro mime-entity-set-encoding-internal (entity encoding)
  `(aset ,entity 4 ,encoding))

(defmacro mime-entity-children-internal (entity)
  `(aref ,entity 5))
(defmacro mime-entity-set-children-internal (entity children)
  `(aset ,entity 5 ,children))
(defmacro mime-entity-parent-internal (entity)
  `(aref ,entity 6))
(defmacro mime-entity-node-id-internal (entity)
  `(aref ,entity 7))

(defmacro mime-entity-decoded-subject-internal (entity)
  `(aref ,entity 8))
(defmacro mime-entity-set-decoded-subject-internal (entity subject)
  `(aset ,entity 8 ,subject))
(defmacro mime-entity-decoded-from-internal (entity)
  `(aref ,entity 9))
(defmacro mime-entity-set-decoded-from-internal (entity from)
  `(aset ,entity 9 ,from))
(defmacro mime-entity-date-internal (entity)
  `(aref ,entity 10))
(defmacro mime-entity-set-date-internal (entity date)
  `(aset ,entity 10 ,date))
(defmacro mime-entity-message-id-internal (entity)
  `(aref ,entity 11))
(defmacro mime-entity-set-message-id-internal (entity message-id)
  `(aset ,entity 11 ,message-id))
(defmacro mime-entity-references-internal (entity)
  `(aref ,entity 12))
(defmacro mime-entity-set-references-internal (entity references)
  `(aset ,entity 12 ,references))
(defmacro mime-entity-chars-internal (entity)
  `(aref ,entity 13))
(defmacro mime-entity-set-chars-internal (entity chars)
  `(aset ,entity 13 ,chars))
(defmacro mime-entity-lines-internal (entity)
  `(aref ,entity 14))
(defmacro mime-entity-set-lines-internal (entity lines)
  `(aset ,entity 14 ,lines))
(defmacro mime-entity-xref-internal (entity)
  `(aref ,entity 15))
(defmacro mime-entity-set-xref-internal (entity xref)
  `(aset ,entity 15 ,xref))

(defmacro mime-entity-original-header-internal (entity)
  `(aref ,entity 16))
(defmacro mime-entity-set-original-header-internal (entity header)
  `(aset ,entity 16 ,header))
(defmacro mime-entity-parsed-header-internal (entity)
  `(aref ,entity 17))
(defmacro mime-entity-set-parsed-header-internal (entity header)
  `(aset ,entity 17 ,header))

(defmacro mime-entity-buffer-internal (entity)
  `(aref ,entity 18))
(defmacro mime-entity-set-buffer-internal (entity buffer)
  `(aset ,entity 18 ,buffer))
(defmacro mime-entity-header-start-internal (entity)
  `(aref ,entity 19))
(defmacro mime-entity-set-header-start-internal (entity point)
  `(aset ,entity 19 ,point))
(defmacro mime-entity-header-end-internal (entity)
  `(aref ,entity 20))
(defmacro mime-entity-set-header-end-internal (entity point)
  `(aset ,entity 20 ,point))
(defmacro mime-entity-body-start-internal (entity)
  `(aref ,entity 21))
(defmacro mime-entity-set-body-start-internal (entity point)
  `(aset ,entity 21 ,point))
(defmacro mime-entity-body-end-internal (entity)
  `(aref ,entity 22))
(defmacro mime-entity-set-body-end-internal (entity point)
  `(aset ,entity 22 ,point))


;;; @ message structure
;;;

(defvar mime-message-structure nil
  "Information about structure of message.
Please use reference function `mime-entity-SLOT' to get value of SLOT.

Following is a list of slots of the structure:

buffer			buffer includes this entity (buffer).
node-id			node-id (list of integers)
header-start		minimum point of header in raw-buffer
header-end		maximum point of header in raw-buffer
body-start		minimum point of body in raw-buffer
body-end		maximum point of body in raw-buffer
content-type		content-type (content-type)
content-disposition	content-disposition (content-disposition)
encoding		Content-Transfer-Encoding (string or nil)
children		entities included in this entity (list of entity)

If an entity includes other entities in its body, such as multipart or
message/rfc822, `mime-entity' structures of them are included in
`children', so the `mime-entity' structure become a tree.")

(make-variable-buffer-local 'mime-message-structure)


;;; @ for mm-backend
;;;

(require 'alist)

(defvar mime-entity-implementation-alist nil)

(defmacro mm-define-backend (type &optional parents)
  "Define TYPE as a mm-backend.
If PARENTS is specified, TYPE inherits PARENTS.
Each parent must be backend name (symbol)."
  (if parents
      `(let ((rest ',(reverse parents)))
	 (while rest
	   (set-alist 'mime-entity-implementation-alist
		      ',type
		      (copy-alist
		       (cdr (assq (car rest)
				  mime-entity-implementation-alist))))
	   (setq rest (cdr rest))
	   ))))

(defmacro mm-define-method (name args &rest body)
  "Define NAME as a method function of (nth 1 (car ARGS)) backend.

ARGS is like an argument list of lambda, but (car ARGS) must be
specialized parameter.  (car (car ARGS)) is name of variable and (nth
1 (car ARGS)) is name of backend."
  (let* ((specializer (car args))
	 (class (nth 1 specializer))
	 (self (car specializer)))
    `(let ((imps (cdr (assq ',class mime-entity-implementation-alist)))
	   (func (lambda ,(if self
			      (cons self (cdr args))
			    (cdr args))
		   ,@body)))
       (if imps
	   (set-alist 'mime-entity-implementation-alist
		      ',class (put-alist ',name func imps))
	 (set-alist 'mime-entity-implementation-alist
		    ',class
		    (list (cons ',name func)))
	 ))))

(put 'mm-define-method 'lisp-indent-function 'defun)

(eval-when-compile
  (defmacro eval-module-depended-macro (module definition)
    (condition-case nil
	(progn
	  (require (eval module))
	  definition)
      (error `(eval-after-load ,(symbol-name (eval module)) ',definition))
      ))
  )

(eval-module-depended-macro
 'edebug
 (def-edebug-spec mm-define-method
   (&define name ((arg symbolp)
		  [&rest arg]
		  [&optional ["&optional" arg &rest arg]]
		  &optional ["&rest" arg]
		  )
	    def-body))
 )

(defsubst mm-arglist-to-arguments (arglist)
  (let (dest)
    (while arglist
      (let ((arg (car arglist)))
	(or (memq arg '(&optional &rest))
	    (setq dest (cons arg dest)))
	)
      (setq arglist (cdr arglist)))
    (nreverse dest)))


;;; @ for mel-backend
;;;

(defvar mel-service-list nil)

(defmacro mel-define-service (name &optional args &rest rest)
  "Define NAME as a service for Content-Transfer-Encodings.
If ARGS is specified, NAME is defined as a generic function for the
service."
  `(progn
     (add-to-list 'mel-service-list ',name)
     (defvar ,(intern (format "%s-obarray" name)) (make-vector 7 0))
     ,@(if args
	   `((defun ,name ,args
	       ,@rest
	       (funcall (mel-find-function ',name ,(car (last args)))
			,@(mm-arglist-to-arguments (butlast args)))
	       )))
     ))

(put 'mel-define-service 'lisp-indent-function 'defun)


(defvar mel-encoding-module-alist nil)

(defsubst mel-find-function-from-obarray (ob-array encoding)
  (let* ((f (intern-soft encoding ob-array)))
    (or f
	(let ((rest (cdr (assoc encoding mel-encoding-module-alist))))
	  (while (and rest
		      (progn
			(require (car rest))
			(null (setq f (intern-soft encoding ob-array)))
			))
	    (setq rest (cdr rest))
	    )
	  f))))

(defsubst mel-copy-method (service src-backend dst-backend)
  (let* ((oa (symbol-value (intern (format "%s-obarray" service))))
	 (f (mel-find-function-from-obarray oa src-backend))
	 sym)
    (when f
      (setq sym (intern dst-backend oa))
      (or (fboundp sym)
	  (fset sym (symbol-function f))
	  ))))
       
(defsubst mel-copy-backend (src-backend dst-backend)
  (let ((services mel-service-list))
    (while services
      (mel-copy-method (car services) src-backend dst-backend)
      (setq services (cdr services)))))

(defmacro mel-define-backend (type &optional parents)
  "Define TYPE as a mel-backend.
If PARENTS is specified, TYPE inherits PARENTS.
Each parent must be backend name (string)."
  (cons 'progn
	(mapcar (lambda (parent)
		  `(mel-copy-backend ,parent ,type)
		  )
		parents)))

(defmacro mel-define-method (name args &rest body)
  "Define NAME as a method function of (nth 1 (car (last ARGS))) backend.
ARGS is like an argument list of lambda, but (car (last ARGS)) must be
specialized parameter.  (car (car (last ARGS))) is name of variable
and (nth 1 (car (last ARGS))) is name of backend (encoding)."
  (let* ((specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(progn
       (mel-define-service ,name)
       (fset (intern ,class ,(intern (format "%s-obarray" name)))
	     (lambda ,(butlast args)
	       ,@body)))))

(put 'mel-define-method 'lisp-indent-function 'defun)

(defmacro mel-define-method-function (spec function)
  "Set SPEC's function definition to FUNCTION.
First element of SPEC is service.
Rest of ARGS is like an argument list of lambda, but (car (last ARGS))
must be specialized parameter.  (car (car (last ARGS))) is name of
variable and (nth 1 (car (last ARGS))) is name of backend (encoding)."
  (let* ((name (car spec))
	 (args (cdr spec))
	 (specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(let (sym)
       (mel-define-service ,name)
       (setq sym (intern ,class ,(intern (format "%s-obarray" name))))
       (or (fboundp sym)
	   (fset sym (symbol-function ,function))))))

(defmacro mel-define-function (function spec)
  (let* ((name (car spec))
	 (args (cdr spec))
	 (specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(progn
       (define-function ,function
	 (intern ,class ,(intern (format "%s-obarray" name))))
       )))

(defvar base64-dl-module
  (if (and (fboundp 'base64-encode-string)
	   (subrp (symbol-function 'base64-encode-string)))
      nil
    (if (fboundp 'dynamic-link)
	(let ((path (expand-file-name "base64.so" exec-directory)))
	  (and (file-exists-p path)
	       path)
	  ))))


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
