;;; mime-def.el --- definition module about MIME

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.
;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

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

(require 'poe)
(require 'poem)
(require 'pcustom)
(require 'mcharset)
(require 'alist)

(eval-when-compile (require 'cl))	; list*

(eval-and-compile
  (defconst mime-library-product ["CLIME" (1 13 2) "額田部"]
    "Product name, version number and code name of MIME-library package.")
  )

(defmacro mime-product-name (product)
  (` (aref (, product) 0)))

(defmacro mime-product-version (product)
  (` (aref (, product) 1)))

(defmacro mime-product-code-name (product)
  (` (aref (, product) 2)))

(defconst mime-library-version
  (eval-when-compile
    (concat (mime-product-name mime-library-product) " "
	    (mapconcat (function number-to-string)
		       (mime-product-version mime-library-product) ".")
	    " - \"" (mime-product-code-name mime-library-product) "\"")))


;;; @ variables
;;;

(defgroup mime '((default-mime-charset custom-variable))
  "Emacs MIME Interfaces"
  :group 'news
  :group 'mail)

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
      (concat "[^" std11-non-qtext-char-list "]"))))
(defconst std11-quoted-string-regexp
  (eval-when-compile
    (concat "\""
	    (regexp-*
	     (regexp-or std11-qtext-regexp std11-quoted-pair-regexp))
	    "\"")))


;;; @ about MIME
;;;

(eval-and-compile
  (defconst mime-tspecial-char-list
    '(?\] ?\[ ?\( ?\) ?< ?> ?@ ?, ?\; ?: ?\\ ?\" ?/ ?? ?=)))
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

(require 'luna)

(autoload 'mime-entity-content-type "mime")
(autoload 'mime-parse-multipart "mime-parse")
(autoload 'mime-parse-encapsulated "mime-parse")
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
	 (primary-type (mime-content-type-primary-type content-type)))
    (cond ((eq primary-type 'multipart)
	   (mime-parse-multipart entity)
	   )
	  ((and (eq primary-type 'message)
		(memq (mime-content-type-subtype content-type)
		      '(rfc822 news external-body)
		      ))
	   (mime-parse-encapsulated entity)
	   ))
    ))

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
  (` (intern (format "mime-%s-entity" (, type)))))

(defmacro mm-define-backend (type &optional parents)
  (` (luna-define-class (, (mm-expand-class-name type))
			(, (nconc (mapcar (function
					   (lambda (parent)
					     (mm-expand-class-name parent)
					     ))
					  parents)
				  '(mime-entity))))))

(defmacro mm-define-method (name args &rest body)
  (or (eq name 'initialize-instance)
      (setq name (intern (format "mime-%s" name))))
  (let ((spec (car args)))
    (setq args
	  (cons (list (car spec)
		      (mm-expand-class-name (nth 1 spec)))
		(cdr args)))
    (` (luna-define-method (, name) (, args) (,@ body)))
    ))

(put 'mm-define-method 'lisp-indent-function 'defun)

(def-edebug-spec mm-define-method
  (&define name ((arg symbolp)
		 [&rest arg]
		 [&optional ["&optional" arg &rest arg]]
		 &optional ["&rest" arg]
		 )
	   def-body))


;;; @ message structure
;;;

(defvar mime-message-structure nil
  "Information about structure of message.
Please use reference function `mime-entity-SLOT' to get value of SLOT.

Following is a list of slots of the structure:

node-id			node-id (list of integers)
content-type		content-type (content-type)
content-disposition	content-disposition (content-disposition)
encoding		Content-Transfer-Encoding (string or nil)
children		entities included in this entity (list of entity)

If an entity includes other entities in its body, such as multipart or
message/rfc822, `mime-entity' structures of them are included in
`children', so the `mime-entity' structure become a tree.")

(make-variable-buffer-local 'mime-message-structure)

(make-obsolete-variable 'mime-message-structure "should not use it.")


;;; @ for mel-backend
;;;

(defvar mel-service-list nil)

(defmacro mel-define-service (name &optional args &rest rest)
  "Define NAME as a service for Content-Transfer-Encodings.
If ARGS is specified, NAME is defined as a generic function for the
service."
  (` (progn
       (add-to-list 'mel-service-list '(, name))
       (defvar (, (intern (format "%s-obarray" name))) (make-vector 7 0))
       (,@ (if args
	       (` ((defun (, name) (, args)
		     (,@ rest)
		     (funcall (mel-find-function '(, name)
						 (, (car (last args))))
			      (,@ (luna-arglist-to-arguments (butlast args))))
		     )))))
       )))

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
	(mapcar (function
		 (lambda (parent)
		   (` (mel-copy-backend (, parent) (, type)))
		   ))
		parents)))

(defmacro mel-define-method (name args &rest body)
  "Define NAME as a method function of (nth 1 (car (last ARGS))) backend.
ARGS is like an argument list of lambda, but (car (last ARGS)) must be
specialized parameter.  (car (car (last ARGS))) is name of variable
and (nth 1 (car (last ARGS))) is name of backend (encoding)."
  (let* ((specializer (car (last args)))
	 (class (nth 1 specializer)))
    (` (progn
	 (mel-define-service (, name))
	 (fset (intern (, class) (, (intern (format "%s-obarray" name))))
	       (function
		(lambda (, (butlast args))
		  (,@ body))))))))

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
    (` (let (sym)
	 (mel-define-service (, name))
	 (setq sym (intern (, class) (, (intern (format "%s-obarray" name)))))
	 (or (fboundp sym)
	     (fset sym (symbol-function (, function))))))))

(defmacro mel-define-function (function spec)
  (let* ((name (car spec))
	 (args (cdr spec))
	 (specializer (car (last args)))
	 (class (nth 1 specializer)))
    (` (progn
	 (define-function (, function)
	   (intern (, class) (, (intern (format "%s-obarray" name)))))
	 ))))

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
