;;; mime-def.el --- definition module about MIME

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

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

(defconst mime-library-version
  '("Chao" "Karasuma Oike" 1 9 0)
  "Implementation name, version name and numbers of MIME-library package.")

(defconst mime-library-version-string
  `,(concat (car mime-library-version) " "
	    (mapconcat #'number-to-string
		       (cddr mime-library-version) ".")
	    " - \"" (cadr mime-library-version) "\""))


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

(defcustom mime-temp-directory (or (getenv "MIME_TMP_DIR")
				   (getenv "TM_TMP_DIR")
				   (getenv "TMPDIR")
				   (getenv "TMP")
				   (getenv "TEMP")
				   "/tmp/")
  "*Directory for temporary files."
  :group 'mime
  :type 'directory)

(defcustom mime-uuencode-encoding-name-list '("x-uue" "x-uuencode")
  "*List of encoding names for uuencode format."
  :group 'mime
  :type '(repeat string))


;;; @ required functions
;;;

(defsubst eliminate-top-spaces (string)
  "Eliminate top sequence of space or tab in STRING."
  (if (string-match "^[ \t]+" string)
      (substring string (match-end 0))
    string))

(defsubst regexp-* (regexp)
  (concat regexp "*"))

(defsubst regexp-or (&rest args)
  (concat "\\(" (mapconcat (function identity) args "\\|") "\\)"))


;;; @ about STD 11
;;;

(defconst std11-quoted-pair-regexp "\\\\.")
(defconst std11-non-qtext-char-list '(?\" ?\\ ?\r ?\n))
(defconst std11-qtext-regexp
  (concat "[^" (char-list-to-string std11-non-qtext-char-list) "]"))
(defconst std11-quoted-string-regexp
  (concat "\""
	  (regexp-*
	   (regexp-or std11-qtext-regexp std11-quoted-pair-regexp))
	  "\""))


;;; @ about MIME
;;;

(defconst mime-tspecials "][()<>@,\;:\\\"/?=")
(defconst mime-token-regexp (concat "[^" mime-tspecials "\000-\040]+"))
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

(defsubst make-mime-entity-internal (representation-type location
				     &optional content-type
				     children parent node-id
				     buffer
				     header-start header-end
				     body-start body-end)
  (vector representation-type location
	  content-type nil nil children parent node-id
	  buffer header-start header-end body-start body-end
	  nil nil))

(defsubst mime-entity-representation-type-internal (entity)
  (aref entity 0))
(defsubst mime-entity-set-representation-type-internal (entity type)
  (aset entity 0 type))
(defsubst mime-entity-location-internal (entity)
  (aref entity 1))

(defsubst mime-entity-content-type-internal (entity)
  (aref entity 2))
(defsubst mime-entity-set-content-type-internal (entity type)
  (aset entity 2 type))
(defsubst mime-entity-content-disposition-internal (entity)
  (aref entity 3))
(defsubst mime-entity-set-content-disposition-internal (entity disposition)
  (aset entity 3 disposition))
(defsubst mime-entity-encoding-internal (entity)
  (aref entity 4))
(defsubst mime-entity-set-encoding-internal (entity encoding)
  (aset entity 4 encoding))

(defsubst mime-entity-children-internal (entity)
  (aref entity 5))
(defsubst mime-entity-set-children-internal (entity children)
  (aset entity 5 children))
(defsubst mime-entity-parent-internal (entity)
  (aref entity 6))
(defsubst mime-entity-node-id-internal (entity)
  (aref entity 7))

(defsubst mime-entity-buffer-internal (entity)
  (aref entity 8))
(defsubst mime-entity-set-buffer-internal (entity buffer)
  (aset entity 8 buffer))
(defsubst mime-entity-header-start-internal (entity)
  (aref entity 9))
(defsubst mime-entity-set-header-start-internal (entity point)
  (aset entity 9 point))
(defsubst mime-entity-header-end-internal (entity)
  (aref entity 10))
(defsubst mime-entity-set-header-end-internal (entity point)
  (aset entity 10 point))
(defsubst mime-entity-body-start-internal (entity)
  (aref entity 11))
(defsubst mime-entity-set-body-start-internal (entity point)
  (aset entity 11 point))
(defsubst mime-entity-body-end-internal (entity)
  (aref entity 12))
(defsubst mime-entity-set-body-end-internal (entity point)
  (aset entity 12 point))

(defsubst mime-entity-original-header-internal (entity)
  (aref entity 13))
(defsubst mime-entity-set-original-header-internal (entity header)
  (aset entity 13 header))
(defsubst mime-entity-parsed-header-internal (entity)
  (aref entity 14))
(defsubst mime-entity-set-parsed-header-internal (entity header)
  (aset entity 14 header))


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
(put 'mm-define-method 'edebug-form-spec
     '(&define name ((arg symbolp) &rest arg) def-body))

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

(defmacro mel-define-service (name &optional args &rest rest)
  (if args
      `(progn
	 (defvar ,(intern (format "%s-obarray" name)) (make-vector 1 nil))
	 (defun ,name ,args
	   ,@rest
	   (funcall (mel-find-function ',name ,(car (last args)))
		    ,@(mm-arglist-to-arguments (butlast args)))
	   ))
    `(defvar ,(intern (format "%s-obarray" name)) (make-vector 1 nil))
    ))

(put 'mel-define-service 'lisp-indent-function 'defun)

(defmacro mel-define-method (name args &rest body)
  (let* ((specializer (car (last args)))
	 (class (nth 1 specializer)))
    `(progn
       (mel-define-service ,name)
       (fset (intern ,class ,(intern (format "%s-obarray" name)))
	     (lambda ,(butlast args)
	       ,@body)))))

(put 'mel-define-method 'lisp-indent-function 'defun)

(defmacro mel-define-method-function (spec function)
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


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
