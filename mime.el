;;; mime.el --- MIME library module

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: MIME, multimedia, mail, news

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

(require 'alist)
(require 'std11)
(require 'mime-def)
(require 'eword-decode)

(autoload 'eword-encode-field "eword-encode"
  "Encode header field STRING, and return the result.")
(autoload 'eword-encode-header "eword-encode"
  "Encode header fields to network representation, such as MIME encoded-word.")


(autoload 'mime-parse-Content-Type "mime-parse"
  "Parse STRING as field-body of Content-Type field.")
(autoload 'mime-read-Content-Type "mime-parse"
  "Read field-body of Content-Type field from current-buffer,
and return parsed it.")

(autoload 'mime-parse-Content-Disposition "mime-parse"
  "Parse STRING as field-body of Content-Disposition field.")
(autoload 'mime-read-Content-Disposition "mime-parse"
  "Read field-body of Content-Disposition field from current-buffer,
and return parsed it.")

(autoload 'mime-parse-Content-Transfer-Encoding "mime-parse"
  "Parse STRING as field-body of Content-Transfer-Encoding field.")
(autoload 'mime-read-Content-Transfer-Encoding "mime-parse"
  "Read field-body of Content-Transfer-Encoding field from
current-buffer, and return it.")

(autoload 'mime-parse-buffer "mime-parse"
  "Parse BUFFER as a MIME message.")


;;; @ Entity Representation and Implementation
;;;

(defsubst mime-find-function (service type)
  (let ((imps (cdr (assq type mime-entity-implementation-alist))))
    (if imps
	(cdr (assq service imps))
      (require (intern (format "mm%s" type)))
      (cdr (assq service
		 (cdr (assq type mime-entity-implementation-alist))))
      )))

(defsubst mime-entity-function (entity service)
  (mime-find-function service
		      (mime-entity-representation-type-internal entity)))

(defsubst mime-entity-send (entity message &rest args)
  "Send MESSAGE to ENTITY with ARGS, and return the result."
  (apply (mime-find-function
	  message (mime-entity-representation-type-internal entity))
	 entity
	 args))

(defmacro mm-define-generic (name args &optional doc)
  (if doc
      `(defun ,(intern (format "mime-%s" name)) ,args
	 ,doc
	 (mime-entity-send ,(car args) ',name
			   ,@(mm-arglist-to-arguments (cdr args)))
	 )
    `(defun ,(intern (format "mime-%s" name)) ,args
       (mime-entity-send ,(car args) ',name
			 ,@(mm-arglist-to-arguments (cdr args)))
       )))

(put 'mm-define-generic 'lisp-indent-function 'defun)

(defun mime-open-entity (type location)
  "Open an entity and return it.
TYPE is representation-type.
LOCATION is location of entity.  Specification of it is depended on
representation-type."
  (let ((entity (make-mime-entity-internal type location)))
    (mime-entity-send entity 'initialize-instance)
    entity))

(mm-define-generic entity-cooked-p (entity)
  "Return non-nil if contents of ENTITY has been already code-converted.")


;;; @ Entity as node of message
;;;

(defun mime-entity-children (entity)
  (or (mime-entity-children-internal entity)
      (mime-entity-send entity 'entity-children)))

(defalias 'mime-entity-node-id 'mime-entity-node-id-internal)

(defun mime-entity-number (entity)
  "Return entity-number of ENTITY."
  (reverse (mime-entity-node-id-internal entity)))

(defun mime-find-entity-from-number (entity-number &optional message)
  "Return entity from ENTITY-NUMBER in MESSAGE.
If MESSAGE is not specified, `mime-message-structure' is used."
  (or message
      (setq message mime-message-structure))
  (let ((sn (car entity-number)))
    (if (null sn)
	message
      (let ((rc (nth sn (mime-entity-children message))))
	(if rc
	    (mime-find-entity-from-number (cdr entity-number) rc)
	  ))
      )))

(defun mime-find-entity-from-node-id (entity-node-id &optional message)
  "Return entity from ENTITY-NODE-ID in MESSAGE.
If MESSAGE is not specified, `mime-message-structure' is used."
  (mime-find-entity-from-number (reverse entity-node-id) message))

(defun mime-entity-parent (entity &optional message)
  "Return mother entity of ENTITY.
If MESSAGE is specified, it is regarded as root entity."
  (if (equal entity message)
      nil
    (mime-entity-parent-internal entity)))

(defun mime-root-entity-p (entity &optional message)
  "Return t if ENTITY is root-entity (message).
If MESSAGE is specified, it is regarded as root entity."
  (null (mime-entity-parent entity message)))


;;; @ Entity Buffer
;;;

(defun mime-entity-buffer (entity)
  (or (mime-entity-buffer-internal entity)
      (mime-entity-send entity 'entity-buffer)))

(mm-define-generic entity-point-min (entity)
  "Return the start point of ENTITY in the buffer which contains ENTITY.")

(mm-define-generic entity-point-max (entity)
  "Return the end point of ENTITY in the buffer which contains ENTITY.")

(defun mime-entity-header-start (entity)
  (or (mime-entity-header-start-internal entity)
      (mime-entity-send entity 'entity-header-start)))

(defun mime-entity-header-end (entity)
  (or (mime-entity-header-end-internal entity)
      (mime-entity-send entity 'entity-header-end)))

(defun mime-entity-body-start (entity)
  (or (mime-entity-body-start-internal entity)
      (mime-entity-send entity 'entity-body-start)))

(defun mime-entity-body-end (entity)
  (or (mime-entity-body-end-internal entity)
      (mime-entity-send entity 'entity-body-end)))


;;; @ Entity Header
;;;

(defun mime-fetch-field (field-name &optional entity)
  (or (symbolp field-name)
      (setq field-name (intern (capitalize (capitalize field-name)))))
  (or entity
      (setq entity mime-message-structure))
  (let* ((header (mime-entity-original-header-internal entity))
	 (field-body (cdr (assq field-name header))))
    (or field-body
	(progn
	  (if (setq field-body
		    (mime-entity-send entity 'fetch-field
				      (symbol-name field-name)))
	      (mime-entity-set-original-header-internal
	       entity (put-alist field-name field-body header))
	    )
	  field-body))))

(defalias 'mime-entity-content-type 'mime-entity-content-type-internal)

(defun mime-entity-content-disposition (entity)
  (or (mime-entity-content-disposition-internal entity)
      (let ((ret (mime-fetch-field 'Content-Disposition entity)))
	(if ret
	    (let ((disposition (mime-parse-Content-Disposition ret)))
	      (when disposition
		(mime-entity-set-content-disposition-internal
		 entity disposition)
		disposition))))))

(defun mime-entity-encoding (entity &optional default-encoding)
  (or (mime-entity-encoding-internal entity)
      (let ((encoding
	     (or (let ((ret (mime-fetch-field
			     'Content-Transfer-Encoding entity)))
		   (and ret (mime-parse-Content-Transfer-Encoding ret)))
		 default-encoding "7bit")))
	(mime-entity-set-encoding-internal entity encoding)
	encoding)))

(defun mime-read-field (field-name &optional entity)
  (or (symbolp field-name)
      (setq field-name (capitalize (capitalize field-name))))
  (or entity
      (setq entity mime-message-structure))
  (cond ((eq field-name 'Content-Type)
	 (mime-entity-content-type entity)
	 )
	((eq field-name 'Content-Disposition)
	 (mime-entity-content-disposition entity)
	 )
	((eq field-name 'Content-Transfer-Encoding)
	 (mime-entity-encoding entity)
	 )
	(t
	 (let* ((header (mime-entity-parsed-header-internal entity))
		(field (cdr (assq field-name header))))
	   (or field
	       (let ((field-body (mime-fetch-field field-name entity)))
		 (when field-body
		   (cond ((memq field-name '(From Resent-From
					     To Resent-To
					     Cc Resent-Cc
					     Bcc Resent-Bcc
					     Reply-To Resent-Reply-To))
			  (setq field (std11-parse-addresses
				       (eword-lexical-analyze field-body)))
			  )
			 ((memq field-name '(Sender Resent-Sender))
			  (setq field (std11-parse-address
				       (eword-lexical-analyze field-body)))
			  )
			 ((memq field-name eword-decode-ignored-field-list)
			  (setq field field-body))
			 ((memq field-name eword-decode-structured-field-list)
			  (setq field (eword-decode-structured-field-body
				       field-body)))
			 (t
			  (setq field (ew-decode-field (symbol-name field-name)
						       field-body))
			  ))
		   (mime-entity-set-parsed-header-internal
		    entity (put-alist field-name field header))
		   field)))))))

(mm-define-generic insert-decoded-header (entity &optional invisible-fields
					  visible-fields)
  "Insert before point a decoded header of ENTITY.")


;;; @ Entity Attributes
;;;

(defun mime-entity-uu-filename (entity)
  (if (member (mime-entity-encoding entity) mime-uuencode-encoding-name-list)
      (save-excursion
	(set-buffer (mime-entity-buffer entity))
	(goto-char (mime-entity-body-start entity))
	(if (re-search-forward "^begin [0-9]+ "
			       (mime-entity-body-end entity) t)
	    (if (looking-at ".+$")
		(buffer-substring (match-beginning 0)(match-end 0))
	      )))))

(defun mime-entity-filename (entity)
  "Return filename of ENTITY."
  (or (mime-entity-uu-filename entity)
      (mime-content-disposition-filename
       (mime-entity-content-disposition entity))
      (cdr (let ((param (mime-content-type-parameters
			 (mime-entity-content-type entity))))
	     (or (assoc "name" param)
		 (assoc "x-name" param))
	     ))))


(defsubst mime-entity-media-type (entity)
  (mime-content-type-primary-type (mime-entity-content-type entity)))
(defsubst mime-entity-media-subtype (entity)
  (mime-content-type-subtype (mime-entity-content-type entity)))
(defsubst mime-entity-parameters (entity)
  (mime-content-type-parameters (mime-entity-content-type entity)))
(defsubst mime-entity-type/subtype (entity-info)
  (mime-type/subtype-string (mime-entity-media-type entity-info)
			    (mime-entity-media-subtype entity-info)))


;;; @ Entity Content
;;;

(mm-define-generic entity-content (entity)
  "Return content of ENTITY as byte sequence (string).")

(mm-define-generic write-entity-content (entity filename)
  "Write content of ENTITY into FILENAME.")

(mm-define-generic write-entity (entity filename)
  "Write header and body of ENTITY into FILENAME.")

(mm-define-generic write-entity-body (entity filename)
  "Write body of ENTITY into FILENAME.")


;;; @ end
;;;

(provide 'mime)

;;; mime.el ends here
