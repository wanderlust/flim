;;; mime-parse.el --- MIME message parser

;; Copyright (C) 1994,1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: parse, MIME, multimedia, mail, news

;; This file is part of SEMI (Spadework for Emacs MIME Interfaces).

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

(require 'std11)
(require 'mime-def)

(eval-when-compile (require 'cl))


;;; @ lexical analyzer
;;;

(defcustom mime-lexical-analyzer
  '(std11-analyze-quoted-string
    std11-analyze-domain-literal
    std11-analyze-comment
    std11-analyze-spaces
    mime-analyze-tspecial
    mime-analyze-token)
  "*List of functions to return result of lexical analyze.
Each function must have two arguments: STRING and START.
STRING is the target string to be analyzed.
START is start position of STRING to analyze.

Previous function is preferred to next function.  If a function
returns nil, next function is used.  Otherwise the return value will
be the result."
  :group 'mime
  :type '(repeat function))

(defun mime-analyze-tspecial (string start)
  (if (and (> (length string) start)
	   (memq (aref string start) mime-tspecial-char-list))
      (cons (cons 'tpecials (substring string start (1+ start)))
	    (1+ start))
    ))

(defun mime-analyze-token (string start)
  (if (and (string-match mime-token-regexp string start)
	   (= (match-beginning 0) start))
      (let ((end (match-end 0)))
	(cons (cons 'mime-token (substring string start end))
	      ;;(substring string end)
	      end)
	)))


;;; @ field parser
;;;

(defconst mime/content-parameter-value-regexp
  (concat "\\("
	  std11-quoted-string-regexp
	  "\\|[^; \t\n]*\\)"))

(defconst mime::parameter-regexp
  (concat "^[ \t]*\;[ \t]*\\(" mime-token-regexp "\\)"
	  "[ \t]*=[ \t]*\\(" mime/content-parameter-value-regexp "\\)"))

(defun mime-parse-parameter (str)
  (if (string-match mime::parameter-regexp str)
      (let ((e (match-end 2)))
	(cons
	 (cons (downcase (substring str (match-beginning 1) (match-end 1)))
	       (std11-strip-quoted-string
		(substring str (match-beginning 2) e))
	       )
	 (substring str e)
	 ))))


;;; @ Content-Type
;;;

;;;###autoload
(defun mime-parse-Content-Type (string)
  "Parse STRING as field-body of Content-Type field.
Return value is
    (PRIMARY-TYPE SUBTYPE (NAME1 . VALUE1)(NAME2 . VALUE2) ...)
or nil.  PRIMARY-TYPE and SUBTYPE are symbol and NAME_n and VALUE_n
are string."
  (setq string (std11-unfold-string string))
  (if (string-match `,(concat "^\\(" mime-token-regexp
			      "\\)/\\(" mime-token-regexp "\\)") string)
      (let* ((type (downcase
		    (substring string (match-beginning 1) (match-end 1))))
	     (subtype (downcase
		       (substring string (match-beginning 2) (match-end 2))))
	     ret dest)
	(setq string (substring string (match-end 0)))
	(while (setq ret (mime-parse-parameter string))
	  (setq dest (cons (car ret) dest)
		string (cdr ret))
	  )
	(make-mime-content-type (intern type)(intern subtype)
				(nreverse dest))
	)))

;;;###autoload
(defun mime-read-Content-Type ()
  "Read field-body of Content-Type field from current-buffer,
and return parsed it.  Format of return value is as same as
`mime-parse-Content-Type'."
  (let ((str (std11-field-body "Content-Type")))
    (if str
	(mime-parse-Content-Type str)
      )))


;;; @ Content-Disposition
;;;

(eval-and-compile
  (defconst mime-disposition-type-regexp mime-token-regexp)
  )

;;;###autoload
(defun mime-parse-Content-Disposition (string)
  "Parse STRING as field-body of Content-Disposition field."
  (setq string (std11-unfold-string string))
  (if (string-match (eval-when-compile
		      (concat "^" mime-disposition-type-regexp)) string)
      (let* ((e (match-end 0))
	     (type (downcase (substring string 0 e)))
	     ret dest)
	(setq string (substring string e))
	(while (setq ret (mime-parse-parameter string))
	  (setq dest (cons (car ret) dest)
		string (cdr ret))
	  )
	(cons (cons 'type (intern type))
	      (nreverse dest))
	)))

;;;###autoload
(defun mime-read-Content-Disposition ()
  "Read field-body of Content-Disposition field from current-buffer,
and return parsed it."
  (let ((str (std11-field-body "Content-Disposition")))
    (if str
	(mime-parse-Content-Disposition str)
      )))


;;; @ Content-Transfer-Encoding
;;;

;;;###autoload
(defun mime-parse-Content-Transfer-Encoding (string)
  "Parse STRING as field-body of Content-Transfer-Encoding field."
  (let ((tokens (std11-lexical-analyze string mime-lexical-analyzer))
	token)
    (while (and tokens
		(setq token (car tokens))
		(std11-ignored-token-p token))
      (setq tokens (cdr tokens)))
    (if token
	(if (eq (car token) 'mime-token)
	    (downcase (cdr token))
	  ))))

;;;###autoload
(defun mime-read-Content-Transfer-Encoding (&optional default-encoding)
  "Read field-body of Content-Transfer-Encoding field from
current-buffer, and return it.
If is is not found, return DEFAULT-ENCODING."
  (let ((str (std11-field-body "Content-Transfer-Encoding")))
    (if str
	(mime-parse-Content-Transfer-Encoding str)
      default-encoding)))


;;; @ Content-Id / Message-Id
;;;

;;;###autoload
(defun mime-parse-msg-id (tokens)
  "Parse TOKENS as msg-id of Content-Id or Message-Id field."
  (car (std11-parse-msg-id tokens)))

;;;###autoload
(defun mime-uri-parse-cid (string)
  "Parse STRING as cid URI."
  (inline
    (mime-parse-msg-id (cons '(specials . "<")
			     (nconc
			      (cdr (cdr (std11-lexical-analyze string)))
			      '((specials . ">")))))))


;;; @ message parser
;;;

(defun mime-parse-multipart (entity)
  (goto-char (point-min))
  (let* ((representation-type
	  (mime-entity-representation-type-internal entity))
	 (content-type (mime-entity-content-type-internal entity))
	 (dash-boundary
	  (concat "--" (mime-content-type-parameter content-type "boundary")))
	 (delimiter       (concat "\n" (regexp-quote dash-boundary)))
	 (close-delimiter (concat delimiter "--[ \t]*$"))
	 (rsep (concat delimiter "[ \t]*\n"))
	 (dc-ctl
	  (if (eq (mime-content-type-subtype content-type) 'digest)
	      (make-mime-content-type 'message 'rfc822)
	    (make-mime-content-type 'text 'plain)
	    ))
	 (header-end (mime-entity-header-end-internal entity))
	 (body-end (mime-entity-body-end-internal entity)))
    (save-restriction
      (goto-char body-end)
      (narrow-to-region header-end
			(if (re-search-backward close-delimiter nil t)
			    (match-beginning 0)
			  body-end))
      (goto-char header-end)
      (if (re-search-forward rsep nil t)
	  (let ((cb (match-end 0))
		ce ncb ret children
		(node-id (mime-entity-node-id-internal entity))
		(i 0))
	    (while (re-search-forward rsep nil t)
	      (setq ce (match-beginning 0))
	      (setq ncb (match-end 0))
	      (save-restriction
		(narrow-to-region cb ce)
		(setq ret (mime-parse-message representation-type dc-ctl
					      entity (cons i node-id)))
		)
	      (setq children (cons ret children))
	      (goto-char (setq cb ncb))
	      (setq i (1+ i))
	      )
	    (setq ce (point-max))
	    (save-restriction
	      (narrow-to-region cb ce)
	      (setq ret (mime-parse-message representation-type dc-ctl
					    entity (cons i node-id)))
	      )
	    (setq children (cons ret children))
	    (mime-entity-set-children-internal entity (nreverse children))
	    )
	(mime-entity-set-content-type-internal
	 entity (make-mime-content-type 'message 'x-broken))
	nil)
      )))

(defun mime-parse-encapsulated (entity)
  (mime-entity-set-children-internal
   entity
   (save-restriction
     (narrow-to-region (mime-entity-body-start-internal entity)
		       (mime-entity-body-end-internal entity))
     (list (mime-parse-message
	    (mime-entity-representation-type-internal entity) nil
	    entity (cons 0 (mime-entity-node-id-internal entity))))
     )))

(defun mime-parse-message (representation-type &optional default-ctl 
					       parent node-id)
  (let ((header-start (point-min))
	header-end
	body-start
	(body-end (point-max))
	content-type)
    (goto-char header-start)
    (if (re-search-forward "^$" nil t)
	(setq header-end (match-end 0)
	      body-start (if (= header-end body-end)
			     body-end
			   (1+ header-end)))
      (setq header-end (point-min)
	    body-start (point-min)))
    (save-restriction
      (narrow-to-region header-start header-end)
      (setq content-type (or (let ((str (std11-fetch-field "Content-Type")))
			       (if str
				   (mime-parse-Content-Type str)
				 ))
			     default-ctl))
      )
    (make-mime-entity-internal representation-type
			       (current-buffer)
			       content-type nil parent node-id
			       nil nil nil nil
			       nil nil nil nil
			       nil nil
			       (current-buffer)
			       header-start header-end
			       body-start body-end)
    ))


;;; @ for buffer
;;;

;;;###autoload
(defun mime-parse-buffer (&optional buffer representation-type)
  "Parse BUFFER as a MIME message.
If buffer is omitted, it parses current-buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (setq mime-message-structure
	  (mime-parse-message (or representation-type 'buffer) nil))
    ))


;;; @ end
;;;

(provide 'mime-parse)

;;; mime-parse.el ends here
