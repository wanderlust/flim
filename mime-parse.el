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



;;; @ end
;;;

(provide 'mime-parse)

;;; mime-parse.el ends here
