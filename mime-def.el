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

(defconst mime-library-version-string "Chao 1.6.0 - \"Kuj,Dr(B\"")


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

(unless (fboundp 'butlast)
  (defun butlast (x &optional n)
    "Returns a copy of LIST with the last N elements removed."
    (if (and n (<= n 0)) x
      (nbutlast (copy-sequence x) n)))
  
  (defun nbutlast (x &optional n)
    "Modifies LIST to remove the last N elements."
    (let ((m (length x)))
      (or n (setq n 1))
      (and (< n m)
	   (progn
	     (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	     x))))
  )

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


;;; @@ Quoted-Printable
;;;

(defconst quoted-printable-hex-chars "0123456789ABCDEF")

(defconst quoted-printable-octet-regexp
  (concat "=[" quoted-printable-hex-chars
	  "][" quoted-printable-hex-chars "]"))


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

(defsubst make-mime-entity (buffer
			    header-start header-end body-start body-end
			    &optional node-id
			    content-type content-disposition
			    encoding children)
  (vector buffer header-start header-end body-start body-end
	  node-id content-type content-disposition encoding nil
	  children nil))

(defsubst mime-entity-buffer (entity)              (aref entity  0))
(defsubst mime-entity-header-start (entity)        (aref entity  1))
(defsubst mime-entity-header-end (entity)          (aref entity  2))
(defsubst mime-entity-body-start (entity)          (aref entity  3))
(defsubst mime-entity-body-end (entity)            (aref entity  4))
(defsubst mime-entity-node-id (entity)             (aref entity  5))
(defsubst mime-entity-content-type (entity)        (aref entity  6))
(defsubst mime-entity-content-disposition (entity) (aref entity  7))
(defsubst mime-entity-encoding (entity)            (aref entity  8))
(defsubst mime-entity-original-header (entity)     (aref entity  9))
(defsubst mime-entity-children (entity)            (aref entity 10))
(defsubst mime-entity-parsed-header (entity)       (aref entity 11))

(defsubst mime-entity-set-original-header (entity header)
  (aset entity  9 header))
(defsubst mime-entity-set-children (entity children)
  (aset entity 10 children))
(defsubst mime-entity-set-parsed-header (entity header)
  (aset entity 11 header))

(defsubst mime-entity-number (entity)
  (reverse (mime-entity-node-id entity)))

(defalias 'mime-entity-point-min 'mime-entity-header-start)
(defalias 'mime-entity-point-max 'mime-entity-body-end)

(defsubst mime-entity-media-type (entity)
  (mime-content-type-primary-type (mime-entity-content-type entity)))
(defsubst mime-entity-media-subtype (entity)
  (mime-content-type-subtype (mime-entity-content-type entity)))
(defsubst mime-entity-parameters (entity)
  (mime-content-type-parameters (mime-entity-content-type entity)))

(defsubst mime-entity-type/subtype (entity-info)
  (mime-type/subtype-string (mime-entity-media-type entity-info)
			    (mime-entity-media-subtype entity-info)))


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


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
