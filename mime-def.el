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

(defconst mime-library-version-string "FLIM 1.4.1 - \"Momoyama-Gory.DNrmae\"")


;;; @ variables
;;;

(require 'custom)

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
	   (regexp-or std11-qtext-regexp std11-quoted-pair-regexp)
	   )
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


;;; @ MIME-entity
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
  (aset entity 9 header))
(defsubst mime-entity-set-parsed-header (entity header)
  (aset entity 11 header))

(defsubst mime-entity-number (entity)
  (reverse (mime-entity-node-id entity)))


;;; @ utility
;;;

(defsubst mime-type/subtype-string (type &optional subtype)
  "Return type/subtype string from TYPE and SUBTYPE."
  (if type
      (if subtype
	  (format "%s/%s" type subtype)
	(format "%s" type))))


;;; @ end
;;;

(provide 'mime-def)

;;; mime-def.el ends here
