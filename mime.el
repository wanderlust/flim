;;; mime.el --- MIME library module

;; Copyright (C) 1998,1999,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
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

(eval-when-compile (require 'mmgeneric))

(eval-and-compile

(autoload 'mime-encode-header-in-buffer "eword-encode"
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

(autoload 'mime-parse-msg-id "mime-parse"
  "Parse TOKENS as msg-id of Content-Id or Message-Id field.")

(autoload 'mime-uri-parse-cid "mime-parse"
  "Parse STRING as cid URI.")

(autoload 'mime-parse-buffer "mime-parse"
  "Parse BUFFER as a MIME message.")

)

(autoload 'mime-encode-field-body "eword-encode"
  "Encode FIELD-BODY as FIELD-NAME, and return the result.")


;;; @ Entity Representation and Implementation
;;;

(defmacro mime-entity-send (entity message &rest args)
  `(luna-send ,entity ',(intern (format "mime-%s" (eval message))) ,@args))

(defun mime-open-entity (type location)
  "Open an entity and return it.
TYPE is representation-type.
LOCATION is location of entity.  Specification of it is depended on
representation-type."
  (require (intern (format "mm%s" type)))
  (luna-make-entity (mm-expand-class-name type) :location location))

(luna-define-generic mime-entity-cooked-p (entity)
  "Return non-nil if contents of ENTITY has been already code-converted.")


;;; @ Entity as node of message
;;;

(defun mime-entity-children (entity)
  "Return list of entities included in the ENTITY."
  (or (mime-entity-children-internal entity)
      (luna-send entity 'mime-entity-children entity)))

(defun mime-entity-node-id (entity)
  "Return node-id of ENTITY."
  (mime-entity-node-id-internal entity))

(defun mime-entity-number (entity)
  "Return entity-number of ENTITY."
  (reverse (mime-entity-node-id-internal entity)))

(defun mime-find-entity-from-number (entity-number message)
  "Return entity from ENTITY-NUMBER in MESSAGE."
  (let ((sn (car entity-number)))
    (if (null sn)
	message
      (let ((rc (nth sn (mime-entity-children message))))
	(if rc
	    (mime-find-entity-from-number (cdr entity-number) rc)
	  ))
      )))

(defun mime-find-entity-from-node-id (entity-node-id message)
  "Return entity from ENTITY-NODE-ID in MESSAGE."
  (mime-find-entity-from-number (reverse entity-node-id) message))

(defun mime-find-entity-from-content-id (cid message)
  "Return entity from CID in MESSAGE."
  (if (equal cid (mime-entity-read-field message "Content-Id"))
      message
    (let ((children (mime-entity-children message))
	  ret)
      (while (and children
		  (null (setq ret (mime-find-entity-from-content-id
				   cid (car children)))))
	(setq children (cdr children)))
      ret)))

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

(defun mime-find-root-entity (entity)
  "Return root entity of ENTITY."
  (while (not (mime-root-entity-p entity))
    (setq entity (mime-entity-parent entity)))
  entity)


;;; @ Header buffer (obsolete)
;;;

;; (luna-define-generic mime-entity-header-buffer (entity))

;; (luna-define-generic mime-goto-header-start-point (entity)
;;   "Set buffer and point to header-start-position of ENTITY.")

;; (luna-define-generic mime-entity-header-start-point (entity)
;;   "Return header-start-position of ENTITY.")

;; (luna-define-generic mime-entity-header-end-point (entity)
;;   "Return header-end-position of ENTITY.")

;; (make-obsolete 'mime-entity-header-buffer "don't use it.")
;; (make-obsolete 'mime-goto-header-start-point "don't use it.")
;; (make-obsolete 'mime-entity-header-start-point "don't use it.")
;; (make-obsolete 'mime-entity-header-end-point "don't use it.")


;;; @ Body buffer (obsolete)
;;;

;; (luna-define-generic mime-entity-body-buffer (entity))

;; (luna-define-generic mime-goto-body-start-point (entity)
;;   "Set buffer and point to body-start-position of ENTITY.")

;; (luna-define-generic mime-goto-body-end-point (entity)
;;   "Set buffer and point to body-end-position of ENTITY.")

;; (luna-define-generic mime-entity-body-start-point (entity)
;;   "Return body-start-position of ENTITY.")

;; (luna-define-generic mime-entity-body-end-point (entity)
;;   "Return body-end-position of ENTITY.")

;; (defalias 'mime-entity-body-start 'mime-entity-body-start-point)
;; (defalias 'mime-entity-body-end 'mime-entity-body-end-point)

;; (make-obsolete 'mime-entity-body-buffer "don't use it.")
;; (make-obsolete 'mime-goto-body-start-point "don't use it.")
;; (make-obsolete 'mime-goto-body-end-point "don't use it.")
;; (make-obsolete 'mime-entity-body-start-point "don't use it.")
;; (make-obsolete 'mime-entity-body-end-point "don't use it.")
;; (make-obsolete 'mime-entity-body-start "don't use it.")
;; (make-obsolete 'mime-entity-body-end "don't use it.")


;;; @ Entity buffer (obsolete)
;;;

;; (luna-define-generic mime-entity-buffer (entity))
;; (make-obsolete 'mime-entity-buffer "don't use it.")

;; (luna-define-generic mime-entity-point-min (entity))
;; (make-obsolete 'mime-entity-point-min "don't use it.")

;; (luna-define-generic mime-entity-point-max (entity))
;; (make-obsolete 'mime-entity-point-max "don't use it.")


;;; @ Entity
;;;

(luna-define-generic mime-insert-entity (entity)
  "Insert header and body of ENTITY at point.")

(luna-define-generic mime-write-entity (entity filename)
  "Write header and body of ENTITY into FILENAME.")


;;; @ Entity Body
;;;

(luna-define-generic mime-entity-body (entity)
  "Return network representation of ENTITY body.")

(luna-define-generic mime-insert-entity-body (entity)
  "Insert network representation of ENTITY body at point.")

(luna-define-generic mime-write-entity-body (entity filename)
  "Write body of ENTITY into FILENAME.")


;;; @ Entity Content
;;;

(luna-define-generic mime-entity-content (entity)
  "Return content of ENTITY as byte sequence (string).")

(luna-define-generic mime-insert-entity-content (entity)
  "Insert content of ENTITY at point.")

(luna-define-generic mime-write-entity-content (entity filename)
  "Write content of ENTITY into FILENAME.")

(luna-define-generic mime-insert-text-content (entity)
  "Insert decoded text body of ENTITY.")


;;; @ Header fields
;;;

(luna-define-generic mime-entity-fetch-field (entity field-name)
  "Return the value of the ENTITY's header field whose type is FIELD-NAME.")

;; (defun mime-fetch-field (field-name &optional entity)
;;   "Return the value of the ENTITY's header field whose type is FIELD-NAME."
;;   (if (symbolp field-name)
;;       (setq field-name (symbol-name field-name))
;;     )
;;   (or entity
;;       (setq entity mime-message-structure))
;;   (mime-entity-fetch-field entity field-name)
;;   )
;; (make-obsolete 'mime-fetch-field 'mime-entity-fetch-field)

(defun mime-entity-content-type (entity)
  "Return content-type of ENTITY."
  (or (mime-entity-content-type-internal entity)
      (let ((ret (mime-entity-fetch-field entity "Content-Type")))
	(if ret
	    (mime-entity-set-content-type-internal
	     entity (mime-parse-Content-Type ret))
	  ))))

(defun mime-entity-content-disposition (entity)
  "Return content-disposition of ENTITY."
  (or (mime-entity-content-disposition-internal entity)
      (let ((ret (mime-entity-fetch-field entity "Content-Disposition")))
	(if ret
	    (mime-entity-set-content-disposition-internal
	     entity (mime-parse-Content-Disposition ret))
	  ))))

(defun mime-entity-encoding (entity &optional default-encoding)
  "Return content-transfer-encoding of ENTITY.
If the ENTITY does not have Content-Transfer-Encoding field, this
function returns DEFAULT-ENCODING.  If it is nil, \"7bit\" is used as
default value."
  (or (mime-entity-encoding-internal entity)
      (let ((ret (mime-entity-fetch-field entity "Content-Transfer-Encoding")))
	(mime-entity-set-encoding-internal
	 entity
	 (or (and ret (mime-parse-Content-Transfer-Encoding ret))
	     default-encoding "7bit"))
	)))

(defvar mime-field-parser-alist
  '((Return-Path	. std11-parse-route-addr)
    
    (Reply-To 		. std11-parse-addresses)
    
    (Sender		. std11-parse-mailbox)
    (From		. std11-parse-addresses)

    (Resent-Reply-To	. std11-parse-addresses)
    
    (Resent-Sender	. std11-parse-mailbox)
    (Resent-From	. std11-parse-addresses)

    (To			. std11-parse-addresses)
    (Resent-To		. std11-parse-addresses)
    (Cc			. std11-parse-addresses)
    (Resent-Cc		. std11-parse-addresses)
    (Bcc		. std11-parse-addresses)
    (Resent-Bcc		. std11-parse-addresses)
    
    (Message-Id		. mime-parse-msg-id)
    (Recent-Message-Id	. mime-parse-msg-id)
    
    (In-Reply-To	. std11-parse-msg-ids)
    (References		. std11-parse-msg-ids)
    
    (Content-Id		. mime-parse-msg-id)
    ))

(defun mime-entity-read-field (entity field-name)
  (let ((sym (if (symbolp field-name)
		 (prog1
		     field-name
		   (setq field-name (symbol-name field-name)))
	       (intern (capitalize field-name)))))
    (cond ((eq sym 'Content-Type)
	   (mime-entity-content-type entity)
	   )
	  ((eq sym 'Content-Disposition)
	   (mime-entity-content-disposition entity)
	   )
	  ((eq sym 'Content-Transfer-Encoding)
	   (mime-entity-encoding entity)
	   )
	  (t
	   (let* ((header (mime-entity-parsed-header-internal entity))
		  (field (cdr (assq sym header))))
	     (or field
		 (let ((field-body (mime-entity-fetch-field entity field-name))
		       parser)
		   (when field-body
		     (setq parser
			   (cdr (assq sym mime-field-parser-alist)))
		     (setq field
			   (if parser
			       (funcall parser
					(eword-lexical-analyze field-body))
			     (mime-decode-field-body field-body sym 'plain)
			     ))
		     (mime-entity-set-parsed-header-internal
		      entity (put-alist sym field header))
		     field))))))))

;; (defun mime-read-field (field-name &optional entity)
;;   (or entity
;;       (setq entity mime-message-structure))
;;   (mime-entity-read-field entity field-name)
;;   )
;; (make-obsolete 'mime-read-field 'mime-entity-read-field)

(luna-define-generic mime-insert-header (entity &optional invisible-fields
						visible-fields)
  "Insert before point a decoded header of ENTITY.")


;;; @ Entity Attributes
;;;

(luna-define-generic mime-entity-name (entity)
  "Return name of the ENTITY.")

(defun mime-entity-uu-filename (entity)
  (if (member (mime-entity-encoding entity) mime-uuencode-encoding-name-list)
      (with-temp-buffer
	(mime-insert-entity-body entity)
	(if (re-search-forward "^begin [0-9]+ " nil t)
	    (if (looking-at ".+$")
		(buffer-substring (match-beginning 0)(match-end 0))
	      )))))

;; unlimited patch by simm-emacs@fan.gr.jp
;;   Mon, 10 Jan 2000 12:59:46 +0900
(defun mime-entity-filename (entity)
  "Return filename of ENTITY."
  (let ((filename
	 (or (mime-entity-uu-filename entity)
	     (mime-content-disposition-filename
	      (mime-entity-content-disposition entity))
	     (cdr (let ((param (mime-content-type-parameters
				(mime-entity-content-type entity))))
		    (or (assoc "name" param)
			(assoc "x-name" param))))
	     "")))
    (or (and mime-decode-unlimited
	     (string-match "\033" filename)
	     (decode-mime-charset-string filename 'iso-2022-jp 'CRLF))
	(eword-decode-string filename))))


(defsubst mime-entity-media-type (entity)
  "Return primary media-type of ENTITY."
  (mime-content-type-primary-type (mime-entity-content-type entity)))

(defsubst mime-entity-media-subtype (entity)
  "Return media-subtype of ENTITY."
  (mime-content-type-subtype (mime-entity-content-type entity)))

(defsubst mime-entity-parameters (entity)
  "Return parameters of Content-Type of ENTITY."
  (mime-content-type-parameters (mime-entity-content-type entity)))

(defsubst mime-entity-type/subtype (entity-info)
  "Return type/subtype of Content-Type of ENTITY."
  (mime-type/subtype-string (mime-entity-media-type entity-info)
			    (mime-entity-media-subtype entity-info)))

(defun mime-entity-set-content-type (entity content-type)
  "Set ENTITY's content-type to CONTENT-TYPE."
  (mime-entity-set-content-type-internal entity content-type))

(defun mime-entity-set-encoding (entity encoding)
  "Set ENTITY's content-transfer-encoding to ENCODING."
  (mime-entity-set-encoding-internal entity encoding))


;;; @ unlimited patch
;;;

;; unlimited patch by simm-emacs@fan.gr.jp (code derives from irchat-pj)
;;   Tue, 01 Feb 2000 01:42:05 +0900
(defun mime-detect-coding-system-region-unlimited (beg end)
  "Detect coding system on region."
  (let (ch esc prev flag)
    (save-excursion
      (catch 'detect
	;; check ISO-2022-JP / ascii
	(catch 'quit
	  (goto-char beg)
	  (while (< (point) end)
	    (setq ch (following-char))
	    (and (<= 256 ch)
		 (throw 'detect nil)) ;;'noconv))
	    (and (<= 128 ch)
		 (throw 'quit t))
	    (and (= 27 ch)
		 (setq esc t))
	    (forward-char 1))
	  (throw 'detect (if esc 'iso-2022-jp nil))) ;;'noconv)))
	;; check EUC-JP / SHIFT-JIS
	(if esc (throw 'detect 'iso-2022-jp))
	(while (< (point) end)
	  (setq ch (following-char))
	  (or (and (= 27 ch)                        ;; ESC
		   (throw 'detect 'iso-2022-jp))
	      (and (<= 128 ch) (<= ch 141)          ;; 0x80 <= ch <= 0x8d
		   (throw 'detect 'shift_jis))
	      (and (<= 144 ch) (<= ch 159)          ;; 0x90 <= ch <= 0x9f
		   (throw 'detect 'shift_jis))
	      (and (eq 'shift_jis prev) (<= ch 127) ;; second byte MSB == 0
		   (throw 'detect 'shift_jis))
	      (and (eq 'euc-jp prev)
		   (<= 161 ch) (<= ch 243)          ;; second byte of EUC Kana
		   (setq prev nil
			 flag 'euc-jp))
	      (and (eq nil prev)
		   (or (= 164 ch) (= 165 ch))       ;; first byte of EUC kana
		   (setq prev 'euc-jp))
	      (< ch 160)                            ;;         ch <= 0xa0
	      (and (eq 'euc-jp prev)
		   (throw 'detect 'euc-jp))
	      (setq prev (if prev nil 'shift_jis)
		    flag (if (eq 'euc-jp flag) 'euc-jp 'shift_jis)))
	  (forward-char 1))
	flag))))
	;;(or flag 'noconv)))))

;; unlimited patch by simm-emacs@fan.gr.jp
;;   Tue, 01 Feb 2000 01:56:38 +0900
(defun mime-detect-coding-system-string-unlimited (str)
  "Detect coding system on string."
  (save-excursion
    (set-buffer (get-buffer-create " *Temporary unlimited*"))
    (insert str)
    (unwind-protect
	(mime-detect-coding-system-region-unlimited (point-min) (point-max))
      (kill-buffer nil))))

;; unlimited patch by simm-emacs@fan.gr.jp
;;   Tue, 01 Feb 2000 13:32:14 +0900
(defsubst insert-unlimited (str)
  "Insert with no-conversion.
On GNU Emacs 20.*, (insert str) after (set-buffer-multibyte nil).
Other environment, perform (insert str)."
  (static-if (boundp 'nonascii-translation-table-unlimited)
      (let ((nonascii-translation-table nonascii-translation-table-unlimited))
	(insert str))
    (insert str)))

(defun decode-mime-charset-string-dist-unlimited (str charset &optional lbt)
  "Detect coding system on string."
  (if (not (eq 'auto-detect charset))
      (decode-mime-charset-string str charset lbt)
    (save-excursion
      (set-buffer (get-buffer-create " *Temporary unlimited*"))
      (unwind-protect
	  (let (code)
	    (insert-unlimited str)
	    (setq code (mime-detect-coding-system-region-unlimited (point-min) (point-max)))
	    (cond ((eq code 'euc-jp)
		   (message "EUC-JP code detected, so convert this message."))
		  ((eq code 'shift_jis)
		   (message "SHIFT-JIS code detected, so convert this message.")))
	    (decode-mime-charset-region (point-min) (point-max)
					(or code default-mime-charset)
					lbt)
	    (buffer-substring (point-min) (point-max)))
	(kill-buffer nil)))))

(defun decode-mime-charset-string-unlimited (str charset &optional lbt)
  "Detect coding system on string."
  (cond ((eq 'auto-detect charset)
	 (save-excursion
	   (set-buffer (get-buffer-create " *Temporary unlimited*"))
	   (unwind-protect
	       (let (code)
		 (insert-unlimited str)
		 (setq code
		       (mime-detect-coding-system-region-unlimited (point-min) (point-max)))
		 (cond ((eq code 'euc-jp)
			(message "EUC-JP code detected, so convert this message."))
		       ((eq code 'shift_jis)
			(message "SHIFT-JIS code detected, so convert this message.")))
		 (decode-mime-charset-region (point-min) (point-max)
					     (or code default-mime-charset)
					     lbt)
		 (buffer-substring (point-min) (point-max)))
	     (kill-buffer nil))))
	((string= "us-ascii" charset)
	 (save-excursion
	   (set-buffer (get-buffer-create " *Temporary unlimited*"))
	   (unwind-protect
	       (let ((code 'us-ascii))
		 (insert-unlimited str)
		 (goto-char (point-min))
		 (while (not (eobp))
		   (if (and (<= 32 (following-char)) (< (following-char) 128))
		       (forward-char 1)
		     (setq code nil)
		     (goto-char (point-max))))
		 (cond ((eq code 'us-ascii)
			(decode-mime-charset-region (point-min) (point-max) nil lbt))
		       (code
			(decode-mime-charset-region (point-min) (point-max) code lbt))
		       (t
			(setq code
			      (mime-detect-coding-system-region-unlimited
			       (point-min) (point-max)))
			(when code
			  (message "Declared US-ASCII but detected %s, so convert."
				   (if (eq code 'shift_jis) "SHIFT-JIS"
				     (upcase (prin1-to-string code))))
			  (decode-mime-charset-region (point-min) (point-max)
						      (or code default-mime-charset)
						      lbt))))
		 (buffer-substring (point-min) (point-max)))
	     (kill-buffer nil))))
	(t
	 (decode-mime-charset-string str charset lbt))))

;;; @ end
;;;

(provide 'mime)

;;; mime.el ends here
