;;; sasl-digest.el --- DIGEST-MD5 module for the SASL client framework

;; Copyright (C) 2000 Daiki Ueno

;; Author: Kenichi OKADA <okada@opaopa.org>
;;	Daiki Ueno <ueno@unixuser.org>
;; Keywords: SASL, DIGEST-MD5

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This program is implemented from draft-leach-digest-sasl-05.txt.
;;
;; It is caller's responsibility to base64-decode challenges and
;; base64-encode responses in IMAP4 AUTHENTICATE command.
;;
;; Passphrase should be longer than 16 bytes. (See RFC 2195)

;;; Commentary:

(require 'sasl)
(require 'hmac-md5)

(defvar sasl-digest-md5-challenge nil)
(defvar sasl-digest-md5-nonce-count 1)
(defvar sasl-digest-md5-unique-id-function
  sasl-unique-id-function)

(defvar sasl-digest-md5-parse-digest-challenge-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?, "." table)
    table)
  "A syntax table for parsing digest-challenge attributes.")

(defconst sasl-digest-md5-steps
  '(ignore				;no initial response
    sasl-digest-md5-response
    ignore))				;""

;;; @ low level functions
;;;
;;; Examples in `draft-leach-digest-sasl-05.txt'.
;;;
;;; (sasl-digest-md5-parse-digest-challenge 
;;;   "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",qop=\"auth\",algorithm=md5-sess,charset=utf-8")
;;; => (realm "elwood.innosoft.com" nonce "OA6MG9tEQGm2hh" qop "auth" algorithm md5-sess charset utf-8)

;;; (sasl-digest-md5-build-response-value
;;;   "chris" "elwood.innosoft.com" "secret" "OA6MG9tEQGm2hh"
;;;   "OA6MHXh6VqTrRk" 1 "imap/elwood.innosoft.com" "auth")
;;; => "d388dad90d4bbd760a152321f2143af7"

(defun sasl-digest-md5-parse-digest-challenge (digest-challenge)
  "Return a property list parsed DIGEST-CHALLENGE.
The value is a cons cell of the form \(realm nonce qop-options stale maxbuf
charset algorithm cipher-opts auth-param)."
  (save-excursion
    (with-temp-buffer
      (set-syntax-table sasl-digest-md5-parse-digest-challenge-syntax-table)
      (insert digest-challenge)
      (goto-char (point-min))
      (insert "(")
      (while (progn (forward-sexp) (not (eobp)))
	(delete-char 1)
	(insert " "))
      (insert ")")
      (condition-case nil
	  (setplist 'sasl-digest-md5-challenge (read (point-min-marker)))
	(end-of-file
	 (error "Parse error in digest-challenge."))))))

(defun sasl-digest-md5-digest-uri (serv-type host &optional serv-name)
  (concat serv-type "/" host
	  (if (and serv-name
		   (null (string= host serv-name)))
	      (concat "/" serv-name))))

(defun sasl-digest-md5-cnonce ()
  (let ((sasl-unique-id-function sasl-digest-md5-unique-id-function))
    (sasl-unique-id)))

(defmacro sasl-digest-md5-challenge (prop)
  (list 'get ''sasl-digest-md5-challenge prop))

(defmacro sasl-digest-md5-build-response-value-1
  (username realm passwd nonce cnonce nonce-count digest-uri qop)
  `(encode-hex-string
    (md5-binary
     (concat
      (encode-hex-string
       (md5-binary (concat (md5-binary 
			    (concat ,username 
				    ":" ,realm
				    ":" ,passwd))
			   ":" ,nonce
			   ":" ,cnonce
			   (let ((authzid (sasl-digest-md5-challenge 'authzid)))
			     (if authzid (concat ":" authzid) nil)))))
      ":" ,nonce
      ":" (format "%08x" ,nonce-count) ":" ,cnonce ":" ,qop ":"
      (encode-hex-string
       (md5-binary
	(concat "AUTHENTICATE:" ,digest-uri
		(if (string-equal "auth-int" ,qop)
		    ":00000000000000000000000000000000"
		  nil))))))))

(defun sasl-digest-md5-build-response-value
  (username realm passwd nonce cnonce nonce-count digest-uri
	    &optional charset qop maxbuf cipher authzid)
  (concat
   "username=\"" username "\","
   "realm=\"" realm "\","
   "nonce=\"" nonce "\","
   (format "nc=%08x," nonce-count)
   "cnonce=\"" cnonce "\","
   "digest-uri=\"" digest-uri "\","
   "response=" 
   (sasl-digest-md5-build-response-value-1
    username realm passwd nonce cnonce nonce-count digest-uri
    (or qop "auth"))
   ","
   (mapconcat 
    #'identity
    (delq nil 
	  (mapcar (lambda (prop)
		    (if (sasl-digest-md5-challenge prop)
			(format "%s=%s"
				prop (sasl-digest-md5-challenge prop))))
		  '(charset qop maxbuf cipher authzid)))
    ",")))

(defun sasl-digest-md5-response (client step)
  (sasl-digest-md5-parse-digest-challenge (sasl-step-data step))
  (let ((passphrase
	 (sasl-read-passphrase
	  (format "DIGEST-MD5 passphrase for %s: "
		  (sasl-client-name client)))))
    (unwind-protect
	(sasl-digest-md5-build-response-value
	 (sasl-client-name client)
	 (or (sasl-client-property client 'realm)
	     (sasl-digest-md5-challenge 'realm))	;need to check
	 passphrase
	 (sasl-digest-md5-challenge 'nonce)
	 (sasl-digest-md5-cnonce)
	 sasl-digest-md5-nonce-count
	 (sasl-digest-md5-digest-uri
	  (sasl-client-service client)
	  (sasl-client-server client)))
      (fillarray passphrase 0))))

(put 'sasl-digest 'sasl-mechanism
     (sasl-make-mechanism "DIGEST-MD5" sasl-digest-md5-steps))

(provide 'sasl-digest)

;;; sasl-digest.el ends here
