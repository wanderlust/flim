;;; digest-md5.el --- Compute DIGEST-MD5.

;; Copyright (C) 1999 Kenichi OKADA

;; Author: Kenichi OKADA <okada@opaopa.org>
;; Keywords: DIGEST-MD5, HMAC-MD5, SASL, IMAP, POP, ACAP

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; NOW BUILDING.

;; This program is implemented from draft-leach-digest-sasl-05.txt.
;;
;; It is caller's responsibility to base64-decode challenges and
;; base64-encode responses in IMAP4 AUTHENTICATE command.
;;
;; Passphrase should be longer than 16 bytes. (See RFC 2195)

;; Examples.
;;
;; (digest-md5-digest-response "chris" "elwood.innosoft.com"
;; 			  "OA6MG9tEQGm2hh" "OA6MHXh6VqTrRk"
;; 			  "imap/elwood.innosoft.com"
;;  			  "d388dad90d4bbd760a152321f2143af7"
;; 			  1 "auth" nil "utf-8")
;; => "charset=utf-8,username=\"chris\",realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",nc=00000001,cnonce=\"OA6MHXh6VqTrRk\",digest-uri=\"imap/elwood.innosoft.com\",response=d388dad90d4bbd760a152321f2143af7,qop=auth"
;;

;;; Code:

(require 'hmac-md5)
(require 'unique-id)

(defvar digest-md5-parse-digest-challenge-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?, "." table)
    table)
  "A syntax table for parsing sgml attributes.")

(defun digest-md5-parse-digest-challenge (digest-challenge)
  ;; return a property list of 
  ;; (realm nonce qop-options stale maxbuf charset 
  ;; algorithm cipher-opts auth-param).
  (with-temp-buffer
    (set-syntax-table digest-md5-parse-digest-challenge-syntax-table)
    (insert digest-challenge)
    (goto-char (point-min))
    (insert "(")
    (while (progn (forward-sexp) (not (eobp)))
      (delete-char 1)
      (insert " "))
    (insert ")")
    (condition-case nil
	(read (point-min-marker))
      (end-of-file
       (error "Parse error in digest-challenge.")))))

(defun digest-md5-digest-uri (serv-type host &optional serv-name)
  (concat serv-type "/" host
	  (if (and serv-name
		   (null (string= host serv-name)))
	      (concat "/" serv-name))))

(defun digest-md5-cnonce ()
  ;; It is RECOMMENDED that it 
  ;; contain at least 64 bits of entropy.
  (concat (unique-id-m "") (unique-id-m "")))

(defun digest-md5-digest-response (username 
				   realm nonce cnonce
				   digest-uri response 
				   &optional nonce-count qop 
				   maxbuf charset cipher authzid)
  (concat
   (if charset
       (concat "charset=" charset ","))
   "username=\"" username "\""
   ",realm=\"" realm "\""
   ",nonce=\"" nonce "\""
   (format ",nc=%08x"
	   (or nonce-count 1))
   ",cnonce=\"" cnonce "\""
   ",digest-uri=\"" digest-uri "\""
   ",response=" response
   (if qop
       (concat ",qop=" qop))
   (if maxbuf
       (concat ",maxbuf=" maxbuf))
   (if cipher
       (concat ",cipher=" cipher))
   (if authzid
       (concat ",authzid=\"" authzid "\""))))

  
(provide 'digest-md5)

;;; digest-md5.el ends here
