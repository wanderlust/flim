;;; sasl.el --- basic functions for SASL

;; Copyright (C) 1995, 1996, 1998, 1999 Free Software Foundation, Inc.

;; Author: Kenichi OKADA <okada@opaopa.org>
;; Keywords: SMTP, SASL, RFC2222

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

;;; Code:

(require 'hmac-md5)

;;; CRAM-MD5
(defun sasl-cram-md5 (username passphrase challenge)
  (let ((secure-word (copy-sequence passphrase)))
    (setq secure-word (unwind-protect
			  (hmac-md5 challenge secure-word)
			(fillarray secure-word 0))
	  secure-word (unwind-protect
			  (encode-hex-string secure-word)
			(fillarray secure-word 0))
	  secure-word (unwind-protect
			  (concat username " " secure-word)
			(fillarray secure-word 0)))))

;;; PLAIN
(defun sasl-plain (authorid authenid passphrase)
  (concat authorid "\0" authenid "\0" passphrase))

;;; SCRAM-MD5
(defvar sasl-scram-md5-client-security-info
  (scram-make-security-info nil t 0))

(defun sasl-scram-md5-client-msg-1 (authenticate-id &optional authorize-id)
  (scram-md5-make-client-msg-1 authenticate-id authorize-id))

(defun sasl-scram-md5-client-msg-2 (server-msg-1 client-msg-1 passphrase)
  (let (client-key)
    (scram-md5-make-client-msg-2
     sasl-scram-md5-client-security-info
     (scram-md5-make-client-proof
      (setq client-key
	    (scram-md5-make-client-key
	     (scram-md5-make-salted-pass
	      passphrase
	      (car ; salt
	       (scram-md5-parse-server-msg-1 server-msg-1)))))
      (scram-md5-make-shared-key
       server-msg-1
       client-msg-1
       sasl-scram-md5-client-security-info
       (scram-md5-make-client-verifier client-key))))))

(defun sasl-scram-md5-authenticate-server (server-msg-1
					   server-msg-2
					   client-msg-1
					   passphrase)
  (scram-md5-authenticate-server
   server-msg-1
   server-msg-2
   client-msg-1
   sasl-scram-md5-client-security-info
   (car ; salt
    (scram-md5-parse-server-msg-1 server-msg-1))
   (scram-md5-make-salted-pass
    passphrase
    (car ; salt
     (scram-md5-parse-server-msg-1 server-msg-1)))))

;;; unique-ID
(defun sasl-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (sasl-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

(defvar sasl-unique-id-char nil)

(defun sasl-unique-id ()
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq sasl-unique-id-char
	(% (1+ (or sasl-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (static-if (fboundp 'current-time)
		(current-time)
	      (let* ((cts (split-string (current-time-string) "[ :]"))
		     (m (cdr (assoc (nth 1 cts)
				    '(("Jan" . "01") ("Feb" . "02")
				      ("Mar" . "03") ("Apr" . "04")
				      ("May" . "05") ("Jun" . "06")
				      ("Jul" . "07") ("Aug" . "08")
				      ("Sep" . "09") ("Oct" . "10")
				      ("Nov" . "11") ("Dec" . "12"))))))
		(list (string-to-int (concat (nth 6 cts) m
					     (substring (nth 2 cts) 0 1)))
		      (string-to-int (concat (substring (nth 2 cts) 1)
					     (nth 4 cts) (nth 5 cts)
					     (nth 6 cts))))))))
    (concat
     (if (memq system-type '(ms-dos emx vax-vms))
	 (let ((user (downcase (user-login-name))))
	   (while (string-match "[^a-z0-9_]" user)
	     (aset user (match-beginning 0) ?_))
	   user)
       (sasl-number-base36 (user-uid) -1))
     (sasl-number-base36 (+ (car   tm)
			  (lsh (% sasl-unique-id-char 25) 16)) 4)
     (sasl-number-base36 (+ (nth 1 tm)
			  (lsh (/ sasl-unique-id-char 25) 16)) 4)
     ;; Append the name of the message interface, because while the
     ;; generated ID is unique to this newsreader, other newsreaders
     ;; might otherwise generate the same ID via another algorithm.
     ".sasl")))

(provide 'sasl)

;;; sasl.el ends here
