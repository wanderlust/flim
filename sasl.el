;;; sasl.el --- SASL client framework

;; Copyright (C) 2000 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: SASL

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

;;; Commentary:
;;

;;; Code:

(require 'poe)

(defvar sasl-mechanisms
  '("CRAM-MD5" "DIGEST-MD5" "PLAIN" "LOGIN" "ANONYMOUS"))

(defvar sasl-mechanism-alist
  '(("CRAM-MD5" sasl-cram)
    ("DIGEST-MD5" sasl-digest)
    ("PLAIN" sasl-plain)
    ("LOGIN" sasl-login)
    ("ANONYMOUS" sasl-anonymous)))

(defvar sasl-unique-id-function #'sasl-unique-id-function)

(defmacro sasl-authenticator-mechanism (authenticator)
  `(aref ,authenticator 0))

(defmacro sasl-authenticator-continuations (authenticator)
  `(aref ,authenticator 1))

(defmacro sasl-make-principal (name service server &optional realm)
  `(vector ,name ,realm ,service ,server))

(defmacro sasl-principal-name (principal)
  `(aref ,principal 0))

(defmacro sasl-principal-realm (principal)
  `(aref ,principal 1))

(defmacro sasl-principal-service (principal)
  `(aref ,principal 2))

(defmacro sasl-principal-server (principal)
  `(aref ,principal 3))

(put 'sasl-error 'error-message "SASL error")
(put 'sasl-error 'error-conditions '(sasl-error error))

(defun sasl-error (datum)
  (signal 'sasl-error (list datum)))

(defun sasl-make-authenticator (mechanism continuations)
  "Make an authenticator.
MECHANISM is a IANA registered SASL mechanism name.
CONTINUATIONS is list of continuation function."
  (vector mechanism
	  (mapcar
	   (lambda (continuation)
	     (let ((symbol (make-symbol (symbol-name continuation))))
	       (fset symbol (symbol-function continuation))
	       symbol))
	   continuations)))

(defun sasl-find-authenticator (mechanisms)
  "Retrieve an apropriate authenticator object from MECHANISMS hints."
  (let* ((sasl-mechanisms sasl-mechanisms)
	 (mechanism
	  (catch 'done
	    (while sasl-mechanisms
	      (if (member (car sasl-mechanisms) mechanisms)
		  (throw 'done (nth 1 (assoc (car sasl-mechanisms)
					     sasl-mechanism-alist))))
	      (setq sasl-mechanisms (cdr sasl-mechanisms))))))
    (when mechanism
      (require mechanism)
      (get mechanism 'sasl-authenticator))))

(defun sasl-evaluate-challenge (authenticator principal &optional challenge)
  "Evaluate the challenge and prepare an appropriate next response.
The data type of the value and optional 3rd argument CHALLENGE is nil or
a cons cell of the form \(CONTINUATION STRING).
At the first time CONTINUATION should be set to nil.

Argument AUTHENTICATOR is the current evaluator.
Argument PRINCIPAL is the client principal."
  (let* ((continuations
	  (sasl-authenticator-continuations authenticator))
	 (function
	  (if (car challenge)
	      (nth 1 (memq (car challenge) continuations))
	    (car continuations))))
    (if function
	(list function (funcall function principal challenge)))))

(defvar sasl-read-passphrase nil)
(defun sasl-read-passphrase (prompt &optional key)
  (if (not sasl-read-passphrase)
      (if (functionp 'read-passwd)
	  (setq sasl-read-passphrase 'read-passwd)
	(if (load "passwd" t)
	    (setq sasl-read-passphrase 'read-passwd)
	  (autoload 'ange-ftp-read-passwd "ange-ftp")
	  (setq sasl-read-passphrase 'ange-ftp-read-passwd))))
  (funcall sasl-read-passphrase prompt))

(defun sasl-unique-id ()
  "Compute a data string which must be different each time.
It contain at least 64 bits of entropy."
  (concat (funcall sasl-unique-id-function)(funcall sasl-unique-id-function)))

(defvar sasl-unique-id-char nil)

;; stolen (and renamed) from message.el
(defun sasl-unique-id-function ()
  ;; Don't use microseconds from (current-time), they may be unsupported.
  ;; Instead we use this randomly inited counter.
  (setq sasl-unique-id-char
	(% (1+ (or sasl-unique-id-char (logand (random t) (1- (lsh 1 20)))))
	   ;; (current-time) returns 16-bit ints,
	   ;; and 2^16*25 just fits into 4 digits i base 36.
	   (* 25 25)))
  (let ((tm (current-time)))
    (concat
     (sasl-unique-id-number-base36
      (+ (car   tm)
	 (lsh (% sasl-unique-id-char 25) 16)) 4)
     (sasl-unique-id-number-base36
      (+ (nth 1 tm)
	 (lsh (/ sasl-unique-id-char 25) 16)) 4))))

(defun sasl-unique-id-number-base36 (num len)
  (if (if (< len 0)
	  (<= num 0)
	(= len 0))
      ""
    (concat (sasl-unique-id-number-base36 (/ num 36) (1- len))
	    (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
				  (% num 36))))))

;;; PLAIN (RFC2595 Section 6)
(defconst sasl-plain-continuations
  '(sasl-plain-response))

(defun sasl-plain-response (principal challenge)
  (let ((passphrase
	 (sasl-read-passphrase
	  (format "PLAIN passphrase for %s: " (sasl-principal-name principal)))))
    (unwind-protect
	(concat "\0" (sasl-principal-name principal) "\0" passphrase)
      (fillarray passphrase 0))))

(put 'sasl-plain 'sasl-authenticator
     (sasl-make-authenticator "PLAIN" sasl-plain-continuations))

(provide 'sasl-plain)

;;; LOGIN (No specification exists)
(defconst sasl-login-continuations
  '(ignore				;no initial response
    sasl-login-response-1
    sasl-login-response-2))

(defun sasl-login-response-1 (principal challenge)
  (unless (string= (nth 1 challenge) "Username:")
    (sasl-error (format "Unexpected response: %s" (nth 1 challenge))))
  (sasl-principal-name principal))

(defun sasl-login-response-2 (principal challenge)
  (unless (string= (nth 1 challenge) "Password:")
    (sasl-error (format "Unexpected response: %s" (nth 1 challenge))))
  (sasl-read-passphrase
   (format "LOGIN passphrase for %s: " (sasl-principal-name principal))))

(put 'sasl-login 'sasl-authenticator
     (sasl-make-authenticator "LOGIN" sasl-login-continuations))

(provide 'sasl-login)

;;; ANONYMOUS (RFC2245)
(defconst sasl-anonymous-continuations
  '(identity				;no initial response
    sasl-anonymous-response))

(defun sasl-anonymous-response (principal challenge)
  (concat (sasl-principal-name principal)))

(put 'sasl-anonymous 'sasl-authenticator
     (sasl-make-authenticator "ANONYMOUS" sasl-anonymous-continuations))

(provide 'sasl-anonymous)

(provide 'sasl)

;;; sasl.el ends here
