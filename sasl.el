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

;; This module provides common interface functions to share several
;; SASL mechanism drivers.  The toplevel is designed to be mostly
;; compatible with [Java-SASL].
;;
;; [SASL] J. Myers, "Simple Authentication and Security Layer (SASL)",
;;	RFC 2222, October 1997.
;;
;; [Java-SASL] R. Weltman & R. Lee, "The Java SASL Application Program
;;	Interface", draft-weltman-java-sasl-03.txt, March 2000.

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

(put 'sasl-error 'error-message "SASL error")
(put 'sasl-error 'error-conditions '(sasl-error error))

(defun sasl-error (datum)
  (signal 'sasl-error (list datum)))

;;; @ SASL instantiator
;;;

(defmacro sasl-make-instantiator (name service server)
  "Return a newly allocated SASL instantiator.
NAME is name of the authorization.  SERVICE is name of the service desired.
SERVER is the fully qualified host name of the server to authenticate to."
  (let ((props (make-symbol "sasl-instantiator-properties")))
    `(vector ,name ,service ,server ',props)))

(defmacro sasl-instantiator-name (instantiator)
  "Return the authorization name of INSTANTIATOR, a string."
  `(aref ,instantiator 0))

(defmacro sasl-instantiator-service (instantiator)
  "Return the service name of INSTANTIATOR, a string."
  `(aref ,instantiator 1))

(defmacro sasl-instantiator-server (instantiator)
  "Return the server name of INSTANTIATOR, a string."
  `(aref ,instantiator 2))

(defmacro sasl-instantiator-set-properties (instantiator plist)
  "Destructively set the properties of INSTANTIATOR.
The second argument PLIST is the new property list."
  `(setplist (aref ,instantiator 3) ,plist))

(defmacro sasl-instantiator-set-property (instantiator property value)
  "Add the given property/value to INSTANTIATOR."
  `(put (aref ,instantiator 3) ,property ,value))

(defmacro sasl-instantiator-property (instantiator property)
  "Return the value of the PROPERTY of INSTANTIATOR."
  `(get (aref ,instantiator 3) ,property))

(defmacro sasl-instantiator-properties (instantiator)
  "Return the properties of INSTANTIATOR."
  `(symbol-plist (aref ,instantiator 3)))

;;; @ SASL authenticator
;;;

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

(defmacro sasl-authenticator-mechanism (authenticator)
  "Return name of the mechanism AUTHENTICATOR supports, a string."
  `(aref ,authenticator 0))

(defmacro sasl-authenticator-continuations (authenticator)
  "Return continuation steps of AUTHENTICATOR, a list of functions."
  `(aref ,authenticator 1))

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

(defun sasl-evaluate-challenge (authenticator instantiator &optional challenge)
  "Evaluate the challenge and prepare an appropriate next response.
The data type of the value and optional 3rd argument CHALLENGE is nil or
a cons cell of the form \(CONTINUATION STRING).
At the first time CONTINUATION should be set to nil.

Argument AUTHENTICATOR is the current evaluator.
Argument INSTANTIATOR is the instantiator instantiator."
  (let* ((continuations
	  (sasl-authenticator-continuations authenticator))
	 (function
	  (if (car challenge)
	      (nth 1 (memq (car challenge) continuations))
	    (car continuations))))
    (if function
	(list function (funcall function instantiator challenge)))))

(defvar sasl-read-passphrase nil)
(defun sasl-read-passphrase (prompt)
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

(defun sasl-plain-response (instantiator challenge)
  (let ((passphrase
	 (sasl-read-passphrase
	  (format "PLAIN passphrase for %s: "
		  (sasl-instantiator-name instantiator))))
	(authentication-name
	 (sasl-instantiator-property
	  instantiator 'authentication-name))
	(name (sasl-instantiator-name instantiator)))
    (unwind-protect
	(if (and authentication-name
		 (not (string= authentication-name name)))
	    (concat authentication-name "\0" name "\0" passphrase)
	  (concat "\0" name "\0" passphrase))
      (fillarray passphrase 0))))

(put 'sasl-plain 'sasl-authenticator
     (sasl-make-authenticator "PLAIN" sasl-plain-continuations))

(provide 'sasl-plain)

;;; LOGIN (No specification exists)
(defconst sasl-login-continuations
  '(ignore				;no initial response
    sasl-login-response-1
    sasl-login-response-2))

(defun sasl-login-response-1 (instantiator challenge)
  (unless (string= (nth 1 challenge) "Username:")
    (sasl-error (format "Unexpected response: %s" (nth 1 challenge))))
  (sasl-instantiator-name instantiator))

(defun sasl-login-response-2 (instantiator challenge)
  (unless (string= (nth 1 challenge) "Password:")
    (sasl-error (format "Unexpected response: %s" (nth 1 challenge))))
  (sasl-read-passphrase
   (format "LOGIN passphrase for %s: " (sasl-instantiator-name instantiator))))

(put 'sasl-login 'sasl-authenticator
     (sasl-make-authenticator "LOGIN" sasl-login-continuations))

(provide 'sasl-login)

;;; ANONYMOUS (RFC2245)
(defconst sasl-anonymous-continuations
  '(identity				;no initial response
    sasl-anonymous-response))

(defun sasl-anonymous-response (instantiator challenge)
  (or (sasl-instantiator-property
       instantiator 'trace)
      (sasl-instantiator-name instantiator)))

(put 'sasl-anonymous 'sasl-authenticator
     (sasl-make-authenticator "ANONYMOUS" sasl-anonymous-continuations))

(provide 'sasl-anonymous)

(provide 'sasl)

;;; sasl.el ends here
