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

(require 'poe)

(defvar sasl-mechanisms
  '(("CRAM-MD5" sasl-cram)))

(defmacro sasl-make-authenticator (mechanism continuations)
  `(vector ,mechanism ,continuations))

(defmacro sasl-authenticator-mechanism-internal (authenticator)
  `(aref ,authenticator 0))

(defmacro sasl-authenticator-continuations-internal (authenticator)
  `(aref ,authenticator 1))

(defmacro sasl-make-principal (name service server)
  `(vector ,name ,service ,server))

(defmacro sasl-principal-name-internal (principal)
  `(aref ,principal 0))

(defmacro sasl-principal-service-internal (principal)
  `(aref ,principal 1))

(defmacro sasl-principal-server-internal (principal)
  `(aref ,principal 2))

(defun sasl-find-authenticator (mechanisms)
  "Retrieve an apropriate authenticator object from MECHANISMS hints."
  (let (mechanism)
    (while mechanisms
      (if (setq mechanism (assoc (car mechanisms) sasl-mechanisms))
	  (setq mechanism (nth 1 mechanism)
		mechanisms nil))
      (setq mechanisms (cdr mechanisms)))
    (when mechanism
      (require mechanism)
      (get mechanism 'sasl-authenticator))))

(defun sasl-evaluate-challenge (authenticator principal &optional challenge)
  "Evaluate the challenge and prepare an appropriate next response.
The data type of the value and the CHALLENGE is nil or a cons cell of the form
\(CONTINUATION STRING).  At the first time CONTINUATION should be set to nil."
  (let* ((continuations
	  (sasl-authenticator-continuations-internal authenticator))
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

(provide 'sasl)

;;; sasl.el ends here
