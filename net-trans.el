;;; net-trans.el --- basic transaction framework for internet protocols

;; Copyright (C) 2000 Daiki Ueno

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 2000/08/14

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

(require 'luna)

(eval-when-compile (require 'cl))

(eval-and-compile
  (luna-define-class net-transaction ())

  (luna-define-internal-accessors 'net-transaction))

(luna-define-generic net-transaction-error-name (trans)
  "Return error symbol of the TRANSACTION.")

(luna-define-generic net-transaction-error (trans error)
  "Throw an ERROR of the TRANSACTION.")

(luna-define-method net-transaction-error-name ((trans net-transaction))
  (intern (format "%s-error" (luna-class-name trans))))

(luna-define-method net-transaction-error ((trans net-transaction) error)
  (throw (net-transaction-error-name trans) error))

(defvar net-transaction-combinator-alist
  '((&& net-transaction-compose-&&)
    (|| net-transaction-compose-||)))

(defun net-transaction-compose-&& (left right)
  "Multiplicative operator of current transaction LEFT and RIGHT."
  `(lambda (trans)
     (let ((next (funcall #',left trans)))
       (funcall #',right next))))

(defun net-transaction-compose-|| (left right)
  "Additive operator of current transaction LEFT and RIGHT."
  `(lambda (trans)
     (let (next error)
       (setq error
	     (catch (net-transaction-error-name trans)
	       (setq next (funcall #',left trans))
	       nil))
       (if error
	   (funcall #',right trans)
	 next))))

(defun net-transaction-compose-fold-left (function accu sequence)
  "Apply FUNCTION to ACCU while folding SEQUENCE left to right."
  (if (null sequence)
      accu
    (net-transaction-compose-fold-left
     function (funcall function accu (car sequence))
     (cdr sequence))))

(defun net-transaction-compose-commands (commands)
  "Compose transaction-function from COMMANDS."
  (let ((combinator
	 (assq (pop commands) net-transaction-combinator-alist))
	(accu
	 (if (listp (car commands))
	     (net-transaction-compose-commands (pop commands))
	   (pop commands))))
    (if (null combinator)
	(error "Unknown operator")
      (setq accu
	    (net-transaction-compose-fold-left
	     `(lambda (accu c)
		(funcall
		 #',(nth 1 combinator) accu
		 (if (listp c)
		     (net-transaction-compose-commands c)
		   c)))
	     accu commands))
      (if (and (listp accu) (eq (car accu) 'lambda))
	  (byte-compile accu)
	accu))))

(provide 'net-trans)

;;; net-trans.el ends here
