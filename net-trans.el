;;; net-trans.el --- basic transaction framework for internet protocols.

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
  (luna-define-class transaction ())

  (luna-define-internal-accessors 'transaction))

(luna-define-generic transaction-error-name (trans)
  "Return error symbol of the TRANSACTION.")

(luna-define-method transaction-error-name ((trans transaction))
  (intern (format "%s-error" (luna-class-name trans))))

(defvar transaction-combinator-alist
  '((&& transaction-compose-&&)
    (|| transaction-compose-||)))

(defun transaction-compose-&& (left right)
  "Multiplicative operator of current transaction LEFT and RIGHT."
  `(lambda (transaction)
     (let ((next (funcall #',left transaction)))
       (funcall #',right next))))

(defun transaction-compose-|| (left right)
  "Additive operator of current transaction LEFT and RIGHT."
  `(lambda (transaction)
     (let (next error)
       (setq error
	     (catch (transaction-error-name transaction)
	       (setq next (funcall #',left transaction))
	       nil))
       (if error
	   (funcall #',right transaction)
	 next))))

(defun transaction-compose-fold-left (function accu sequence)
  (if (null sequence)
      accu
    (transaction-compose-fold-left
     function (funcall function accu (car sequence))
     (cdr sequence))))

(defun transaction-compose-commands (commands)
  "Compose COMMANDS."
  (let ((combinator
	 (assq (pop commands) transaction-combinator-alist))
	(accu
	 (if (listp (car commands))
	     (transaction-compose-commands (pop commands))
	   (pop commands))))
    (if (null combinator)
	(error "Unknown operator")
      (setq accu
	    (transaction-compose-fold-left
	     `(lambda (accu c)
		(funcall
		 #',(nth 1 combinator) accu
		 (if (listp c)
		     (transaction-compose-commands c)
		   c)))
	     accu commands))
      (if (byte-code-function-p accu)
	  accu
	(byte-compile accu)))))

(provide 'net-trans)

;;; net-trans.el ends here
