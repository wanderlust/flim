;;; tram.el --- basic session framework for internet protocols

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
(require 'closure)

(eval-when-compile (require 'cl))

(eval-and-compile
  (luna-define-class tram-stream ())

  (luna-define-internal-accessors 'tram-stream))

(luna-define-generic tram-stream-error-name (stream)
  "Return error symbol of the STREAM.")

(luna-define-generic tram-stream-error (stream error)
  "Throw an ERROR of the STREAM.")

(luna-define-method tram-stream-error-name ((trans tram-stream))
  (intern (format "%s-error" (luna-class-name trans))))

(luna-define-method tram-stream-error ((trans tram-stream) error)
  (throw (tram-stream-error-name trans) error))

(put '&& 'tram-compose-function #'tram-compose-&&)
(put '|| 'tram-compose-function #'tram-compose-||)

(defun tram-compose-&& (left right)
  "Multiplicative combinator which composes LEFT and RIGHT operations."
  `(lambda (trans)
     (let ((next (closure-call ',left trans)))
       (closure-call ',right next))))

(defun tram-compose-|| (left right)
  "Additive combinator which composes LEFT and RIGHT operations."
  `(lambda (trans)
     (let (next error)
       (setq error
	     (catch (tram-stream-error-name trans)
	       (setq next (closure-call ',left trans))
	       nil))
       (if error
	   (closure-call ',right trans)
	 next))))

(defun tram-fold-left (function accu sequence)
  "Apply FUNCTION to ACCU while folding SEQUENCE left to right."
  (if (null sequence)
      accu
    (tram-fold-left
     function (funcall function accu (car sequence))
     (cdr sequence))))

;;;###autoload
(defmacro tram-define-transaction (name tram-transaction &optional doc)
  "Set NAME the compiled code of TRAM-TRANSACTION."
  `(let ((transaction
	  ,(tram-compose-transaction (eval tram-transaction))))
     (defconst ,name transaction ,doc)))

;;;###autoload
(defun tram-compose-transaction (tram-transaction)
  "Compose transaction-function from TRAM-TRANSACTION."
  (if (not (symbolp (car tram-transaction)))
      tram-transaction
    (let ((combinator
	   (get (pop tram-transaction) 'tram-compose-function))
	  accu)
      (if (null combinator)
	  (error "Unknown operator")
	(setq accu
	      (tram-fold-left
	       `(lambda (accu c)
		  (funcall
		   #',combinator accu
		   (if (listp c)
		       (tram-compose-transaction c)
		     c)))
	       (if (listp (car tram-transaction))
		   (tram-compose-transaction (pop tram-transaction))
		 (pop tram-transaction))
	       tram-transaction))
	(if (and (listp accu) (eq (car accu) 'lambda))
	    (byte-compile accu)
	  accu)))))

(provide 'tram)

;;; tram.el ends here
