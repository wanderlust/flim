;;; mmgeneric.el --- MIME entity module for generic buffer

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

(require 'mime)
(require 'mime-parse)

(mm-define-backend generic)

(mm-define-method entity-header-start ((entity generic))
  (mime-entity-set-header-start-internal
   entity
   (save-excursion
     (set-buffer (mime-entity-buffer entity))
     (point-min)
     )))

(mm-define-method entity-header-end ((entity generic))
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (mime-entity-header-end-internal entity)
    ))

(mm-define-method entity-body-start ((entity generic))
  (mime-entity-set-body-start-internal
   entity
   (save-excursion
     (set-buffer (mime-entity-buffer entity))
     (mime-entity-body-start-internal entity)
     )))

(mm-define-method entity-body-end ((entity generic))
  (mime-entity-set-body-end-internal
   entity
   (save-excursion
     (set-buffer (mime-entity-buffer entity))
     (point-max)
     )))

(mm-define-method entity-point-min ((entity generic))
  (or (mime-entity-header-start-internal entity)
      (mime-entity-send entity 'entity-header-start)))

(mm-define-method entity-point-max ((entity generic))
  (or (mime-entity-body-end-internal entity)
      (mime-entity-send entity 'entity-body-end)))

(mm-define-method fetch-field ((entity generic) field-name)
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (save-restriction
      (narrow-to-region (mime-entity-header-start-internal entity)
			(mime-entity-header-end-internal entity))
      (std11-fetch-field field-name)
      )))

(mm-define-method entity-cooked-p ((entity generic)) nil)

(mm-define-method entity-children ((entity generic))
  (let* ((content-type (mime-entity-content-type entity))
	 (primary-type (mime-content-type-primary-type content-type)))
    (cond ((eq primary-type 'multipart)
	   (mime-parse-multipart entity)
	   )
	  ((and (eq primary-type 'message)
		(memq (mime-content-type-subtype content-type)
		      '(rfc822 news external-body)
		      ))
	   (mime-parse-encapsulated entity)
	   ))
    ))

(mm-define-method entity-content ((entity generic))
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (mime-decode-string
     (buffer-substring (mime-entity-body-start-internal entity)
		       (mime-entity-body-end-internal entity))
     (mime-entity-encoding entity))))

(mm-define-method write-entity-content ((entity generic) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (mime-write-decoded-region (mime-entity-body-start-internal entity)
			       (mime-entity-body-end-internal entity)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))
    ))

(mm-define-method write-entity ((entity generic) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (write-region-as-raw-text-CRLF (mime-entity-header-start-internal entity)
				   (mime-entity-body-end-internal entity)
				   filename)
    ))

(mm-define-method write-entity-body ((entity generic) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (write-region-as-binary (mime-entity-body-start-internal entity)
			    (mime-entity-body-end-internal entity)
			    filename)
    ))

(defun mime-visible-field-p (field-name visible-fields invisible-fields)
  (or (catch 'found
	(while visible-fields
	  (let ((regexp (car visible-fields)))
	    (if (string-match regexp field-name)
		(throw 'found t)
	      ))
	  (setq visible-fields (cdr visible-fields))
	  ))
      (catch 'found
	(while invisible-fields
	  (let ((regexp (car invisible-fields)))
	    (if (string-match regexp field-name)
		(throw 'found nil)
	      ))
	  (setq invisible-fields (cdr invisible-fields))
	  )
	t)))

(defun mime-insert-header-from-buffer (buffer start end
					      &optional invisible-fields
					      visible-fields)
  (let ((the-buf (current-buffer))
	(decoder-alist
         (cdr (assq 'folding mime-field-decoder-cache)))
	field-decoder
	f-b p f-e field-name len field field-body)
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(narrow-to-region start end)
	(goto-char start)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq f-b (match-beginning 0)
		p (match-end 0)
		field-name (buffer-substring f-b p)
		len (string-width field-name)
		f-e (std11-field-end))
	  (when (mime-visible-field-p field-name
				      visible-fields invisible-fields)
	    (setq field (intern
			 (capitalize (buffer-substring f-b (1- p))))
		  field-body (buffer-substring p f-e)
		  field-decoder 
                    (cdr (or (assq field decoder-alist)
                             (prog1
                                 (funcall
                                   mime-update-field-decoder-cache
                                   field 'folding)
                               (setq decoder-alist
                                     (cdr (assq 'folding
                                                mime-field-decoder-cache)))))))
	    (with-current-buffer the-buf
	      (insert field-name)
	      (insert (if field-decoder
			  (funcall field-decoder field-body len)
			;; Don't decode
			field-body))
	      (insert "\n")
	      )))))))

(mm-define-method insert-header ((entity generic)
				 &optional invisible-fields visible-fields)
  (mime-insert-header-from-buffer
   (mime-entity-buffer entity)
   (mime-entity-header-start-internal entity)
   (mime-entity-header-end-internal entity)
   invisible-fields visible-fields)
  )

(mm-define-method insert-text-content ((entity generic))
  (insert
   (decode-mime-charset-string (mime-entity-content entity)
			       (or (mime-content-type-parameter
				    (mime-entity-content-type entity)
				    "charset")
				   default-mime-charset)
			       'CRLF)
   ))


;;; @ end
;;;

(provide 'mmgeneric)

;;; mmgeneric.el ends here
