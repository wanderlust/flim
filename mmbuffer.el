;;; mmbuffer.el --- MIME entity module for binary buffer

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

(mm-define-backend buffer)

(mm-define-method initialize-instance ((entity buffer))
  (mime-entity-set-buffer-internal
   entity (mime-entity-location-internal entity))
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (setq mime-message-structure entity)
    (let ((header-start (point-min))
	  header-end
	  body-start
	  (body-end (point-max)))
      (goto-char header-start)
      (if (re-search-forward "^$" nil t)
	  (setq header-end (match-end 0)
		body-start (if (= header-end body-end)
			       body-end
			     (1+ header-end)))
	(setq header-end (point-min)
	      body-start (point-min)))
      (save-restriction
	(narrow-to-region header-start header-end)
	(mime-entity-set-content-type-internal
	 entity
	 (let ((str (std11-fetch-field "Content-Type")))
	   (if str
	       (mime-parse-Content-Type str)
	     )))
	)
      (mime-entity-set-header-start-internal entity header-start)
      (mime-entity-set-header-end-internal entity header-end)
      (mime-entity-set-body-start-internal entity body-start)
      (mime-entity-set-body-end-internal entity body-end)
      )))

(mm-define-method entity-point-min ((entity buffer))
  (mime-entity-header-start-internal entity))

(mm-define-method entity-point-max ((entity buffer))
  (mime-entity-body-end-internal entity))

(mm-define-method fetch-field ((entity buffer) field-name)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (save-restriction
      (narrow-to-region (mime-entity-header-start-internal entity)
			(mime-entity-header-end-internal entity))
      (std11-fetch-field field-name)
      )))

(mm-define-method entity-cooked-p ((entity buffer)) nil)

(mm-define-method entity-children ((entity buffer))
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

(mm-define-method entity-content ((entity buffer))
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (mime-decode-string
     (buffer-substring (mime-entity-body-start-internal entity)
		       (mime-entity-body-end-internal entity))
     (mime-entity-encoding entity))))

(mm-define-method write-entity-content ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (mime-write-decoded-region (mime-entity-body-start-internal entity)
			       (mime-entity-body-end-internal entity)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))
    ))

(mm-define-method write-entity ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (write-region-as-raw-text-CRLF (mime-entity-header-start-internal entity)
				   (mime-entity-body-end-internal entity)
				   filename)
    ))

(mm-define-method write-entity-body ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
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

(mm-define-method insert-decoded-header ((entity buffer)
					 &optional invisible-fields
					 visible-fields)
  (save-restriction
    (narrow-to-region (point)(point))
    (let ((the-buf (current-buffer))
	  (src-buf (mime-entity-buffer-internal entity))
	  (h-end (mime-entity-header-end-internal entity))
	  beg p end field-name len field)
      (save-excursion
	(set-buffer src-buf)
	(goto-char (mime-entity-header-start-internal entity))
	(save-restriction
	  (narrow-to-region (point) h-end)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq beg (match-beginning 0)
		  p (match-end 0)
		  field-name (buffer-substring beg (1- p))
		  len (string-width field-name)
		  end (std11-field-end))
	    (when (mime-visible-field-p field-name
					visible-fields invisible-fields)
	      (setq field (intern (capitalize field-name)))
	      (save-excursion
		(set-buffer the-buf)
		(insert field-name)
		(insert ":")
		(cond ((memq field eword-decode-ignored-field-list)
		       ;; Don't decode
		       (insert-buffer-substring src-buf p end)
		       )
		      ((memq field eword-decode-structured-field-list)
		       ;; Decode as structured field
		       (let ((body (save-excursion
				     (set-buffer src-buf)
				     (buffer-substring p end)
				     )))
			 (insert (eword-decode-and-fold-structured-field
				  body (1+ len)))
			 ))
		      (t
		       ;; Decode as unstructured field
		       (let ((body (save-excursion
				     (set-buffer src-buf)
				     (buffer-substring p end)
				     )))
			 (insert (eword-decode-unstructured-field-body
				  body (1+ len)))
			 )))
		(insert "\n")
		))))))))


;;; @ end
;;;

(provide 'mmbuffer)

;;; mmbuffer.el ends here
