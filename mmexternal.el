;;; mmexternal.el --- MIME entity module for external buffer

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
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
(require 'pces)

(eval-and-compile
  (luna-define-class mime-external-entity (mime-entity)
		     (body-buffer
		      body-file))
  (luna-define-internal-accessors 'mime-external-entity)

  ;; In an external entity, information of media-type or other
  ;; information which are represented in a header in a non-external
  ;; entity are in the body of the parent entity.
  )

(luna-define-method mime-entity-name ((entity mime-external-entity))
  (concat "child of "
	  (buffer-name
	   (mime-entity-name
	    (mime-entity-parent-internal entity)))))


;; [tomo] We should think about modularity about header presentation...

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
	(mode-obj (mime-find-field-presentation-method 'wide))
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
		  field-decoder (inline (mime-find-field-decoder-internal
					 field mode-obj)))
	    (with-current-buffer the-buf
	      (insert field-name)
	      (insert (if field-decoder
			  (funcall field-decoder field-body len)
			;; Don't decode
			field-body))
	      (insert "\n")
	      )))))))

(luna-define-method mime-insert-header ((entity mime-external-entity)
					&optional invisible-fields
					visible-fields)
  (mime-insert-header-from-buffer
   (mime-entity-body-buffer (mime-entity-parent-internal entity))
   (mime-entity-body-start-point (mime-entity-parent-internal entity))
   (mime-entity-body-end-point (mime-entity-parent-internal entity))
   invisible-fields visible-fields))


(luna-define-method mime-entity-content ((entity mime-external-entity))
  (let ((buf (mime-entity-body-buffer entity)))
    (if buf
	(with-current-buffer buf
	  (mime-decode-string
	   (buffer-string)
	   (mime-entity-encoding entity)))
      (message "Cannot get external content")
      nil)))

(luna-define-method mime-entity-fetch-field :around
  ((entity mime-external-entity) field-name)
  (or (luna-call-next-method)
      (save-excursion
	(mime-goto-body-start-point (mime-entity-parent-internal entity))
	(save-restriction
	  (narrow-to-region
	   (point)
	   (mime-entity-body-end-point (mime-entity-parent-internal entity)))
	  (let ((ret (std11-fetch-field field-name)))
	    (when ret
	      (or (symbolp field-name)
		  (setq field-name
			(intern (capitalize (capitalize field-name)))))
  	      (mime-entity-set-original-header-internal
	       entity
	       (put-alist field-name ret
			  (mime-entity-original-header-internal entity)))
	      ret))))))

(luna-define-method mime-insert-entity-content ((entity mime-external-entity))
  (insert
   (with-current-buffer (mime-external-entity-body-buffer-internal entity)
     (mime-decode-string
      (buffer-string)
      (mime-entity-encoding entity)))))

(mm-define-method write-entity-content ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-external-entity-buffer-internal entity))
    (mime-write-decoded-region (mime-external-entity-body-start-internal entity)
			       (mime-external-entity-body-end-internal entity)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))
    ))

(mm-define-method insert-entity ((entity buffer))
  (insert-buffer-substring (mime-external-entity-buffer-internal entity)
			   (mime-external-entity-header-start-internal entity)
			   (mime-external-entity-body-end-internal entity))
  )

(mm-define-method write-entity ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-external-entity-buffer-internal entity))
    (write-region-as-raw-text-CRLF
     (mime-external-entity-header-start-internal entity)
     (mime-external-entity-body-end-internal entity)
     filename)
    ))

(mm-define-method write-entity-body ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-external-entity-buffer-internal entity))
    (write-region-as-binary (mime-external-entity-body-start-internal entity)
			    (mime-external-entity-body-end-internal entity)
			    filename)
    ))


;;; @ header buffer
;;;

(luna-define-method mime-entity-header-buffer ((entity mime-external-entity))
  (mime-entity-body-buffer (mime-entity-parent-internal entity)))

(luna-define-method mime-goto-header-start-point ((entity
						   mime-external-entity))
  (mime-goto-body-start-point (mime-entity-parent-internal entity)))

(luna-define-method mime-entity-header-start-point ((entity
						     mime-external-entity))
  (mime-entity-body-start-point (mime-entity-parent-internal entity)))

(luna-define-method mime-entity-header-end-point ((entity
						   mime-external-entity))
  (mime-entity-body-end-point (mime-entity-parent-internal entity)))


;;; @ body buffer
;;;

(luna-define-method mime-entity-body-buffer ((entity mime-external-entity))
  (or (mime-external-entity-body-buffer-internal entity)
      (let* ((ct
	      (mime-entity-content-type (mime-entity-parent-internal entity))))
	(if (string= (mime-content-type-parameter ct "access-type")
		     "anon-ftp")
	    (let* ((site (mime-content-type-parameter ct "site"))
		   (directory (mime-content-type-parameter ct "directory"))
		   (name (mime-content-type-parameter ct "name"))
		   (pathname
		    (expand-file-name
		     name (concat "/anonymous@" site ":" directory)))
		   (buf (create-file-buffer pathname)))
	      (condition-case nil
		  (with-current-buffer buf
		    (insert-file-contents-as-binary pathname)
		    (mime-external-entity-set-body-buffer-internal
		     entity buf))
		(error (message "Cannot open external buffer")))
	      buf)))))

(luna-define-method mime-goto-body-start-point ((entity mime-external-entity))
  (set-buffer (mime-entity-body-buffer entity))
  (goto-char (point-min)))

(luna-define-method mime-goto-body-end-point ((entity mime-external-entity))
  (set-buffer (mime-entity-body-buffer entity))
  (goto-char (point-max)))

(luna-define-method mime-entity-body-start-point ((entity
						   mime-external-entity))
  (with-current-buffer (mime-entity-body-buffer entity)
    (point-min)))

(luna-define-method mime-entity-body-end-point ((entity mime-external-entity))
  (with-current-buffer (mime-entity-body-buffer entity)
    (point-max)))


;;; @ buffer (obsolete)
;;;

(luna-define-method mime-entity-buffer ((entity mime-external-entity))
  (mime-entity-body-buffer entity))

(luna-define-method mime-entity-point-min ((entity mime-external-entity))
  (mime-entity-body-start-point entity))

(luna-define-method mime-entity-point-max ((entity mime-external-entity))
  (mime-entity-body-end-point entity))


;;; @ end
;;;

(provide 'mmexternal)

;;; mmexternal.el ends here
