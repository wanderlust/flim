;;; mmbuffer.el --- MIME entity module for binary buffer

;; Copyright (C) 1998,1999,2000 Free Software Foundation, Inc.

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

(require 'mmgeneric)
(require 'mime)

(eval-and-compile
  (luna-define-class mime-buffer-entity (mime-entity)
		     (buffer
		      header-start
		      header-end
		      body-start
		      body-end))

  (luna-define-internal-accessors 'mime-buffer-entity)
  )

(luna-define-method initialize-instance :after ((entity mime-buffer-entity)
						&rest init-args)
  (or (mime-buffer-entity-buffer-internal entity)
      (mime-buffer-entity-set-buffer-internal
       entity (mime-entity-location-internal entity)))
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (if (mime-root-entity-p entity)
	(setq mime-message-structure entity))
    (let ((header-start
	   (or (mime-buffer-entity-header-start-internal entity)
	       (mime-buffer-entity-set-header-start-internal
		entity (point-min))))
	  (header-end (mime-buffer-entity-header-end-internal entity))
	  (body-start (mime-buffer-entity-body-start-internal entity))
	  (body-end
	   (or (mime-buffer-entity-body-end-internal entity)
	       (mime-buffer-entity-set-body-end-internal entity (point-max)))))
      (goto-char header-start)
      (unless (and header-end body-start)
	(if (re-search-forward "^$" body-end t)
	    (setq header-end (match-end 0)
		  body-start (if (= header-end body-end)
				 body-end
			       (1+ header-end)))
	  (setq header-end (point-min)
		body-start (point-min)))
	(mime-buffer-entity-set-header-end-internal entity header-end)
	(mime-buffer-entity-set-body-start-internal entity body-start)
	)
      (or (mime-entity-content-type-internal entity)
	  (save-restriction
	    (narrow-to-region header-start header-end)
	    (mime-entity-set-content-type-internal
	     entity
	     (let ((str (std11-fetch-field "Content-Type")))
	       (if str
		   (mime-parse-Content-Type str)
		 )))
	    ))
      ))
  entity)

(luna-define-method mime-entity-name ((entity mime-buffer-entity))
  (buffer-name (mime-buffer-entity-buffer-internal entity))
  )


(luna-define-method mime-insert-header ((entity mime-buffer-entity)
					&optional invisible-fields
					visible-fields)
  (mime-insert-header-from-buffer
   (mime-buffer-entity-buffer-internal entity)
   (mime-buffer-entity-header-start-internal entity)
   (mime-buffer-entity-header-end-internal entity)
   invisible-fields visible-fields)
  )

(luna-define-method mime-entity-content ((entity mime-buffer-entity))
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (mime-decode-string
     (buffer-substring (mime-buffer-entity-body-start-internal entity)
		       (mime-buffer-entity-body-end-internal entity))
     (mime-entity-encoding entity))))

(luna-define-method mime-entity-fetch-field :around
  ((entity mime-buffer-entity) field-name)
  (or (luna-call-next-method)
      (save-excursion
	(set-buffer (mime-buffer-entity-buffer-internal entity))
	(save-restriction
	  (narrow-to-region (mime-buffer-entity-header-start-internal entity)
			    (mime-buffer-entity-header-end-internal entity))
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

(mm-define-method insert-entity-content ((entity buffer))
  (insert (with-current-buffer (mime-buffer-entity-buffer-internal entity)
	    (mime-decode-string
	     (buffer-substring (mime-buffer-entity-body-start-internal entity)
			       (mime-buffer-entity-body-end-internal entity))
	     (mime-entity-encoding entity)))))

(mm-define-method write-entity-content ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (mime-write-decoded-region (mime-buffer-entity-body-start-internal entity)
			       (mime-buffer-entity-body-end-internal entity)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))
    ))

(mm-define-method insert-entity ((entity buffer))
  (insert-buffer-substring (mime-buffer-entity-buffer-internal entity)
			   (mime-buffer-entity-header-start-internal entity)
			   (mime-buffer-entity-body-end-internal entity))
  )

(mm-define-method write-entity ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (write-region-as-raw-text-CRLF
     (mime-buffer-entity-header-start-internal entity)
     (mime-buffer-entity-body-end-internal entity)
     filename)
    ))

(mm-define-method write-entity-body ((entity buffer) filename)
  (save-excursion
    (set-buffer (mime-buffer-entity-buffer-internal entity))
    (write-region-as-binary (mime-buffer-entity-body-start-internal entity)
			    (mime-buffer-entity-body-end-internal entity)
			    filename)
    ))


;;; @ header buffer
;;;

(luna-define-method mime-entity-header-buffer ((entity mime-buffer-entity))
  (mime-buffer-entity-buffer-internal entity)
  )

(luna-define-method mime-goto-header-start-point ((entity mime-buffer-entity))
  (set-buffer (mime-buffer-entity-buffer-internal entity))
  (goto-char (mime-buffer-entity-header-start-internal entity))
  )

(luna-define-method mime-entity-header-start-point ((entity
						     mime-buffer-entity))
  (mime-buffer-entity-header-start-internal entity)
  )

(luna-define-method mime-entity-header-end-point ((entity
						   mime-buffer-entity))
  (mime-buffer-entity-header-end-internal entity)
  )


;;; @ body buffer
;;;

(luna-define-method mime-entity-body-buffer ((entity mime-buffer-entity))
  (mime-buffer-entity-buffer-internal entity)
  )

(luna-define-method mime-goto-body-start-point ((entity mime-buffer-entity))
  (set-buffer (mime-buffer-entity-buffer-internal entity))
  (goto-char (mime-buffer-entity-body-start-internal entity))
  )

(luna-define-method mime-goto-body-end-point ((entity mime-buffer-entity))
  (set-buffer (mime-buffer-entity-buffer-internal entity))
  (goto-char (mime-buffer-entity-body-end-internal entity))
  )

(luna-define-method mime-entity-body-start-point ((entity mime-buffer-entity))
  (mime-buffer-entity-body-start-internal entity)
  )

(luna-define-method mime-entity-body-end-point ((entity mime-buffer-entity))
  (mime-buffer-entity-body-end-internal entity)
  )


;;; @ buffer (obsolete)
;;;

(luna-define-method mime-entity-buffer ((entity mime-buffer-entity))
  (mime-buffer-entity-buffer-internal entity)
  )

(luna-define-method mime-entity-point-min ((entity mime-buffer-entity))
  (mime-buffer-entity-header-start-internal entity)
  )

(luna-define-method mime-entity-point-max ((entity mime-buffer-entity))
  (mime-buffer-entity-body-end-internal entity)
  )


;;; @ end
;;;

(provide 'mmbuffer)

;;; mmbuffer.el ends here
