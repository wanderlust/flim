;;; mmcooked.el --- MIME entity implementation for binary buffer

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

(require 'mmbuffer)

(mm-define-backend cooked (buffer))

(mm-define-method open ((nil cooked) location)
  (mime-parse-buffer location 'cooked))

(mm-define-method cooked-p ((entity cooked)) t)

(mm-define-method write-content ((entity cooked) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (let ((encoding (or (mime-entity-encoding entity) "7bit")))
      (if (member encoding '("7bit" "8bit" "binary"))
	  (write-region (mime-entity-body-start-internal entity)
			(mime-entity-body-end-internal entity) filename)
	(mime-write-decoded-region (mime-entity-body-start-internal entity)
				   (mime-entity-body-end-internal entity)
				   filename encoding)
	))))

(mm-define-method write-with-header ((entity cooked) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (write-region (mime-entity-header-start-internal entity)
		  (mime-entity-body-end-internal entity)
		  filename)
    ))

(mm-define-method write-body ((entity cooked) filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (write-region (mime-entity-body-start-internal entity)
		  (mime-entity-body-end-internal entity)
		  filename)
    ))

(mm-define-method insert-decoded-header ((entity cooked)
					 &optional invisible-fields
					 visible-fields)
  (let (default-mime-charset)
    (funcall (mime-find-function 'insert-decoded-header 'buffer)
	     entity invisible-fields visible-fields)
    ))


;;; @ end
;;;

(provide 'mmcooked)

;;; mmcooked.el ends here
