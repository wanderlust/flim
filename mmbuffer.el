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

(defun mmbuffer-open-entity (location)
  (mime-parse-buffer location)
  )

(defun mmbuffer-entity-point-min (entity)
  (mime-entity-header-start-internal entity)
  )

(defun mmbuffer-entity-point-max (entity)
  (mime-entity-body-end-internal entity)
  )

(defun mmbuffer-fetch-field (entity field-name)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (save-restriction
      (narrow-to-region (mime-entity-header-start-internal entity)
			(mime-entity-header-end-internal entity))
      (std11-fetch-field field-name)
      )))

(defun mmbuffer-cooked-p () nil)

(defun mmbuffer-entity-content (entity)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (mime-decode-string
     (buffer-substring (mime-entity-body-start-internal entity)
		       (mime-entity-body-end-internal entity))
     (mime-entity-encoding entity))))

(defun mmbuffer-write-entity-content (entity filename)
  (save-excursion
    (set-buffer (mime-entity-buffer-internal entity))
    (mime-write-decoded-region (mime-entity-body-start-internal entity)
			       (mime-entity-body-end-internal entity)
			       filename
			       (or (mime-entity-encoding entity) "7bit"))
    ))


;;; @ end
;;;

(provide 'mmbuffer)

;;; mmbuffer.el ends here
