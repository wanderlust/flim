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

(defun mmcooked-open-entity (location)
  (mime-parse-buffer location 'cooked)
  )

(defalias 'mmcooked-entity-point-min	'mmbuffer-entity-point-min)
(defalias 'mmcooked-entity-point-max	'mmbuffer-entity-point-max)
(defalias 'mmcooked-fetch-field		'mmbuffer-fetch-field)

(defun mmcooked-cooked-p () t)

(defalias 'mmcooked-entity-content	'mmbuffer-entity-content)

(defun mmcooked-write-entity-content (entity filename)
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

(defun mmcooked-write-entity (entity filename)
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (write-region (mime-entity-point-min entity)
		  (mime-entity-point-max entity) filename)
    ))

(defun mmcooked-write-entity-body (entity filename)
  (save-excursion
    (set-buffer (mime-entity-buffer entity))
    (write-region (mime-entity-body-start entity)
		  (mime-entity-body-end entity) filename)
    ))

(defun mmcooked-insert-decoded-header (entity &optional invisible-fields
					      visible-fields)
  (save-restriction
    (narrow-to-region (point)(point))
    (let ((the-buf (current-buffer))
	  (src-buf (mime-entity-buffer entity))
	  (h-end (mime-entity-header-end entity))
	  beg p end field-name len field)
      (save-excursion
	(set-buffer src-buf)
	(goto-char (mime-entity-header-start entity))
	(save-restriction
	  (narrow-to-region (point) h-end)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq beg (match-beginning 0)
		  p (match-end 0)
		  field-name (buffer-substring beg (1- p))
		  len (string-width field-name)
		  end (std11-field-end))
	    (when (eword-visible-field-p field-name
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
				     ))
			     default-mime-charset)
			 (insert (eword-decode-and-fold-structured-field
				  body (1+ len)))
			 ))
		      (t
		       ;; Decode as unstructured field
		       (let ((body (save-excursion
				     (set-buffer src-buf)
				     (buffer-substring p end)
				     ))
			     default-mime-charset)
			 (insert (eword-decode-unstructured-field-body
				  body (1+ len)))
			 )))
		(insert "\n")
		))))))))


;;; @ end
;;;

(provide 'mmcooked)

;;; mmcooked.el ends here
