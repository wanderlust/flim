;;; mel-g.el: Gzip64 encoder/decoder for GNU Emacs

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko
;; Copyright (C) 1996,1997 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;;	modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Maintainer: Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
;; Created: 1995/10/25
;; Keywords: Gzip64, base64, gzip, MIME

;; This file is part of MEL (MIME Encoding Library).

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

;;; Commentary:

;;; NOTE: Gzip64 is an experimental Content-Transfer-Encoding and its
;;; use is STRONGLY DISCOURAGED except for private communication.

;;; Code:

(require 'emu)
(require 'path-util)


;;; @ variables
;;;

(defvar gzip64-external-encoder
  (let ((file (exec-installed-p "mmencode")))
    (and file
	 (` ("sh" "-c" (, (concat "gzip -c | " file))))
	 ))
  "*list of gzip64 encoder program name and its arguments.")

(defvar gzip64-external-decoder
  (let ((file (exec-installed-p "mmencode")))
    (and file
	 (` ("sh" "-c" (, (concat file " -u | gzip -dc"))))
	 ))
  "*list of gzip64 decoder program name and its arguments.")


;;; @ encoder/decoder for region
;;;

(defun gzip64-external-encode-region (beg end)
  (interactive "*r")
  (save-excursion
    (as-binary-process (apply (function call-process-region)
			      beg end (car gzip64-external-encoder)
			      t t nil (cdr gzip64-external-encoder))
		       )
    ;; for OS/2
    ;;   regularize line break code
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match "")
      )
    ))

(defun gzip64-external-decode-region (beg end)
  (interactive "*r")
  (save-excursion
    (as-binary-process (apply (function call-process-region)
			      beg end (car gzip64-external-decoder)
			      t t nil (cdr gzip64-external-decoder))
		       )
    ))

(defalias 'gzip64-encode-region 'gzip64-external-encode-region)
(defalias 'gzip64-decode-region 'gzip64-external-decode-region)


;;; @ encoder/decoder for file
;;;

(defun gzip64-insert-encoded-file (filename)
  (interactive (list (read-file-name "Insert encoded file: ")))
  (apply (function call-process) (car gzip64-external-encoder)
	 filename t nil
	 (cdr gzip64-external-encoder))
  )

(defun gzip64-write-decoded-region (start end filename)
  "Decode and write current region encoded by gzip64 into FILENAME.
START and END are buffer positions."
  (interactive
   (list (region-beginning) (region-end)
	 (read-file-name "Write decoded region to file: ")))
  (as-binary-process
   (apply (function call-process-region)
	  start end (car gzip64-external-decoder)
	  nil nil nil
	  (let ((args (cdr gzip64-external-decoder)))
	    (append (butlast args)
		    (list (concat (car (last args)) ">" filename))))
	  )))


;;; @ end
;;;

(provide 'mel-g)

;;; mel-g.el ends here.
