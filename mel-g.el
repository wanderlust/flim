;;;
;;; mel-g.el: Gzip64 encoder/decoder for GNU Emacs
;;;
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;; Copyright (C) 1996 Shuhei KOBAYASHI
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;;         Shuhei KOBAYASHI <shuhei@cmpt01.phys.tohoku.ac.jp>
;;; Created: 1995/10/25
;;; Version:
;;;	$Id$
;;; Keywords: MIME, base64, gzip
;;;
;;; This file is not part of MEL (MIME Encoding Library) yet.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

;;; @ variables
;;;

(defvar gzip64-external-encoder '("sh" "-c" "gzip -c | mmencode")
  "*list of gzip64 encoder program name and its arguments.")

(defvar gzip64-external-decoder '("sh" "-c" "mmencode -u | gzip -dc")
  "*list of gzip64 decoder program name and its arguments.")


;;; @ external encoder
;;;

(cond ((boundp 'MULE)
       (define-program-coding-system
	 nil (car gzip64-external-encoder) *noconv*)
       (define-program-coding-system
	 nil (car gzip64-external-decoder) *noconv*)
       )
      ((boundp 'NEMACS)
       (define-program-kanji-code
	 nil (car gzip64-external-encoder) 0)
       (define-program-kanji-code
	 nil (car gzip64-external-decoder) 0)
       ))

(defun gzip64-external-encode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let (selective-display ; Disable ^M to nl translation.
	  mc-flag           ; for Mule
	  kanji-flag)       ; for NEmacs
      (apply (function call-process-region)
	     beg end (car gzip64-external-encoder)
	     t t nil (cdr gzip64-external-encoder))
      )))

(defun gzip64-external-decode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let ((selective-display nil)       ; Disable ^M to nl translation.
	  (mc-flag nil)                 ; for Mule
	  (kanji-flag nil))             ; for NEmacs
      (apply (function call-process-region)
             beg end (car gzip64-external-decoder)
             t t nil (cdr gzip64-external-decoder))
      )))

(defalias 'gzip64-encode-region 'gzip64-external-encode-region)
(defalias 'gzip64-decode-region 'gzip64-external-decode-region)


;;; @ end
;;;

(provide 'mel-g)

;;; mel-g.el ends here.
