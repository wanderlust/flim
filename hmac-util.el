;;; hmac-util.el --- Utilities for HMAC functions.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: HMAC, RFC 2104

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defsubst hmac-hex-to-int (chr)
  (cond ((<= ?a chr) (+ (- chr ?a) 10))
	((<= ?A chr) (+ (- chr ?A) 10))
	((<= ?0 chr) (- chr ?0))))

(defsubst hmac-int-to-hex (num)
  (aref "0123456789abcdef" num))

(defun hmac-unhex-string (str)
  (let* ((len (length str))
	 (dst (make-string (/ len 2) 0))
	 (idx 0)(pos 0))
    (while (< pos len)
      (aset dst idx (logior (lsh (hmac-hex-to-int (aref str pos)) 4)
			    (hmac-hex-to-int (aref str (1+ pos)))))
      (setq idx (1+ idx)
            pos (+ 2 pos)))
    dst))

(defun hmac-hex-string (str)
  (let* ((len (length str))
	 (dst (make-string (* len 2) 0))
	 (idx 0)(pos 0))
    (while (< pos len)
      (aset dst idx (hmac-int-to-hex (logand (lsh (aref str pos) -4) 15)))
      (setq idx (1+ idx))
      (aset dst idx (hmac-int-to-hex (logand (aref str pos) 15)))
      (setq idx (1+ idx)
            pos (1+ pos)))
    dst))

(provide 'hmac-util)

;;; hmac-util.el ends here.
