;;; hmac-def.el --- Functions/macros for defining HMAC functions.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: HMAC, RFC 2104

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

;; See RFC 2104, "HMAC: Keyed-Hashing for Message Authentication"
;; for definition of HMAC.

;;; Code:

(require 'hmac-util)

(defmacro hmac-unhex-string-macro (string length)
  (let* ((len (eval length))
         (dst (make-string (/ len 2) 0)))
    `(let ((str ,string)
           (dst ,dst)
           (idx 0)(pos 0))
       (while (< pos ,len)
	 (aset dst idx (+ (* (hmac-hex-to-int (aref str pos)) 16)
			  (hmac-hex-to-int (aref str (1+ pos)))))
	 (setq idx (1+ idx)
	       pos (+ 2 pos)))
       dst)))

;; Note that H, B, and L will be evaluated multiple times.  They are
;; usually constants, so I don't want to bother to bind them locally.
(defmacro define-hmac-function (name H B L &optional bit)
  "Define a function NAME which computes HMAC with hash function H.

HMAC function is H\(KEY XOR opad, H\(KEY XOR ipad, TEXT\)\):

H is a cryptographic hash function, such as SHA1 and MD5, which takes
a string and return a digest of it \(in hexadecimal form\).
B is a byte-length of a block size of H. \(B=64 for both SHA1 and MD5.\)
L is a byte-length of hash outputs. \(L=16 for MD5, L=20 for SHA1.\)
If BIT is non-nil, truncate output to specified bits."
  `(defun ,name (text key)
     ,(concat "Compute " (upcase (symbol-name name)) " over TEXT with KEY.")
     (let ((key-xor-ipad (make-string ,B ?\x36))
           (key-xor-opad (make-string ,B ?\x5C))
           (len (length key))
           (pos 0))
       (when (> len ,B)
         (setq key (hmac-unhex-string-macro (,H key) ,(* L 2)))
	 (setq len ,L))
       (while (< pos len)
         (aset key-xor-ipad pos (logxor (aref key pos) ?\x36))
         (aset key-xor-opad pos (logxor (aref key pos) ?\x5C))
         (setq pos (1+ pos)))
       ;; If outer `hmac-unhex-string-macro' is removed, return value
       ;; will be in hexadecimal form.  It is useful for test.
       ,(if (and bit (< (/ bit 8) L))
	    `(substring
	      (hmac-unhex-string-macro
	       (,H
		(concat key-xor-opad
			(hmac-unhex-string-macro
			 (,H (concat key-xor-ipad text))
			 ,(* L 2))))
	       ,(* L 2))
	      0 ,(/ bit 8))
	  `(hmac-unhex-string-macro
	    (,H
	     (concat key-xor-opad
		     (hmac-unhex-string-macro
		      (,H (concat key-xor-ipad text))
		      ,(* L 2))))
	    ,(* L 2))))))

(provide 'hmac-def)

;;; hmac-def.el ends here.
