;;; sha1.el --- SHA1 Message Digest Algorithm.
;; Copyright (C) 1998,1999 Keiichi Suzuki.

;; Author: Keiichi Suzuki <kei-suzu@mail.wbs.ne.jp>
;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 1998-12-25
;; Revised: 1999-01-13
;; Keywords: sha1, news, cancel-lock, hmac, rfc2104

;; This file is part of FLIM (Faithful Library about Internet Message).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; Commentary:

;; This is a direct translation into Emacs LISP of the reference C
;; implementation of the SHA1 message digest algorithm.

;;; Usage:

;; To compute the SHA1 message digest for a message M (represented as
;; a string), call
;; 
;;   (sha1-encode M)
;;
;; which returns the message digest as a hexadecimal string of 20 bytes.
;; If you need to supply the message in pieces M1, M2, ... Mn, then call
;; 
;;   (sha1-init)
;;   (sha1-update M1)
;;   (sha1-update M2)
;;   ...
;;   (sha1-update Mn)
;;   (sha1-final)

;;; Notes:

;; The C algorithm uses 32-bit integers; because GNU Emacs
;; implementations provide 28-bit integers (with 24-bit integers on
;; versions prior to 19.29), the code represents a 32-bit integer as the
;; cons of two 16-bit integers.  The most significant word is stored in
;; the car and the least significant in the cdr.  The algorithm requires
;; at least 19 bits of integer representation in order to represent the
;; carry from a 16-bit addition. (see sha1-add())

;;; Code:

(defmacro sha1-f1 (x y z)
  `(cons
    (logior (logand (car ,x) (car ,y)) (logand (lognot (car ,x)) (car ,z)))
    (logior (logand (cdr ,x) (cdr ,y)) (logand (lognot (cdr ,x)) (cdr ,z)))
    ))

(defmacro sha1-f2 (x y z)
  `(cons
    (logxor (car ,x) (car ,y) (car ,z))
    (logxor (cdr ,x) (cdr ,y) (cdr ,z))
    ))

(defmacro sha1-f3 (x y z)
  `(cons
    (logior (logand (car ,x) (car ,y)) (logand (car ,x) (car ,z))
	    (logand (car ,y) (car ,z)))
    (logior (logand (cdr ,x) (cdr ,y)) (logand (cdr ,x) (cdr ,z))
	    (logand (cdr ,y) (cdr ,z)))
    ))

(defmacro sha1-f4 (x y z)
  `(cons
    (logxor (car ,x) (car ,y) (car ,z))
    (logxor (cdr ,x) (cdr ,y) (cdr ,z))
    ))

(defconst sha1-const1 '(23170 . 31129)
  "SHA constants 1 \(0x5a827999\)")
(defconst sha1-const2 '(28377 . 60321)
  "SHA constants 2 \(0x6ed9eba1\)")
(defconst sha1-const3 '(36635 . 48348)
  "SHA constants 3 \(0x8f1bbcdc\)")
(defconst sha1-const4 '(51810 . 49622)
  "SHA constants 4 \(0xca62c1d6\)")

(defvar sha1-digest (make-vector 5 nil))
(defvar sha1-count-lo nil)
(defvar sha1-count-hi nil)
(defvar sha1-data nil)
(defvar sha1-local nil)
(defconst SHA1-BLOCKSIZE 64)

(defun sha1-init ()
  "Initialize the state of the SHA1 message digest routines."
  (aset sha1-digest 0 (cons 26437 8961))
  (aset sha1-digest 1 (cons 61389 43913))
  (aset sha1-digest 2 (cons 39098 56574))
  (aset sha1-digest 3 (cons  4146 21622))
  (aset sha1-digest 4 (cons 50130 57840))
  (setq sha1-count-lo (cons 0 0)
	sha1-count-hi (cons 0 0)
	sha1-local 0
	sha1-data nil)
  )

(defmacro sha1-32-make (v)
  "Return 32bits internal value from normal integer."
  `(cons (lsh ,v -16) (logand 65535 ,v)))

(defun sha1-add (to &rest vals)
  "Set sum of all the arguments to the first one."
  (let (val)
    (while (setq val (car vals))
      (setcar to (+ (car to) (car val)))
      (setcdr to (+ (cdr to) (cdr val)))
      (setq vals (cdr vals))
      )
    (setcar to (logand 65535 (+ (car to) (lsh (cdr to) -16))))
    (setcdr to (logand 65535 (cdr to)))
    to
    ))

(defun sha1-xor (to &rest vals)
  "Set bitwise-exclusive-or of all the arguments to the first one."
  (let (val)
    (while (setq val (car vals))
      (setcar to (logxor (car to) (car val)))
      (setcdr to (logxor (cdr to) (cdr val)))
      (setq vals (cdr vals)))
    ))

(defmacro sha1-rot (val c1 c2)
  "Internal macro for sha1-rot-*."
  `(cons
    (logand 65535 (logior (lsh (car ,val) ,c1) (lsh (cdr ,val) ,c2)))
    (logand 65535 (logior (lsh (cdr ,val) ,c1) (lsh (car ,val) ,c2)))
    ))

(defmacro sha1-rot-1 (val)
  "Return VAL with its bits rotated left by 1."
  `(sha1-rot ,val 1 -15)
  )

(defmacro sha1-rot-5 (val)
  "Return VAL with its bits rotated left by 5."
  `(sha1-rot ,val 5 -11)
  )

(defmacro sha1-rot-30 (val)
  "Return VAL with its bits rotated left by 30."
  `(sha1-rot ,val -2 14)
  )

(defun sha1-inc (to)
  "Set TO pulus one to TO."
  (setcdr to (1+ (cdr to)))
  (when (> (cdr to) 65535)
    (setcdr to (logand 65535 (cdr to)))
    (setcar to (logand 65535 (1+ (car to))))))

(defun sha1-lsh (to v count)
  "Set TO with its bits shifted left by COUNT to TO."
  (setcar to (logand 65535
		     (logior (lsh (car v) count) (lsh (cdr v) (- count 16)))))
  (setcdr to (logand 65535 (lsh (cdr v) count)))
  to
  )

(defun sha1-rsh (to v count)
  "Set TO with its bits shifted right by COUNT to TO."
  (setq count (- 0 count))
  (setcdr to (logand 65535
		     (logior (lsh (cdr v) count) (lsh (car v) (- count 16)))))
  (setcar to (logand 65535 (lsh (car v) count)))
  to
  )

(defun sha1-< (v1 v2)
  "Return t if firast argment is less then second argument."
  (or (< (car v1) (car v2))
      (and (eq (car v1) (car v2))
	   (< (cdr v1) (cdr v2))))
  )

(unless (fboundp 'string-as-unibyte)
  (defsubst string-as-unibyte (string)
    string)
  )

(defun sha1-update (bytes)
  "Update the current SHA1 state with BYTES (an string of uni-bytes)."
  (setq bytes (string-as-unibyte bytes))
  (let* ((len (length bytes))
	 (len32 (sha1-32-make len))
	 (tmp32 (cons 0 0))
	 (top 0)
	 (clo (cons 0 0))
	 i done)
    (sha1-add clo sha1-count-lo (sha1-lsh tmp32 len32 3))
    (when (sha1-< clo sha1-count-lo)
      (sha1-inc sha1-count-hi))
    (setq sha1-count-lo clo)
    (sha1-add sha1-count-hi (sha1-rsh tmp32 len32 29))
    (when (> (length sha1-data) 0)
      (setq i (- SHA1-BLOCKSIZE (length sha1-data)))
      (when (> i len)
	(setq i len))
      (setq sha1-data (concat sha1-data (substring bytes 0 i)))
      (setq len (- len i)
	    top i)
      (if (eq (length sha1-data) SHA1-BLOCKSIZE)
	  (sha1-transform)
	(setq done t)))
    (when (not done)
      (while (and (not done)
		  (>= len SHA1-BLOCKSIZE))
	(setq sha1-data (substring bytes top (+ top SHA1-BLOCKSIZE))
	      top (+ top SHA1-BLOCKSIZE)
	      len (- len SHA1-BLOCKSIZE))
	(sha1-transform))
      (setq sha1-data (substring bytes top (+ top len))))
    ))

(defmacro sha1-FA (n)
  (let ((func (intern (format "sha1-f%d" n)))
	(const (intern (format "sha1-const%d" n))))
    `(setq T (sha1-add (cons 0 0) (sha1-rot-5 A) (,func B C D) E (aref W WIDX)
		       ,const)
	   WIDX (1+ WIDX)
	   B (sha1-rot-30 B))))

(defmacro sha1-FB (n)
  (let ((func (intern (format "sha1-f%d" n)))
	(const (intern (format "sha1-const%d" n))))
    `(setq E (sha1-add (cons 0 0) (sha1-rot-5 T) (,func A B C) D (aref W WIDX)
		       ,const)
	   WIDX (1+ WIDX)
	   A (sha1-rot-30 A))))

(defmacro sha1-FC (n)
  (let ((func (intern (format "sha1-f%d" n)))
	(const (intern (format "sha1-const%d" n))))
    `(setq D (sha1-add (cons 0 0) (sha1-rot-5 E) (,func T A B) C (aref W WIDX)
		       ,const)
	   WIDX (1+ WIDX)
	   T (sha1-rot-30 T))))

(defmacro sha1-FD (n)
  (let ((func (intern (format "sha1-f%d" n)))
	(const (intern (format "sha1-const%d" n))))
    `(setq C (sha1-add (cons 0 0) (sha1-rot-5 D) (,func E T A) B (aref W WIDX)
		       ,const)
	   WIDX (1+ WIDX)
	   E (sha1-rot-30 E))))

(defmacro sha1-FE (n)
  (let ((func (intern (format "sha1-f%d" n)))
	(const (intern (format "sha1-const%d" n))))
    `(setq B (sha1-add (cons 0 0) (sha1-rot-5 C) (,func D E T) A (aref W WIDX)
		       ,const)
	   WIDX (1+ WIDX)
	   D (sha1-rot-30 D))))

(defmacro sha1-FT (n)
  (let ((func (intern (format "sha1-f%d" n)))
	(const (intern (format "sha1-const%d" n))))
    `(setq A (sha1-add (cons 0 0) (sha1-rot-5 B) (,func C D E) T (aref W WIDX)
		       ,const)
	   WIDX (1+ WIDX)
	   C (sha1-rot-30 C))))

(defun sha1-transform ()
  "Basic SHA1 step. Transform sha1-digest based on sha1-data."
  (let ((W (make-vector 80 nil))
	(WIDX 0)
	(bidx 0)
	T A B C D E)
    (while (< WIDX 16)
      (aset W WIDX
	    (cons (logior (lsh (aref sha1-data bidx) 8)
			  (aref sha1-data (setq bidx (1+ bidx))))
		  (logior (lsh (aref sha1-data (setq bidx (1+ bidx))) 8)
			  (aref sha1-data (setq bidx (1+ bidx))))))
      (setq bidx (1+ bidx)
	    WIDX (1+ WIDX)))
    (while (< WIDX 80)
      (aset W WIDX (cons 0 0))
      (sha1-xor (aref W WIDX)
		   (aref W (- WIDX 3)) (aref W (- WIDX 8))
		   (aref W (- WIDX 14)) (aref W (- WIDX 16)))
      (aset W WIDX (sha1-rot-1 (aref W WIDX)))
      (setq WIDX (1+ WIDX)))
    (setq A (cons (car (aref sha1-digest 0)) (cdr (aref sha1-digest 0)))
	  B (cons (car (aref sha1-digest 1)) (cdr (aref sha1-digest 1)))
	  C (cons (car (aref sha1-digest 2)) (cdr (aref sha1-digest 2)))
	  D (cons (car (aref sha1-digest 3)) (cdr (aref sha1-digest 3)))
	  E (cons (car (aref sha1-digest 4)) (cdr (aref sha1-digest 4)))
	  WIDX 0)

    (sha1-FA 1) (sha1-FB 1) (sha1-FC 1) (sha1-FD 1) (sha1-FE 1) (sha1-FT 1)
    (sha1-FA 1) (sha1-FB 1) (sha1-FC 1) (sha1-FD 1) (sha1-FE 1) (sha1-FT 1)
    (sha1-FA 1) (sha1-FB 1) (sha1-FC 1) (sha1-FD 1) (sha1-FE 1) (sha1-FT 1)
    (sha1-FA 1) (sha1-FB 1) (sha1-FC 2) (sha1-FD 2) (sha1-FE 2) (sha1-FT 2)
    (sha1-FA 2) (sha1-FB 2) (sha1-FC 2) (sha1-FD 2) (sha1-FE 2) (sha1-FT 2)
    (sha1-FA 2) (sha1-FB 2) (sha1-FC 2) (sha1-FD 2) (sha1-FE 2) (sha1-FT 2)
    (sha1-FA 2) (sha1-FB 2) (sha1-FC 2) (sha1-FD 2) (sha1-FE 3) (sha1-FT 3)
    (sha1-FA 3) (sha1-FB 3) (sha1-FC 3) (sha1-FD 3) (sha1-FE 3) (sha1-FT 3)
    (sha1-FA 3) (sha1-FB 3) (sha1-FC 3) (sha1-FD 3) (sha1-FE 3) (sha1-FT 3)
    (sha1-FA 3) (sha1-FB 3) (sha1-FC 3) (sha1-FD 3) (sha1-FE 3) (sha1-FT 3)
    (sha1-FA 4) (sha1-FB 4) (sha1-FC 4) (sha1-FD 4) (sha1-FE 4) (sha1-FT 4)
    (sha1-FA 4) (sha1-FB 4) (sha1-FC 4) (sha1-FD 4) (sha1-FE 4) (sha1-FT 4)
    (sha1-FA 4) (sha1-FB 4) (sha1-FC 4) (sha1-FD 4) (sha1-FE 4) (sha1-FT 4)
    (sha1-FA 4) (sha1-FB 4)

    (sha1-add (aref sha1-digest 0) E)
    (sha1-add (aref sha1-digest 1) T)
    (sha1-add (aref sha1-digest 2) A)
    (sha1-add (aref sha1-digest 3) B)
    (sha1-add (aref sha1-digest 4) C)
    ))

(defun sha1-final (&optional binary)
  "Transform buffered sha1-data and return SHA1 message digest.
If optional argument BINARY is non-nil, then return binary formed 
string of message digest."
  (let ((count (logand (lsh (cdr sha1-count-lo) -3) 63)))
    (when (< (length sha1-data) SHA1-BLOCKSIZE)
      (setq sha1-data
	    (concat sha1-data
		    (make-string (- SHA1-BLOCKSIZE (length sha1-data)) 0))))
    (aset sha1-data count 128)
    (setq count (1+ count))
    (if (> count (- SHA1-BLOCKSIZE 8))
	(progn
	  (setq sha1-data (concat (substring sha1-data 0 count)
				  (make-string (- SHA1-BLOCKSIZE count) 0)))
	  (sha1-transform)
	  (setq sha1-data (concat (make-string (- SHA1-BLOCKSIZE 8) 0)
				  (substring sha1-data -8))))
      (setq sha1-data (concat (substring sha1-data 0 count)
			      (make-string (- SHA1-BLOCKSIZE 8 count) 0)
			      (substring sha1-data -8))))
    (aset sha1-data 56 (lsh (car sha1-count-hi) -8))
    (aset sha1-data 57 (logand 255 (car sha1-count-hi)))
    (aset sha1-data 58 (lsh (cdr sha1-count-hi) -8))
    (aset sha1-data 59 (logand 255 (cdr sha1-count-hi)))
    (aset sha1-data 60 (lsh (car sha1-count-lo) -8))
    (aset sha1-data 61 (logand 255 (car sha1-count-lo)))
    (aset sha1-data 62 (lsh (cdr sha1-count-lo) -8))
    (aset sha1-data 63 (logand 255 (cdr sha1-count-lo)))
    (sha1-transform)
    (if binary
	(mapconcat
	 (lambda (elem)
	   (concat (char-to-string (/ (car elem) 256))
		   (char-to-string (% (car elem) 256))
		   (char-to-string (/ (cdr elem) 256))
		   (char-to-string (% (cdr elem) 256))))
	 (list (aref sha1-digest 0) (aref sha1-digest 1) (aref sha1-digest 2)
	       (aref sha1-digest 3) (aref sha1-digest 4))
	 "")
      (format "%04x%04x%04x%04x%04x%04x%04x%04x%04x%04x"
	      (car (aref sha1-digest 0)) (cdr (aref sha1-digest 0))
	      (car (aref sha1-digest 1)) (cdr (aref sha1-digest 1))
	      (car (aref sha1-digest 2)) (cdr (aref sha1-digest 2))
	      (car (aref sha1-digest 3)) (cdr (aref sha1-digest 3))
	      (car (aref sha1-digest 4)) (cdr (aref sha1-digest 4)))
      )))

(defun sha1-encode (message &optional binary)
  "Encodes MESSAGE using the SHA1 message digest algorithm.
MESSAGE must be a unibyte-string.
By default, return a string which formed hex-decimal charcters
from message digest.
If optional argument BINARY is non-nil, then return binary formed
string of message digest."
  (sha1-init)
  (sha1-update message)
  (sha1-final binary))

(defun sha1-encode-binary (message)
  "Encodes MESSAGE using the SHA1 message digest algorithm.
MESSAGE must be a unibyte-string.
Return binary formed string of message digest."
  (sha1-encode message 'binary))

(provide 'sha1)

;;; sha1.el ends here
