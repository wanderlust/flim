;;; mel-q.el: Quoted-Printable and Q-encoding encoder/decoder for GNU Emacs

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1995/6/25
;; Version: $Id$
;; Keywords: MIME, Quoted-Printable, Q-encoding

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

;;; Code:

(require 'emu)


;;; @ constants
;;;

(defconst quoted-printable-hex-chars "0123456789ABCDEF")
(defconst quoted-printable-octet-regexp
  (concat "=[" quoted-printable-hex-chars
	  "][" quoted-printable-hex-chars "]"))


;;; @ variables
;;;

(defvar quoted-printable-external-encoder '("mmencode" "-q")
  "*list of quoted-printable encoder program name and its arguments.")

(defvar quoted-printable-external-decoder '("mmencode" "-q" "-u")
  "*list of quoted-printable decoder program name and its arguments.")

(defvar quoted-printable-internal-encoding-limit 10000
  "*limit size to use internal quoted-printable encoder.
If size of input to encode is larger than this limit,
external encoder is called.")

(defvar quoted-printable-internal-decoding-limit nil
  "*limit size to use internal quoted-printable decoder.
If size of input to decode is larger than this limit,
external decoder is called.")


;;; @ Quoted-Printable (Q-encode) encoder/decoder
;;;

(defun byte-to-hex-string (num)
  (concat (char-to-string (elt quoted-printable-hex-chars (ash num -4)))
	  (char-to-string (elt quoted-printable-hex-chars (logand num 15)))
	  ))

(defun quoted-printable-quote-char (chr)
  (concat "="
	  (char-to-string (elt quoted-printable-hex-chars (ash chr -4)))
	  (char-to-string (elt quoted-printable-hex-chars (logand chr 15)))
	  ))


;;; @@ Quoted-Printable encoder/decoder for string
;;;

(defun quoted-printable-encode-string (string)
  "Encode STRING to quoted-printable, and return the result."
  (let ((i 0))
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ?\n)
			 (setq i 0)
			 "\n")
			((or (< chr 32) (< 126 chr) (eq chr ?=))
			 (if (>= i 73)
			     (progn
			       (setq i 3)
			       (concat "=\n" (quoted-printable-quote-char chr))
			       )
			   (progn
			     (setq i (+ i 3))
			     (quoted-printable-quote-char chr)
			     )))
			(t (if (>= i 75)
			       (progn
				 (setq i 1)
				 (concat "=\n" (char-to-string chr))
				 )
			     (progn
			       (setq i (1+ i))
			       (char-to-string chr)
			       )))
			)))
	       string "")))

(defun quoted-printable-decode-string (string)
  "Decode STRING which is encoded in quoted-printable, and return the result."
  (let (q h l)
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ?=)
			 (setq q t)
			 "")
			(q (setq h
				 (cond ((<= ?a chr) (+ (- chr ?a) 10))
				       ((<= ?A chr) (+ (- chr ?A) 10))
				       ((<= ?0 chr) (- chr ?0))
				       ))
			   (setq q nil)
			   "")
			(h (setq l (cond ((<= ?a chr) (+ (- chr ?a) 10))
					 ((<= ?A chr) (+ (- chr ?A) 10))
					 ((<= ?0 chr) (- chr ?0))
					 ))
			   (prog1
			       (char-to-string (logior (ash h 4) l))
			     (setq h nil)
			     )
			   )
			(t (char-to-string chr))
			)))
	       string "")))


;;; @@ Quoted-Printable encoder/decoder for region
;;;

(defun quoted-printable-internal-encode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let ((str (buffer-substring beg end)))
	(delete-region beg end)
	(insert (quoted-printable-encode-string str))
	)
      (or (bolp)
	  (insert "=\n")
	  )
      )))

(defun quoted-printable-internal-decode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "=\n" nil t)
	(replace-match "")
	)
      (goto-char (point-min))
      (let (b e str)
	(while (re-search-forward quoted-printable-octet-regexp nil t)
	  (setq b (match-beginning 0))
	  (setq e (match-end 0))
	  (setq str (buffer-substring b e))
	  (delete-region b e)
	  (insert (quoted-printable-decode-string str))
	  ))
      )))

(defun quoted-printable-external-encode-region (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (as-binary-process
       (apply (function call-process-region)
	      beg end (car quoted-printable-external-encoder)
	      t t nil (cdr quoted-printable-external-encoder))
       )
      ;; for OS/2
      ;;   regularize line break code
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
	(replace-match "")
	)
      )))

(defun quoted-printable-external-decode-region (beg end)
  (save-excursion
    (as-binary-process
     (apply (function call-process-region)
	    beg end (car quoted-printable-external-decoder)
	    t t nil (cdr quoted-printable-external-decoder))
     )))

(defun quoted-printable-encode-region (beg end)
  "Encode current region by quoted-printable.
START and END are buffer positions.
This function calls internal quoted-printable encoder if size of
region is smaller than `quoted-printable-internal-encoding-limit',
otherwise it calls external quoted-printable encoder specified by
`quoted-printable-external-encoder'.  In this case, you must install
the program (maybe mmencode included in metamail or XEmacs package)."
  (interactive "r")
  (if (and quoted-printable-internal-encoding-limit
	   (> (- end beg) quoted-printable-internal-encoding-limit))
      (quoted-printable-external-encode-region beg end)
    (quoted-printable-internal-encode-region beg end)
    ))

(defun quoted-printable-decode-region (beg end)
  "Decode current region by quoted-printable.
START and END are buffer positions.
This function calls internal quoted-printable decoder if size of
region is smaller than `quoted-printable-internal-decoding-limit',
otherwise it calls external quoted-printable decoder specified by
`quoted-printable-external-decoder'.  In this case, you must install
the program (maybe mmencode included in metamail or XEmacs package)."
  (interactive "r")
  (if (and quoted-printable-internal-decoding-limit
	   (> (- end beg) quoted-printable-internal-decoding-limit))
      (quoted-printable-external-decode-region beg end)
    (quoted-printable-internal-decode-region beg end)
    ))


;;; @@ Quoted-Printable encoder/decoder for file
;;;

(defun quoted-printable-insert-encoded-file (filename)
  "Encode contents of file FILENAME to quoted-printable, and insert the result.
It calls external quoted-printable encoder specified by
`quoted-printable-external-encoder'.  So you must install the program
(maybe mmencode included in metamail or XEmacs package)."
  (interactive (list (read-file-name "Insert encoded file: ")))
  (apply (function call-process) (car quoted-printable-external-encoder)
	 filename t nil (cdr quoted-printable-external-encoder))
  )


;;; @ Q-encoding encode/decode string
;;;

(defconst q-encoding-special-chars-alist
  '((text	?= ?? ?_)
    (comment	?= ?? ?_ ?\( ?\) ?\\)
    (phrase	?= ?? ?_ ?\( ?\) ?\\ ?\" ?# ?$ ?% ?& ?' ?, ?. ?/
		?: ?\; ?< ?> ?@ ?\[ ?\] ?^ ?` ?{ ?| ?} ?~)
    ))

(defun q-encoding-encode-string (string &optional mode)
  "Encode STRING to Q-encoding of encoded-word, and return the result.
MODE allows `text', `comment', `phrase' or nil.  Default value is
`phrase'."
  (let ((specials (cdr (or (assq mode q-encoding-special-chars-alist)
			   (assq 'phrase q-encoding-special-chars-alist)
			   ))))
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr 32) "_")
			((or (< chr 32) (< 126 chr)
			     (memq chr specials)
			     )
			 (quoted-printable-quote-char chr)
			 )
			(t
			 (char-to-string chr)
			 ))
		  ))
	       string "")
    ))

(defun q-encoding-decode-string (string)
  "Decode STRING which is encoded in Q-encoding and return the result."
  (let (q h l)
    (mapconcat (function
		(lambda (chr)
		  (cond ((eq chr ?_) " ")
			((eq chr ?=)
			 (setq q t)
			 "")
			(q (setq h (cond ((<= ?a chr) (+ (- chr ?a) 10))
					 ((<= ?A chr) (+ (- chr ?A) 10))
					 ((<= ?0 chr) (- chr ?0))
					 ))
			   (setq q nil)
			   "")
			(h (setq l (cond ((<= ?a chr) (+ (- chr ?a) 10))
					 ((<= ?A chr) (+ (- chr ?A) 10))
					 ((<= ?0 chr) (- chr ?0))
					 ))
			   (prog1
			       (char-to-string (logior (ash h 4) l))
			     (setq h nil)
			     )
			   )
			(t (char-to-string chr))
			)))
	       string "")))


;;; @@ etc
;;;

(defun q-encoding-printable-char-p (chr mode)
  (and (not (memq chr '(?= ?? ?_)))
       (<= ?\   chr)(<= chr ?~)
       (cond ((eq mode 'text) t)
	     ((eq mode 'comment)
	      (not (memq chr '(?\( ?\) ?\\)))
	      )
	     (t
	      (string-match "[A-Za-z0-9!*+/=_---]" (char-to-string chr))
	      ))))

(defun q-encoding-encoded-length (string &optional mode)
  (let ((l 0)(i 0)(len (length string)) chr)
    (while (< i len)
      (setq chr (elt string i))
      (if (q-encoding-printable-char-p chr mode)
	  (setq l (+ l 1))
	(setq l (+ l 3))
	)
      (setq i (+ i 1)) )
    l))


;;; @ end
;;;

(provide 'mel-q)

;;; mel-q.el ends here
