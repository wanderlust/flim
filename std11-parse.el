;;; std11-parse.el --- STD 11 parser for GNU Emacs

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, RFC 822, STD 11
;; Version: $Id$

;; This file is part of MU (Message Utilities).

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

(require 'std11)
(require 'emu)


;;; @ lexical analyze
;;;

(defconst std11-space-chars " \t\n")
(defconst std11-spaces-regexp (` (, (concat "[" std11-space-chars "]+"))))
(defconst std11-special-char-list '(?\] ?\[
					?\( ?\) ?< ?> ?@
					?, ?\; ?: ?\\ ?\"
					?.))
(defconst std11-atom-regexp
  (` (, (concat "^[^" std11-special-char-list std11-space-chars "]+"))))

(defun std11-analyze-spaces (string)
  (if (and (string-match std11-spaces-regexp string)
	   (= (match-beginning 0) 0))
      (let ((end (match-end 0)))
	(cons (cons 'spaces (substring string 0 end))
	      (substring string end)
	      ))))

(defun std11-analyze-special (str)
  (if (and (> (length str) 0)
	   (memq (aref str 0) std11-special-char-list))
      (cons (cons 'specials (substring str 0 1))
	    (substring str 1)
	    )))

(defun std11-analyze-atom (str)
  (if (string-match std11-atom-regexp str)
      (let ((end (match-end 0)))
	(cons (cons 'atom (substring str 0 end))
	      (substring str end)
	      ))))

(defun std11-check-enclosure (str open close &optional recursive from)
  (let ((len (length str))
	(i (or from 0))
	)
    (if (and (> len i)
	     (eq (aref str i) open))
	(let (p chr)
	  (setq i (1+ i))
	  (catch 'tag
	    (while (< i len)
	      (setq chr (aref str i))
	      (cond ((eq chr ?\\)
		     (setq i (1+ i))
		     (if (>= i len)
			 (throw 'tag nil)
		       )
		     (setq i (1+ i))
		     )
		    ((eq chr close)
		     (throw 'tag (1+ i))
		     )
		    ((eq chr open)
		     (if (and recursive
			      (setq p (std11-check-enclosure
				       str open close recursive i))
			      )
			 (setq i p)
		       (throw 'tag nil)
		       ))
		    (t
		     (setq i (1+ i))
		     ))
	      ))))))

(defun std11-analyze-quoted-string (str)
  (let ((p (std11-check-enclosure str ?\" ?\")))
    (if p
	(cons (cons 'quoted-string (substring str 1 (1- p)))
	      (substring str p))
      )))

(defun std11-analyze-domain-literal (str)
  (let ((p (std11-check-enclosure str ?\[ ?\])))
    (if p
	(cons (cons 'domain-literal (substring str 1 (1- p)))
	      (substring str p))
      )))

(defun std11-analyze-comment (str)
  (let ((p (std11-check-enclosure str ?\( ?\) t)))
    (if p
	(cons (cons 'comment (substring str 1 (1- p)))
	      (substring str p))
      )))

(defun std11-lexical-analyze (str)
  (let (dest ret)
    (while (not (string-equal str ""))
      (setq ret
	    (or (std11-analyze-quoted-string str)
		(std11-analyze-domain-literal str)
		(std11-analyze-comment str)
		(std11-analyze-spaces str)
		(std11-analyze-special str)
		(std11-analyze-atom str)
		'((error) . "")
		))
      (setq dest (cons (car ret) dest))
      (setq str (cdr ret))
      )
    (nreverse dest)
    ))


;;; @ parser
;;;

(defun std11-ignored-token-p (token)
  (let ((type (car token)))
    (or (eq type 'spaces)(eq type 'comment))
    ))

(defun std11-parse-token (lal)
  (let (token itl)
    (while (and lal
		(progn
		  (setq token (car lal))
		  (std11-ignored-token-p token)
		  ))
      (setq lal (cdr lal))
      (setq itl (cons token itl))
      )
    (cons (nreverse (cons token itl))
	  (cdr lal))
    ))

(defun std11-parse-ascii-token (lal)
  (let (token itl parsed token-value)
    (while (and lal
		(setq token (car lal))
		(or (std11-ignored-token-p token)
		    (if (and (setq token-value (cdr token))
			     (find-non-ascii-charset-string token-value)
			     )
			(setq token nil)
		      )))
      (setq lal (cdr lal))
      (setq itl (cons token itl))
      )
    (if (and token
	     (setq parsed (nreverse (cons token itl)))
	     )
	(cons parsed (cdr lal))
      )))

(defun std11-parse-token-or-comment (lal)
  (let (token itl)
    (while (and lal
		(progn
		  (setq token (car lal))
		  (eq (car token) 'spaces)
		  ))
      (setq lal (cdr lal))
      (setq itl (cons token itl))
      )
    (cons (nreverse (cons token itl))
	  (cdr lal))
    ))

(defun std11-parse-word (lal)
  (let ((ret (std11-parse-ascii-token lal)))
    (if ret
	(let ((elt (car ret))
	      (rest (cdr ret))
	      )
	  (if (or (assq 'atom elt)
		  (assq 'quoted-string elt))
	      (cons (cons 'word elt) rest)
	    )))))

(defun std11-parse-word-or-comment (lal)
  (let ((ret (std11-parse-token-or-comment lal)))
    (if ret
	(let ((elt (car ret))
	      (rest (cdr ret))
	      )
	  (cond ((or (assq 'atom elt)
		     (assq 'quoted-string elt))
		 (cons (cons 'word elt) rest)
		 )
		((assq 'comment elt)
		 (cons (cons 'comment-word elt) rest)
		 ))
	  ))))

(defun std11-parse-phrase (lal)
  (let (ret phrase)
    (while (setq ret (std11-parse-word-or-comment lal))
      (setq phrase (append phrase (cdr (car ret))))
      (setq lal (cdr ret))
      )
    (if phrase
	(cons (cons 'phrase phrase) lal)
      )))

(defun std11-parse-local-part (lal)
  (let ((ret (std11-parse-word lal)))
    (if ret
	(let ((local-part (cdr (car ret))) dot)
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (setq dot (car ret))
		      (string-equal (cdr (assq 'specials dot)) ".")
		      (setq ret (std11-parse-word (cdr ret)))
		      (setq local-part
			    (append local-part dot (cdr (car ret)))
			    )
		      (setq lal (cdr ret))
		      ))
	  (cons (cons 'local-part local-part) lal)
	  ))))

(defun std11-parse-sub-domain (lal)
  (let ((ret (std11-parse-ascii-token lal)))
    (if ret
	(let ((sub-domain (car ret)))
	  (if (or (assq 'atom sub-domain)
		  (assq 'domain-literal sub-domain)
		  )
	      (cons (cons 'sub-domain sub-domain)
		    (cdr ret)
		    )
	    )))))

(defun std11-parse-domain (lal)
  (let ((ret (std11-parse-sub-domain lal)))
    (if ret
	(let ((domain (cdr (car ret))) dot)
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (setq dot (car ret))
		      (string-equal (cdr (assq 'specials dot)) ".")
		      (setq ret (std11-parse-sub-domain (cdr ret)))
		      (setq domain
			    (append domain dot (cdr (car ret)))
			    )
		      (setq lal (cdr ret))
		      ))
	  (cons (cons 'domain domain) lal)
	  ))))

(defun std11-parse-at-domain (lal)
  (let ((ret (std11-parse-ascii-token lal)) at-sign)
    (if (and ret
	     (setq at-sign (car ret))
	     (string-equal (cdr (assq 'specials at-sign)) "@")
	     (setq ret (std11-parse-domain (cdr ret)))
	     )
	(cons (cons 'at-domain (append at-sign (cdr (car ret))))
	      (cdr ret))
      )))

(defun std11-parse-addr-spec (lal)
  (let ((ret (std11-parse-local-part lal))
	addr)
    (if (and ret
	     (prog1
		 (setq addr (cdr (car ret)))
	       (setq lal (cdr ret))
	       (and (setq ret (std11-parse-at-domain lal))
		    (setq addr (append addr (cdr (car ret))))
		    (setq lal (cdr ret))
		    )))
	(cons (cons 'addr-spec addr) lal)
      )))

(defun std11-parse-route (lal)
  (let ((ret (std11-parse-at-domain lal))
	route comma colon)
    (if (and ret
	     (progn
	       (setq route (cdr (car ret)))
	       (setq lal (cdr ret))
	       (while (and (setq ret (std11-parse-ascii-token lal))
			   (setq comma (car ret))
			   (string-equal (cdr (assq 'specials comma)) ",")
			   (setq ret (std11-parse-at-domain (cdr ret)))
			   )
		 (setq route (append route comma (cdr (car ret))))
		 (setq lal (cdr ret))
		 )
	       (and (setq ret (std11-parse-ascii-token lal))
		    (setq colon (car ret))
		    (string-equal (cdr (assq 'specials colon)) ":")
		    (setq route (append route colon))
		    )
	       ))
	(cons (cons 'route route)
	      (cdr ret)
	      )
      )))

(defun std11-parse-route-addr (lal)
  (let ((ret (std11-parse-ascii-token lal))
	< route addr-spec >)
    (if (and ret
	     (setq < (car ret))
	     (string-equal (cdr (assq 'specials <)) "<")
	     (setq lal (cdr ret))
	     (progn (and (setq ret (std11-parse-route lal))
			 (setq route (cdr (car ret)))
			 (setq lal (cdr ret))
			 )
		    (setq ret (std11-parse-addr-spec lal))
		    )
	     (setq addr-spec (cdr (car ret)))
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-ascii-token lal))
	     (setq > (car ret))
	     (string-equal (cdr (assq 'specials >)) ">")
	     )
	(cons (cons 'route-addr (append route addr-spec))
	      (cdr ret)
	      )
      )))

(defun std11-parse-phrase-route-addr (lal)
  (let ((ret (std11-parse-phrase lal)) phrase)
    (if ret
	(progn
	  (setq phrase (cdr (car ret)))
	  (setq lal (cdr ret))
	  ))
    (if (setq ret (std11-parse-route-addr lal))
	(cons (list 'phrase-route-addr
		    phrase
		    (cdr (car ret)))
	      (cdr ret))
      )))

(defun std11-parse-mailbox (lal)
  (let ((ret (or (std11-parse-phrase-route-addr lal)
		 (std11-parse-addr-spec lal)))
	mbox comment)
    (if (and ret
	     (prog1
		 (setq mbox (car ret))
	       (setq lal (cdr ret))
	       (if (and (setq ret (std11-parse-token-or-comment lal))
			(setq comment (cdr (assq 'comment (car ret))))
			)
		   (setq lal (cdr ret))
		 )))
	(cons (list 'mailbox mbox comment)
	      lal)
      )))

(defun std11-parse-group (lal)
  (let ((ret (std11-parse-phrase lal))
	phrase colon comma mbox semicolon)
    (if (and ret
	     (setq phrase (cdr (car ret)))
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-ascii-token lal))
	     (setq colon (car ret))
	     (string-equal (cdr (assq 'specials colon)) ":")
	     (setq lal (cdr ret))
	     (progn
	       (and (setq ret (std11-parse-mailbox lal))
		    (setq mbox (list (car ret)))
		    (setq lal (cdr ret))
		    (progn
		      (while (and (setq ret (std11-parse-ascii-token lal))
				  (setq comma (car ret))
				  (string-equal
				   (cdr (assq 'specials comma)) ",")
				  (setq lal (cdr ret))
				  (setq ret (std11-parse-mailbox lal))
				  (setq mbox (cons (car ret) mbox))
				  (setq lal (cdr ret))
				  )
			)))
	       (and (setq ret (std11-parse-ascii-token lal))
		    (setq semicolon (car ret))
		    (string-equal (cdr (assq 'specials semicolon)) ";")
		    )))
	(cons (list 'group phrase (nreverse mbox))
	      (cdr ret)
	      )
      )))

(defun std11-parse-address (lal)
  (or (std11-parse-group lal)
      (std11-parse-mailbox lal)
      ))

(defun std11-parse-addresses (lal)
  (let ((ret (std11-parse-address lal)))
    (if ret
	(let ((dest (list (car ret))))
	  (setq lal (cdr ret))
	  (while (and (setq ret (std11-parse-ascii-token lal))
		      (string-equal (cdr (assq 'specials (car ret))) ",")
		      (setq ret (std11-parse-address (cdr ret)))
		      )
	    (setq dest (cons (car ret) dest))
	    (setq lal (cdr ret))
	    )
	  (nreverse dest)
	  ))))

(defun std11-parse-msg-id (lal)
  (let ((ret (std11-parse-ascii-token lal))
	< addr-spec >)
    (if (and ret
	     (setq < (car ret))
	     (string-equal (cdr (assq 'specials <)) "<")
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-addr-spec lal))
	     (setq addr-spec (car ret))
	     (setq lal (cdr ret))
	     (setq ret (std11-parse-ascii-token lal))
	     (setq > (car ret))
	     (string-equal (cdr (assq 'specials >)) ">")
	     )
	(cons (cons 'msg-id (cdr addr-spec))
	      (cdr ret))
      )))


;;; @ end
;;;

(provide 'std11-parse)

;;; std11-parse.el ends here
