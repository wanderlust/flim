(require 'closure)
(require 'ew-line)
(require 'ew-quote)
(require 'ew-bq)

(provide 'ew-unit)

(defun ew-b-check (encoding encoded-text) (string-match ew-b-regexp encoded-text))
(defun ew-q-check (encoding encoded-text) (string-match ew-q-regexp encoded-text))

(defsubst ew-eword-p (str)
  (let ((len (length str)))
    (and
     (<= 3 len)
     (eq (aref str 0) ?=)
     (eq (aref str 1) ??)
     (eq (aref str (- len 2)) ??)
     (eq (aref str (1- len)) ?=))))

(defun ew-decode-eword (str)
  (if (string-match ew-anchored-encoded-word-regexp str)
      (let ((charset (match-string 1 str))
	    (encoding (match-string 2 str))
	    (encoded-text (match-string 3 str))
	    bdec cdec
	    bcheck
	    tmp)
	(if (and (setq bdec (ew-byte-decoder encoding))
		 (setq cdec (ew-char-decoder charset)))
	    (if (or (null (setq bcheck (ew-byte-checker encoding)))
		    (funcall bcheck encoding encoded-text))
		(ew-quote (closure-call cdec (funcall bdec encoded-text)))
	      (ew-quote str))
	  (ew-quote-eword charset encoding encoded-text)))
    (ew-quote str)))

(defun ew-byte-decoder (encoding)
  (cdr (assoc (upcase encoding) ew-byte-decoder-alist)))

(defun ew-byte-checker (encoding)
  (cdr (assoc (upcase encoding) ew-byte-checker-alist)))

(defun ew-char-decoder (charset)
  (let ((sym (intern (downcase charset))))
    (when (mime-charset-to-coding-system sym 'LF)
      (closure-make
       (lambda (str) (decode-mime-charset-string str sym 'LF))
       sym))))
