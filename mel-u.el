;;;
;;; $Id$
;;;

;;; @ variables
;;;

(defvar uuencode-external-encoder '("uuencode" "-")
  "*list of uuencode encoder program name and its arguments.")

(defvar uuencode-external-decoder '("uudecode")
  "*list of uuencode decoder program name and its arguments.")
(list "sh" "-c" (format "(cd %s; uudecode)" mime/tmp-dir))

(defvar mime/tmp-dir (or (getenv "TM_TMP_DIR") "/tmp/"))


;;; @ external encoder
;;;

(cond ((boundp 'MULE)
       (define-program-coding-system
	 nil (car uuencode-external-encoder) *noconv*)
       (define-program-coding-system
	 nil (car uuencode-external-decoder) *noconv*)
       )
      ((boundp 'NEMACS)
       (define-program-kanji-code
	 nil (car uuencode-external-encoder) 0)
       (define-program-kanji-code
	 nil (car uuencode-external-decoder) 0)
       ))

(defun uuencode-external-encode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let ((selective-display nil) ;Disable ^M to nl translation.
	  (mc-flag nil)      ;Mule
	  (kanji-flag nil))  ;NEmacs
      (apply (function call-process-region)
	     beg end (car uuencode-external-encoder)
	     t t nil (cdr uuencode-external-encoder))
      )))

(defun uuencode-external-decode-region (beg end)
  (interactive "*r")
  (save-excursion
    (let ((selective-display nil) ;Disable ^M to nl translation.
	  (mc-flag nil)		;Mule
	  (kanji-flag nil)	;NEmacs
	  (filename (save-excursion
		      (save-restriction
			(narrow-to-region beg end)
			(goto-char beg)
			(if (re-search-forward "^begin [0-9]+ " nil t)
			    (if (looking-at ".+$")
				(buffer-substring (match-beginning 0)
						  (match-end 0)
						  )
			      )))))
	  )
      (if filename
	  (progn
	    (funcall (function call-process-region)
		     beg end "sh"
		     t t nil
		     "-c" (format "(cd %s; %s)"
				  mime/tmp-dir
				  (mapconcat (function identity)
					     uuencode-external-decoder
					     " ")
				  ))
	    (setq filename (expand-file-name filename mime/tmp-dir))
	    (let ((file-coding-system-for-read
		   (if (boundp 'MULE) *noconv*))	; Mule
		  kanji-fileio-code)			; NEmacs
	      (insert-file-contents filename)
	      )
	    (delete-file filename)
	    ))
      )))

(defalias 'uuencode-encode-region 'uuencode-external-encode-region)
(defalias 'uuencode-decode-region 'uuencode-external-decode-region)


;;; @ end
;;;

(provide 'mel-u)
