;;;
;;; mel : a MIME encoding library
;;;
;;;	by MORIOKA Tomohiko <morioka@jaist.ac.jp>, 1995/6/25
;;;
;;; $Id$
;;;

(autoload 'base64-encode-region "mel-b" nil t)
(autoload 'base64-decode-region "mel-b" nil t)
(autoload 'base64-encode-string "mel-b")
(autoload 'base64-decode-string "mel-b")
(autoload 'base64-encoded-length "mel-b")

(autoload 'quoted-printable-encode-region "mel-q" nil t)
(autoload 'quoted-printable-decode-region "mel-q" nil t)

(autoload 'q-encoding-encode-string-for-text "mel-q")
(autoload 'q-encoding-encode-string-for-comment "mel-q")
(autoload 'q-encoding-encode-string-for-phrase "mel-q")
(autoload 'q-encoding-encode-string "mel-q")
(autoload 'q-encoding-decode-string "mel-q")
(autoload 'q-encoding-encoded-length "mel-q")

(autoload 'uuencode-encode-region "mel-u" nil t)
(autoload 'uuencode-decode-region "mel-u" nil t)

(defvar mime-encoding-method-alist
  '(("base64"           . base64-encode-region)
    ("quoted-printable" . quoted-printable-encode-region)
    ("x-uue"            . uuencode-encode-region)
    ("7bit")
    ("8bit")
    ("binary")
    ))

(defvar mime-decoding-method-alist
  '(("base64"           . base64-decode-region)
    ("quoted-printable" . quoted-printable-decode-region)
    ("x-uue"            . uuencode-decode-region)
    ))


;;; @ region
;;;

(defun mime/encode-region (encoding beg end)
  "Encode region BEG to END of current buffer using ENCODING. [mel.el]"
  (interactive
   (list (completing-read "encoding: "
			  mime-encoding-method-alist
			  nil t "base64")
	 (region-beginning) (region-end))
   )
  (let ((f (cdr (assoc encoding mime-encoding-method-alist))))
    (if f
	(funcall f beg end)
      )))

(defun mime/decode-region (encoding beg end)
  "Decode region BEG to END of current buffer using ENCODING. [mel.el]"
  (interactive
   (list (completing-read "encoding: "
			  mime-decoding-method-alist
			  nil t "base64")
	 (region-beginning) (region-end))
   )
  (let ((f (cdr (assoc encoding mime-decoding-method-alist))))
    (if f
	(funcall f beg end)
      )))

(defalias 'mime-encode-region 'mime/encode-region)
(defalias 'mime-decode-region 'mime/decode-region)


;;; @ end
;;;

(provide 'mel)
