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

(provide 'mel)
