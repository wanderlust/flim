(provide 'ew-var)

;;; user customizable variables.

(defvar ew-decode-sticked-encoded-word nil)
(defvar ew-decode-quoted-encoded-word nil)
(defvar ew-ignore-75bytes-limit nil)
(defvar ew-ignore-76bytes-limit nil)
(defvar ew-permit-sticked-comment nil)
(defvar ew-permit-sticked-special nil)

(defvar ew-parse-error-sit-for-seconds 0)

;;; anonymous function to decode ground string.
;; NOTE: STR is CRLF-form and it should return as CRLF-form.
(defvar ew-decode-us-ascii (lambda (str) (decode-coding-string str 'iso-latin-1-unix)))

;;;
(defvar ew-decode-field-syntax-alist
'(("from"               ew-scan-unibyte-std11 . ew:tag-mailbox+-tok)
  ("sender"             ew-scan-unibyte-std11 . ew:tag-mailbox-tok)
  ("to"                 ew-scan-unibyte-std11 . ew:tag-address+-tok)
  ("resent-to"          ew-scan-unibyte-std11 . ew:tag-address+-tok)
  ("cc"                 ew-scan-unibyte-std11 . ew:tag-address+-tok)
  ("resent-cc"          ew-scan-unibyte-std11 . ew:tag-address+-tok)
  ("bcc"                ew-scan-unibyte-std11 . ew:tag-address*-tok)
  ("resent-bcc"         ew-scan-unibyte-std11 . ew:tag-address*-tok)
  ("message-id"         ew-scan-unibyte-std11)
  ("resent-message-id"  ew-scan-unibyte-std11)
  ("in-reply-to"        ew-scan-unibyte-std11 . ew:tag-phrase-msg-id*-tok)
  ("references"         ew-scan-unibyte-std11 . ew:tag-phrase-msg-id*-tok)
  ("keywords"           ew-scan-unibyte-std11 . ew:tag-phrase*-tok)
  ("subject"            ew-scan-unibyte-unstructured)
  ("comments"           ew-scan-unibyte-unstructured)
  ("encrypted"          ew-scan-unibyte-std11)
  ("date"               ew-scan-unibyte-std11)
  ("reply-to"           ew-scan-unibyte-std11 . ew:tag-address+-tok)
  ("received"           ew-scan-unibyte-std11)
  ("resent-reply-to"    ew-scan-unibyte-std11 . ew:tag-address+-tok)
  ("resent-from"        ew-scan-unibyte-std11 . ew:tag-mailbox+-tok)
  ("resent-sender"      ew-scan-unibyte-std11 . ew:tag-mailbox-tok)
  ("resent-date"        ew-scan-unibyte-std11)
  ("return-path"        ew-scan-unibyte-std11)
  ("mime-version"       ew-scan-unibyte-std11)
  ("content-type"       ew-scan-unibyte-mime)
  ("content-transfer-encoding"  ew-scan-unibyte-mime)
  ("content-id"         ew-scan-unibyte-mime)
  ("content-description"        ew-scan-unibyte-unstructured)
))

(defvar ew-decode-field-default-syntax '(ew-scan-unibyte-unstructured))

;;; constants.

(defconst ew-token-regexp "[-!#-'*+0-9A-Z^-~]+")
(defconst ew-encoded-text-regexp "[!->@-~]+")
(defconst ew-encoded-word-regexp
  (concat (regexp-quote "=?")
          "\\(" ew-token-regexp "\\)"
          (regexp-quote "?")
          "\\(" ew-token-regexp "\\)"
          (regexp-quote "?")
          "\\(" ew-encoded-text-regexp "\\)"
          (regexp-quote "?=")))

;;; utilities for variables.

(defun ew-dynamic-options ()
  (list
   ew-decode-sticked-encoded-word
   ew-decode-quoted-encoded-word
   ew-ignore-75bytes-limit
   ew-ignore-76bytes-limit
   ew-permit-sticked-comment
   ew-permit-sticked-special))
