(provide 'ew-var)

;;; user customizable variables.

(defvar ew-decode-sticked-encoded-word nil)
(defvar ew-decode-quoted-encoded-word nil)
(defvar ew-ignore-75bytes-limit nil)
(defvar ew-ignore-76bytes-limit nil)
(defvar ew-permit-sticked-comment nil)
(defvar ew-permit-sticked-special nil)
(defvar ew-permit-null-encoded-text nil) ; affect when loading time.

(defvar ew-remove-bare-crlf nil)
(defvar ew-default-mime-charset 'x-ctext)

;;;
(defvar ew-decode-field-syntax-alist
'((from                 ew-scan-unibyte-std11 . ew:tag-mailbox+)
  (sender               ew-scan-unibyte-std11 . ew:tag-mailbox)
  (to                   ew-scan-unibyte-std11 . ew:tag-address+)
  (resent-to            ew-scan-unibyte-std11 . ew:tag-address+)
  (cc                   ew-scan-unibyte-std11 . ew:tag-address+)
  (resent-cc            ew-scan-unibyte-std11 . ew:tag-address+)
  (bcc                  ew-scan-unibyte-std11 . ew:tag-address*)
  (resent-bcc           ew-scan-unibyte-std11 . ew:tag-address*)
  (message-id           ew-scan-unibyte-std11)
  (resent-message-id    ew-scan-unibyte-std11)
  (in-reply-to          ew-scan-unibyte-std11 . ew:tag-phrase-msg-id*)
  (references           ew-scan-unibyte-std11 . ew:tag-phrase-msg-id*)
  (keywords             ew-scan-unibyte-std11 . ew:tag-phrase*)
  (subject              ew-scan-unibyte-unstructured)
  (comments             ew-scan-unibyte-unstructured)
  (encrypted            ew-scan-unibyte-std11)
  (date                 ew-scan-unibyte-std11)
  (reply-to             ew-scan-unibyte-std11 . ew:tag-address+)
  (received             ew-scan-unibyte-std11)
  (resent-reply-to      ew-scan-unibyte-std11 . ew:tag-address+)
  (resent-from          ew-scan-unibyte-std11 . ew:tag-mailbox+)
  (resent-sender        ew-scan-unibyte-std11 . ew:tag-mailbox)
  (resent-date          ew-scan-unibyte-std11)
  (return-path          ew-scan-unibyte-std11)
  (mime-version         ew-scan-unibyte-std11)
  (content-type         ew-scan-unibyte-mime)
  (content-transfer-encoding    ew-scan-unibyte-mime)
  (content-id           ew-scan-unibyte-std11)
  (content-description  ew-scan-unibyte-unstructured)
  (content-disposition  ew-scan-unibyte-mime)
  (approved             ew-scan-unibyte-std11 . ew:tag-address+)
  (newsgroups           ew-scan-unibyte-none)
  (path                 ew-scan-unibyte-none)
  (lines                ew-scan-unibyte-none)
  (xref                 ew-scan-unibyte-none)
  (followup-to          ew-scan-unibyte-none)
))

(defvar ew-decode-field-default-syntax '(ew-scan-unibyte-unstructured))

(defvar ew-parse-error-sit-for-seconds 0)

;;; constants.

(defconst ew-token-regexp "[-!#-'*+0-9A-Z^-~]+")
(defconst ew-encoded-text-regexp
  (if ew-permit-null-encoded-text
      "[!->@-~]*"
    "[!->@-~]+"))

(defconst ew-encoded-word-regexp
  (concat (regexp-quote "=?")
          "\\(" ew-token-regexp "\\)"
          (regexp-quote "?")
          "\\(" ew-token-regexp "\\)"
          (regexp-quote "?")
          "\\(" ew-encoded-text-regexp "\\)"
          (regexp-quote "?=")))

(defconst ew-anchored-encoded-word-regexp
  (concat "\\`" ew-encoded-word-regexp "\\'"))

(defconst ew-b-regexp
  (eval-when-compile
    (concat "\\`\\("
            "[A-Za-z0-9+/]"
            "[A-Za-z0-9+/]"
            "[A-Za-z0-9+/]"
            "[A-Za-z0-9+/]"
            "\\)*"
            "\\("
            "[A-Za-z0-9+/]"
            "[A-Za-z0-9+/]"
            "\\(==\\|[A-Za-z0-9+/]=\\)"
            "\\)?"
            "\\'")))

(defconst ew-q-regexp
  "\\`\\([^=?]\\|=[0-9A-Fa-f][0-9A-Fa-f]\\)*\\'")

(defconst ew-quoting-char ?+)
(defconst ew-quoting-chars-regexp
  (concat (regexp-quote (char-to-string ew-quoting-char)) "*"))

(defconst ew-type2-regexp
  (concat (regexp-quote "=?")
          "\\(" ew-token-regexp "\\)"
          (regexp-quote "?")
          "\\(" ew-token-regexp "\\)"
          (regexp-quote "?")
          "\\(" ew-encoded-text-regexp "\\)"
          (regexp-quote "?")
          "\\'"))

(defconst ew-byte-decoder-alist
  '(("B" . ew-decode-b)
    ("Q" . ew-decode-q)))

(defconst ew-byte-checker-alist
  '(("B" . ew-b-check)
    ("Q" . ew-q-check)))

;;; utilities for variables.

(defun ew-dynamic-options ()
  (cons
   ew-default-mime-charset
   (logior
    (if ew-decode-sticked-encoded-word 1 0)
    (if ew-decode-quoted-encoded-word 2 0)
    (if ew-ignore-75bytes-limit 4 0)
    (if ew-ignore-76bytes-limit 8 0)
    (if ew-permit-sticked-comment 16 0)
    (if ew-permit-sticked-special 32 0)
    (if ew-remove-bare-crlf 64 0)
    (if ew-permit-null-encoded-text 128 0)
    )))