(provide 'ew-var)

;;; user customizable variables.

(defvar ew-decode-sticked-encoded-word nil)
(defvar ew-decode-quoted-encoded-word nil)
(defvar ew-ignore-75bytes-limit nil)
(defvar ew-ignore-76bytes-limit nil)
(defvar ew-permit-sticked-comment nil)
(defvar ew-permit-sticked-special nil)
(defvar ew-permit-null-encoded-text nil)

(defvar ew-remove-bare-crlf nil)
(defvar ew-default-mime-charset 'x-ctext)

;;;
(defvar ew-decode-field-syntax-alist
  '(
;;; std11 (rfc822, rfc1123)
    (from                 ew-scan-unibyte-std11 . ew:tag-mailbox+)
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
;;; rfc1049
    ;; (content-type         ew-scan-unibyte-std11)
;;; rfc2045
    (mime-version         ew-scan-unibyte-std11)
    (content-type         ew-scan-unibyte-mime)
    (content-transfer-encoding    ew-scan-unibyte-mime)
    (content-id           ew-scan-unibyte-std11)
    (content-description  ew-scan-unibyte-unstructured)
;;; rfc2183
    (content-disposition  ew-scan-unibyte-mime)
;;; rfc1864
    (content-md5          ew-scan-unibyte-none)
;;; rfc2076
    (status               ew-scan-unibyte-none)
;;; draft-ietf-drums-msg-fmt-05
    ;; (date                 ew-scan-unibyte-std11)
    ;; (from                 ew-scan-unibyte-std11 . ew:tag-mailbox+)
    ;; (sender               ew-scan-unibyte-std11 . ew:tag-mailbox)
    ;; (reply-to             ew-scan-unibyte-std11 . ew:tag-address+)
    ;; (to                   ew-scan-unibyte-std11 . ew:tag-address+)
    ;; (cc                   ew-scan-unibyte-std11 . ew:tag-address+)
    ;; (bcc                  ew-scan-unibyte-std11 . ew:tag-address*)
    ;; (message-id           ew-scan-unibyte-std11)
    ;; (in-reply-to          ew-scan-unibyte-std11)
    ;; (references           ew-scan-unibyte-std11)
    ;; (subject              ew-scan-unibyte-unstructured)
    ;; (comments             ew-scan-unibyte-unstructured)
    ;; (keywords             ew-scan-unibyte-std11 . ew:tag-phrase*)
    ;; (resent-date          ew-scan-unibyte-std11)
    ;; (resent-from          ew-scan-unibyte-std11 . ew:tag-mailbox+)
    ;; (resent-sender        ew-scan-unibyte-std11 . ew:tag-mailbox)
    ;; (resent-to            ew-scan-unibyte-std11 . ew:tag-address+)
    ;; (resent-cc            ew-scan-unibyte-std11 . ew:tag-address+)
    ;; (resent-bcc           ew-scan-unibyte-std11 . ew:tag-address*)
    ;; (resent-message-id    ew-scan-unibyte-std11)
    ;; (return-path          ew-scan-unibyte-std11)
    ;; (received             ew-scan-unibyte-std11)
;;; draft-ietf-drums-mail-followup-to-00
    (mail-followup-to     ew-scan-unibyte-std11 . ew:tag-mailbox+)
;;; draft-ietf-usefor-article-01
    ;; (date                 ew-scan-unibyte-std11)
    ;; (from                 ew-scan-unibyte-std11 . ew:tag-mailbox+)
    ;; (message-id           ew-scan-unibyte-std11)
    ;; (subject              ew-scan-unibyte-unstructured)
    (newsgroups           ew-scan-unibyte-none)
    (path                 ew-scan-unibyte-none)
    (followup-to          ew-scan-unibyte-none)
    (expires              ew-scan-unibyte-std11)
    ;; (reply-to             ew-scan-unibyte-std11 . ew:tag-address+)
    ;; (references           ew-scan-unibyte-std11 . ew:tag-phrase-msg-id*)
    (control              ew-scan-unibyte-none)
    (distribution         ew-scan-unibyte-none)
    ;; (keywords             ew-scan-unibyte-std11 . ew:tag-phrase*)
    (summary              ew-scan-unibyte-unstructured)
    (approved             ew-scan-unibyte-std11 . ew:tag-mailbox+)
    (lines                ew-scan-unibyte-none)
    (xref                 ew-scan-unibyte-none)
    (organization         ew-scan-unibyte-unstructured)
    (user-agent           ew-scan-unibyte-mime)
    (supersedes           ew-scan-unibyte-std11)
    (replaces             ew-scan-unibyte-std11)
    (replaced-by          ew-scan-unibyte-std11)
    (archive              ew-scan-unibyte-none)
;;; draft-ietf-usefor-posted-mailed-01
    (posted-and-mailed    ew-scan-unibyte-none)
    (followup-host        ew-scan-unibyte-none) ; news-url
;;; draft-ietf-mailext-new-fields-13
    (auto-submitted       ew-scan-unibyte-mime)
    ;; (supersedes           ew-scan-unibyte-std11)
    ;; (expires              ew-scan-unibyte-std11)
;;; others
    (x-face               ew-scan-unibyte-none)
    (x-pgp-sig            ew-scan-unibyte-none)
    ))

(defvar ew-decode-field-default-syntax '(ew-scan-unibyte-unstructured))

(defvar ew-parse-error-sit-for-seconds 0)

;;; constants.

(defconst ew-token-regexp "[-!#-'*+0-9A-Z^-~]+")
(defconst ew-encoded-text-regexp "[!->@-~]*")

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

(defconst ew-option-list
  '(ew-decode-sticked-encoded-word
    ew-decode-quoted-encoded-word
    ew-ignore-75bytes-limit
    ew-ignore-76bytes-limit
    ew-permit-sticked-comment
    ew-permit-sticked-special
    ew-permit-null-encoded-text))

(defun ew-save-boolean-options ()
  (let ((tmp 1) (opts ew-option-list) (val 0))
    (while opts
      (when (symbol-value (car opts)) (setq val (logior val tmp)))
      (setq tmp (lsh tmp 1)
	    opts (cdr opts)))
    val))

(defun ew-restore-boolean-options (val)
  (let ((tmp 1) (opts ew-option-list))
    (while opts
      (set (car opts) (not (zerop (logand val tmp))))
      (setq tmp (lsh tmp 1)
	    opts (cdr opts)))))

(defun ew-dynamic-options ()
  (cons
   ew-default-mime-charset
   (ew-save-boolean-options)))

