(require 'emu)
(require 'mel)

(defvar ew-bq-use-mel (not (fboundp 'make-ccl-coding-system)))

(cond
 (ew-bq-use-mel
  (defalias 'ew-decode-q 'q-encoding-decode-string)
  (defalias 'ew-decode-b 'base64-decode-string))
 (t
  (require 'ew-ccl)
  (defun ew-decode-q (str)
    (string-as-unibyte (decode-coding-string str 'ew-ccl-uq)))
  (if base64-dl-module
      (defalias 'ew-decode-b 'base64-decode-string)
    (defun ew-decode-b (str)
      (string-as-unibyte (decode-coding-string str 'ew-ccl-b))))))

(provide 'ew-bq)

;;;
(defvar ew-bq-use-mel nil)

(defun ew-encode-uq (str)
  (encode-coding-string (string-as-unibyte str) 'ew-ccl-uq))
