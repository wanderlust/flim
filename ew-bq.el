(require 'emu)
(require 'mel)

(defun ew-decode-q (string)
  (if (equal string "")
    ""
    (encoded-text-decode-string string "Q")))

(defun ew-decode-b (string)
  (if (equal string "")
    ""
    (encoded-text-decode-string string "B")))

(provide 'ew-bq)
