(require 'ew-data)
(require 'ew-parse)
(provide 'ew-scan-n)

(defun ew-scan-none (col str)
  (let ((res (ew-make-anchor col str)))
    (ew-add-frag res 0 (length str) 'ew:n-body)
    (ew-terminate res)
    res))

(defalias 'ew-scan-unibyte-none 'ew-scan-none)
(defalias 'ew-scan-multibyte-none 'ew-scan-none)

