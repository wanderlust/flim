(require 'ew-dec)
(require 'eword-decode)

(require 'ew-line)
(eval-when-compile (require 'cl))

(defun ew-gnus-structured-field-decoder (string)
  (if (fboundp 'ew-decode-field)
      (let ((ew-ignore-76bytes-limit t)
	    (ew-default-mime-charset default-mime-charset))
	(condition-case nil
	    (ew-cut-cr-lf (ew-decode-field "From" (ew-lf-crlf-to-crlf string)))
	  (error
	   (message "gnus-structured-field-decoder error: %s" string)
	   (decode-mime-charset-string string 'x-ctext))))
    (eword-decode-and-unfold-structured-field-body string)))

(defun ew-gnus-unstructured-field-decoder (string)
  (if (fboundp 'ew-decode-field)
      (let ((ew-ignore-76bytes-limit t)
	    (ew-default-mime-charset default-mime-charset))
	(condition-case nil
	    (ew-cut-cr-lf (ew-decode-field "Subject" (ew-lf-crlf-to-crlf string)))
	  (error
	   (message "gnus-unstructured-field-decoder error: %s" string)
	   (decode-mime-charset-string string 'x-ctext))))
    (eword-decode-unstructured-field-body (std11-unfold-string string) 'must-unfold)))
