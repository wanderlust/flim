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

(defun ew-mime-update-field-decoder-cache (field mode)
  (let ((fun (cond
              ((eq mode 'plain)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (let ((res (ew-crlf-to-lf
                               (ew-decode-field field-name field-body))))
                     (add-text-properties
                      0 (length res)
                      (list 'original-field-name field-name
                            'original-field-body field-body)
                      res)
                     res))))
              ((eq mode 'wide)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (let ((res (ew-crlf-to-lf
                               (ew-crlf-refold
                                (ew-decode-field field-name field-body)
                                (length field-name)
                                (or max-column fill-column)))))
                     (add-text-properties
                      0 (length res)
                      (list 'original-field-name field-name
                            'original-field-body field-body)
                      res)
                     res))))
              ((eq mode 'summary)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (let ((res (ew-crlf-to-lf
                               (ew-crlf-unfold
                                (ew-decode-field field-name field-body)))))
                     (add-text-properties
                      0 (length res)
                      (list 'original-field-name field-name
                            'original-field-body field-body)
                      res)
                     res))))
              ((eq mode 'nov)
               (lexical-let ((field-name (symbol-name field)))
                 (lambda (field-body &optional start-column max-column must-unfold)
                   (setq field-body (ew-lf-to-crlf field-body))
                   (require 'ew-var)
                   (let ((ew-ignore-76bytes-limit t))
                     (let ((res (ew-crlf-to-lf
                                 (ew-crlf-unfold
                                  (ew-decode-field field-name field-body)))))
                       (add-text-properties
                        0 (length res)
                        (list 'original-field-name field-name
                              'original-field-body field-body)
                        res)
                       res)))))
              (t
               nil))))
    (mime-update-field-decoder-cache field mode fun)))

(setq mime-update-field-decoder-cache 'ew-mime-update-field-decoder-cache)
