(require 'lunit)
(require 'sasl)

(luna-define-class test-sasl (lunit-test-case))

(luna-define-method test-sasl-find-mechanism ((case test-sasl))
  (let ((mechanisms sasl-mechanisms))
    (while mechanisms
      (let* ((sasl-mechanisms (list (car mechanisms))))
	(lunit-assert
	 (sasl-find-mechanism (list (car mechanisms)))))
      (setq mechanisms (cdr mechanisms)))))

(luna-define-method test-sasl-digest-md5-imap ((case test-sasl))
  (let* ((sasl-mechanisms '("DIGEST-MD5"))
	 (mechanism
	  (sasl-find-mechanism '("DIGEST-MD5")))
	 (client
	  (sasl-make-client mechanism "chris" "imap" "elwood.innosoft.com"))
	 (sasl-read-passphrase
	  (function
	   (lambda (prompt)
	     "secret")))
	 step
	 response)
    (sasl-client-set-property client 'realm "elwood.innosoft.com")
    (sasl-client-set-property client 'cnonce "OA6MHXh6VqTrRk")
    (setq step (sasl-next-step client nil))
    (sasl-step-set-data
     step "realm=\"elwood.innosoft.com\",nonce=\"OA6MG9tEQGm2hh\",\
qop=\"auth\",algorithm=md5-sess,charset=utf-8")
    (setq step (sasl-next-step client step))
    (sasl-step-data step)
    (setq response (sasl-digest-md5-parse-string (sasl-step-data step)))
    (lunit-assert
     (string=
      (plist-get response 'response) "d388dad90d4bbd760a152321f2143af7"))))

(luna-define-method test-sasl-digest-md5-acap ((case test-sasl))
  (let* ((sasl-mechanisms '("DIGEST-MD5"))
	 (mechanism
	  (sasl-find-mechanism '("DIGEST-MD5")))
	 (client
	  (sasl-make-client mechanism "chris" "acap" "elwood.innosoft.com"))
	 (sasl-read-passphrase
	  (function
	   (lambda (prompt)
	     "secret")))
	 step
	 response)
    (sasl-client-set-property client 'realm "elwood.innosoft.com")
    (sasl-client-set-property client 'cnonce "OA9BSuZWMSpW8m")
    (setq step (sasl-next-step client nil))
    (sasl-step-set-data
     step "realm=\"elwood.innosoft.com\",nonce=\"OA9BSXrbuRhWay\",qop=\"auth\",\
algorithm=md5-sess,charset=utf-8")
    (setq step (sasl-next-step client step))
    (sasl-step-data step)
    (setq response (sasl-digest-md5-parse-string (sasl-step-data step)))
    (lunit-assert
     (string=
      (plist-get response 'response) "6084c6db3fede7352c551284490fd0fc"))))
