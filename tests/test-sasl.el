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

(luna-define-method test-sasl-digest-md5-build-response-value ((case test-sasl))
  (require 'sasl-digest)
  (lunit-assert
   (string=
    (sasl-digest-md5-build-response-value
     "chris" "elwood.innosoft.com" "secret" "OA9BSXrbuRhWay" "OA9BSuZWMSpW8m" 1
     (sasl-digest-md5-digest-uri "acap" "elwood.innosoft.com"))
    "username=\"chris\",realm=\"elwood.innosoft.com\",nonce=\"OA9BSXrbuRhWay\",nc=00000001,cnonce=\"OA9BSuZWMSpW8m\",digest-uri=\"acap/elwood.innosoft.com\",response=6084c6db3fede7352c551284490fd0fc,")))
