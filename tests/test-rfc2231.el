(require 'lunit)
(require 'mime)

(luna-define-class test-rfc2231 (lunit-test-case))

;;; Parameter Value Continuations
;; The former is semantically identical to the latter.
(luna-define-method test-rfc2231-1 ((case test-rfc2231))
  (lunit-assert
   (eq
    (mime-content-type-primary-type
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""))
    (mime-content-type-primary-type
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")))))

(luna-define-method test-rfc2231-2 ((case test-rfc2231))
  (lunit-assert
   (eq
    (mime-content-type-subtype
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""))
    (mime-content-type-subtype
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")))))

(luna-define-method test-rfc2231-3 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "access-type")
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "access-type"))))

(luna-define-method test-rfc2231-4 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "url")
    (mime-content-type-parameter
     (mime-parse-Content-Type "message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
     "url"))))

;;; Parameter Value Character Set and Language Information
(luna-define-method test-rfc2231-5 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A")
     "title")
    "This is ***fun***")))

(luna-define-method test-rfc2231-6 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*0*=us-ascii'en'This%20is%20even%20more%20;
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*2=\"isn't it!\"")
     "title")
    "This is even more ***fun*** isn't it!")))

;;; MIME states that parameters are not order sensitive.
(luna-define-method test-rfc2231-7 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*2=\"isn't it!\";
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*0*=us-ascii'en'This%20is%20even%20more%20")
     "title")
    "This is even more ***fun*** isn't it!")))

;;; ABNF states that `ext-octet' is case-insensitive.
(luna-define-method test-rfc2231-8 ((case test-rfc2231))
  (lunit-assert
   (let ((case-fold-search nil))
     (string=
      (mime-content-type-parameter
       (mime-parse-Content-Type "application/x-stuff;
 title*=us-ascii'en-us'This%20is%20%2a%2a%2afun%2a%2a%2a")
       "title")
      "This is ***fun***"))))

;;; unencoded segments MUST NOT be decoded.
(luna-define-method test-rfc2231-9 ((case test-rfc2231))
  (lunit-assert
   (string=
    (mime-content-type-parameter
     (mime-parse-Content-Type "application/x-stuff;
 title*0*=us-ascii'en'This%20is%20even%20more%20;
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*2=\"isn%27t%20it!\"")
     "title")
    "This is even more ***fun*** isn%27t%20it!")))
