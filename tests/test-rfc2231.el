(require 'lunit)
(require 'mime)

(luna-define-class test-rfc2231 (lunit-test-case))

;; The former is semantically identical to the latter.
(luna-define-method test-rfc2231-1 ((case test-rfc2231))
  (lunit-assert
   (equal
    (mime-parse-Content-Type "\
 message/external-body; access-type=URL;
 URL*0=\"ftp://\";
 URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\"")
    (mime-parse-Content-Type "\
 message/external-body; access-type=URL;
 URL=\"ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""))))

(luna-define-method test-rfc2231-2 ((case test-rfc2231))
  (lunit-assert
   (equal
    (mime-parse-Content-Type "\
 application/x-stuff;
 title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A")
    '((type . application) (subtype . x-stuff)
      ("title" . #("This is ***fun***" 0 17 (mime-language "en-us")))))))

(luna-define-method test-rfc2231-3 ((case test-rfc2231))
  (lunit-assert
   (equal
    (mime-parse-Content-Type "\
 application/x-stuff;
 title*0*=us-ascii'en'This%20is%20even%20more%20;
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*2=\"isn't it!\"")
    '((type . application) (subtype . x-stuff)
      ("title" . #("This is even more ***fun*** isn't it!" 0 37 (mime-language "en")))))))

(luna-define-method test-rfc2231-4 ((case test-rfc2231))
  (lunit-assert
   (equal
    (mime-parse-Content-Type "\
 application/x-stuff;
 title*0*=us-ascii'en'This%20is%20even%20more%20;
 title*1*=%2A%2A%2Afun%2A%2A%2A%20;
 title*2=\"isn't it!\"")
    (mime-parse-Content-Type "\
 application/x-stuff;
 title*=us-ascii'en'This%20is%20even%20more%20%2A%2A%2Afun%2A%2A%2A%20isn%27t%20it!\""))))
