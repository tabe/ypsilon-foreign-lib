#!/usr/bin/env ypsilon
#!r6rs

(import (core)
        (rnrs)
        (prefix (base64) base64:)
        (ypsilon ffi)
        (ypsilon gcrypt))

;; cf. http://gist.github.com/112188

(define *key* "1234567890")
(define *string*
  (format (string-append
           "GET~%webservices.amazon.com~%/onca/xml~%AWSAccessKeyId=~a&"
           "ItemId=~a&"
           "Operation=~a&"
           "ResponseGroup=~a&"
           "Service=~a&"
           "Timestamp=~a&"
           "Version=~a")
          "00000000000000000000"
          "0679722769"
          "ItemLookup"
          "ItemAttributes%2COffers%2CImages%2CReviews"
          "AWSECommerceService"
          "2009-01-01T12%3A00%3A00Z"
          "2009-01-06"))
(define *data* (string->utf8 *string*))
(define *expected* "Nace+U3Az4OhN7tISqgs1vdLBHBEijWcBeCqL5xN9xg=")

(let* ((hdp (make-bytevector sizeof:void*)))
  (gcry_md_open hdp GCRY_MD_SHA256 GCRY_MD_FLAG_HMAC)
  (let ((hd (make-bytevector-mapping (c-void*-ref hdp) sizeof:void*)))
    (gcry_md_setkey hd (make-c-string *key*) (string-length *key*))
    (gcry_md_write hd *data* (bytevector-length *data*))
    (let* ((result (gcry_md_read hd 0))
           (r (make-bytevector-mapping result 32)))
      (dynamic-wind
          (lambda () #f)
          (lambda ()
            (let ((actual (base64:encode-bytevector r)))
              (cond ((string=? *expected* actual)
                     (display "passed.\n"))
                    (else
                     (format (current-error-port)
                             "~s expected, but got ~s~%failed.~%"
                             *expected*
                             actual)))))
          (lambda () (gcry_md_close hd))))))
