;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname p3-es) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define test-1 (make-email-server "example.com" (list
                                                 (make-email "one" "example.com")
                                                 (make-email "two" "example.com")
                                                 (make-email "person" "example.net")
                                                 (make-email "three" "example.com")
                                                 (make-email "another" "different"))))
(define out-1 (make-external-report "example.com" (list (make-email "person" "example.net")
                                                        (make-email "another" "different"))))
(define test-2 (make-email-server "empty" '()))
(define out-2 (make-external-report "empty" '()))
(define test-3 (make-email-server "internal"  (list
                                               (make-email "one" "internal")
                                               (make-email "123456789" "internal")
                                               (make-email "qwerty" "internal")
                                               (make-email "one" "internal"))))
(define out-3 (make-external-report "internal" '()))
(define test-4 (make-email-server "different" (list
                                               (make-email "one" "example.com")
                                               (make-email "asdfghjkl,.?" "internal")
                                               (make-email "one" "example.com")
                                               (make-email "person" "example.net"))))
(define out-4 (make-external-report "different" (list
                                                 (make-email "one" "example.com")
                                                 (make-email "asdfghjkl,.?" "internal")
                                                 (make-email "one" "example.com")
                                                 (make-email "person" "example.net"))))

(check-expect (external-senders '()) '())
(check-expect (external-senders (list test-1)) (list out-1))
(check-expect (external-senders (list test-2)) (list out-2))
(check-expect (external-senders (list test-3)) (list out-3))
(check-expect (external-senders (list test-4)) (list out-4))
(check-expect (external-senders (list test-1 test-2 test-3 test-4))
              (list out-1 out-2 out-3 out-4))