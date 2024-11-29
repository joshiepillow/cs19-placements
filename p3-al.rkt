;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname p3-al) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define penguin-1 (make-canada-penguin "Ada" "Gentoo" (/ 19 #e2.2)))
(define penguin-1-after (make-canada-penguin "Ada" "Gentoo" (/ 21 #e2.2)))
(define penguin-2 (make-canada-penguin "Bob" "Gentoo" (/ 20 #e2.2)))
(define penguin-3 (make-canada-penguin "Chip" "Gentoo" (/ #e9.5 #e2.2)))
(define penguin-4 (make-canada-penguin "Dana" "King" (/ 35 #e2.2)))
(define penguin-4-after (make-canada-penguin "Dana" "King" (/ 37 #e2.2)))
(define penguin-5 (make-canada-penguin "123456789" "King" (/ 15 #e2.2)))

(check-expect (after-loan '()) '())
(check-expect (after-loan (list penguin-1)) (list penguin-1-after))
(check-expect (after-loan (list penguin-1-after)) '())
(check-expect (after-loan (list penguin-1 penguin-2 penguin-3 penguin-4 penguin-5))
              (list penguin-1-after penguin-4-after))
(check-expect (after-loan (list penguin-4 penguin-4 penguin-1 penguin-1 penguin-1-after))
              (list penguin-4-after penguin-4-after penguin-1-after penguin-1-after))