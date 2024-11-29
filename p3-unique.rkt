;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname p3-unique) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (unique (list 1 2 3 4 5 4 3 2 1 "3")) (list 1 2 3 4 5 "3"))
(check-expect (unique '()) '())
(check-expect (unique (list 1 2 3 4 5 "3")) (list 1 2 3 4 5 "3"))
(check-expect (unique (list #true #true #true #true #true #true)) (list #true))
(check-expect (unique (list 1 2 3 4 5 4 3 2 1 1 1 2 2 2 3 3 3 6 0 6 7 8 0))
              (list 1 2 3 4 5 6 0 7 8))