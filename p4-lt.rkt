;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname p4-lt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (l33t '()) '())
(check-expect (l33t (list "hello")) (list "h3ll0"))
(check-expect (l33t (list "aeiouAEIOU AEIOUaeiou"))
              (list "4310u4310U 4310U4310u"))
(check-expect (l33t (list "cat" "abc" "a" "CEO" "aaabbbbbdddde" "dacbO" ""))
              (list "c4t" "4bc" "4" "C30" "444bbbbbdddd3" "d4cb0" ""))
(check-expect (l33t (list "much" "lurch" "123456789" "" " : "
                          "bcdfghjklmnpqrstuvwxyzBCDFGHJKLMNPQRSTUVWXYZ"))
              (list "much" "lurch" "123456789" "" " : "
                    "bcdfghjklmnpqrstuvwxyzBCDFGHJKLMNPQRSTUVWXYZ"))