;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname p4-ecc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (elim-contains-char #\c '()) '())
(check-expect (elim-contains-char #\c (list "c")) '())
(check-expect (elim-contains-char #\c (list "cat" "abc" "a" "C" "aaabbbbbdddd" "dacb" ""))
              (list "a" "C" "aaabbbbbdddd" ""))
(check-expect (elim-contains-char #\& (list "f&f" "asdfghjkl&" "7&&&&&&7" "&")) '())
(check-expect (elim-contains-char #\0 (list "go" "nothing" "123456789" "" " : "))
              (list "go" "nothing" "123456789" "" " : "))