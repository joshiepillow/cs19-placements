;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname p4-code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; sender-username :: String
;; sender-domain :: String
(define-struct email [sender-username sender-domain])

;; domain :: String
;; emails :: List-of-email
(define-struct email-server [domain emails])

;; excluded-domain :: String
;; emails :: List-of-email
(define-struct external-report [excluded-domain emails])

; external-senders :: List-of-email-server -> List-of-external-report
; given a list of emails from servers, creates a report containing the emails from each server with an external address
(define (external-senders server-list)
  (map (lambda (server) ; apply to each email server
         (make-external-report (email-server-domain server)
                               (filter (lambda (email) ; filter for external addresses
                                         (not (equal? (email-server-domain server)
                                                      (email-sender-domain email))))
                                       (email-server-emails server))))
       server-list))

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
(check-expect (external-senders (list test-1 test-2 test-3 test-4)) (list out-1 out-2 out-3 out-4))

; unique :: List-of-any -> List-of-any
; given a list, returns the list without duplicate entries, keeping the first occurance of any element
(define (unique any-list)
  (foldl cons '() ; reverses order
         (foldl (lambda (next acc)
                  (if (member? next acc) ; don't add element if already in accumulated list
                      acc
                      (cons next acc)))
                '() any-list)))

(check-expect (unique (list 1 2 3 4 5 4 3 2 1 "3")) (list 1 2 3 4 5 "3"))
(check-expect (unique '()) '())
(check-expect (unique (list 1 2 3 4 5 "3")) (list 1 2 3 4 5 "3"))
(check-expect (unique (list #true #true #true #true #true #true)) (list #true))
(check-expect (unique (list 1 2 3 4 5 4 3 2 1 1 1 2 2 2 3 3 3 6 0 6 7 8 0))
              (list 1 2 3 4 5 6 0 7 8))

;; letter :: Char
;; authors :: List-of-string
(define-struct shelf [letter authors])
  
; fix-shelves :: List-of-shelf -> List-of-shelf
; given a list of shelves, determine which shelves contain a book whose author does not belong on the shelf
(define (fix-shelves shelf-list)
  (filter (lambda (a-shelf) (not (empty? (shelf-authors a-shelf)))) ; remove shelves with no incorrect authors
          (map (lambda (a-shelf) ; apply to each shelf
                 (let ([letter (shelf-letter a-shelf)])
                   (make-shelf letter
                               (filter (lambda (author) ; remove correctly placed authors
                                         (not (equal? letter (first (string->list author)))))
                                       (shelf-authors a-shelf)))))
               shelf-list)))

(define authors-1 (list "hilbert" "heisenberg" "h" "hello"))
(define authors-2 '())
(define authors-3 (list "bad" "generally" "good" "great"))
(define authors-4 (list "generally" "good" "bad" "great"))

(check-expect (fix-shelves '()) '())
(check-expect (fix-shelves (list (make-shelf #\h authors-1))) '())
(check-expect (fix-shelves (list (make-shelf #\t authors-1)))
              (list (make-shelf #\t authors-1)))
(check-expect (fix-shelves (list
                            (make-shelf #\h authors-1)
                            (make-shelf #\t authors-1)
                            (make-shelf #\h authors-2)
                            (make-shelf #\t authors-2)
                            (make-shelf #\b authors-3)
                            (make-shelf #\g authors-3)
                            (make-shelf #\h authors-4)
                            (make-shelf #\g authors-4)))
              (list (make-shelf #\t authors-1)
                    (make-shelf #\b (list "generally" "good" "great"))
                    (make-shelf #\g (list "bad"))
                    (make-shelf #\h authors-4)
                    (make-shelf #\g (list "bad"))))
(check-expect (fix-shelves (list (make-shelf #\g authors-4)
                                 (make-shelf #\g authors-4)
                                 (make-shelf #\g authors-4)))
              (list (make-shelf #\g (list "bad"))
                    (make-shelf #\g (list "bad"))
                    (make-shelf #\g (list "bad"))))

; elim-contains-char :: Char List-of-strings -> List-of-strings
; given a list of strings, returns the list only containing the strings without the given character
(define (elim-contains-char a-char string-list)
  (filter (lambda (a-string) (not (member a-char (string->list a-string)))) string-list))

(check-expect (elim-contains-char #\c '()) '())
(check-expect (elim-contains-char #\c (list "c")) '())
(check-expect (elim-contains-char #\c (list "cat" "abc" "a" "C" "aaabbbbbdddd" "dacb" ""))
              (list "a" "C" "aaabbbbbdddd" ""))
(check-expect (elim-contains-char #\& (list "f&f" "asdfghjkl&" "7&&&&&&7" "&")) '())
(check-expect (elim-contains-char #\0 (list "go" "nothing" "123456789" "" " : "))
              (list "go" "nothing" "123456789" "" " : "))

; l33t :: List-of-strings -> List-of-strings
; given a list of strings, returns the list with certain characters in each string replaced
(define (l33t string-list)
  (map (lambda (a-string) ; apply to each string
         (list->string (map (lambda (a-char) ; apply to each character
                              (cond [(or (equal? a-char #\A) (equal? a-char #\a)) #\4]
                                    [(or (equal? a-char #\E) (equal? a-char #\e)) #\3]
                                    [(or (equal? a-char #\I) (equal? a-char #\i)) #\1]
                                    [(or (equal? a-char #\O) (equal? a-char #\o)) #\0]
                                    [else a-char]))
                            (string->list a-string))))
       string-list))

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

; is-palindrome :: String -> Boolean
; determine if a string is a palidrome after removing all non-alphanumeric characters
(define (is-palindrome a-string)
  (let* ([char-list (string->list a-string)]
         [filtered-list (filter (lambda (a-char) ; remove all non-alphanumeric characters
                                  (or (char-alphabetic? a-char)
                                      (char-numeric? a-char)))
                                char-list)]
         [upper-list (map (lambda (a-char) ; ignore case
                            (char-upcase a-char)) filtered-list)]) 
    (foldl (lambda (next next-reversed acc) ; compare each character in list and reversed list
             (and acc (equal? next next-reversed))) 
           #true upper-list (foldl cons '() upper-list))))

(check-expect (is-palindrome "") #true)
(check-expect (is-palindrome "A") #true)
(check-expect (is-palindrome "AB") #false)
(check-expect (is-palindrome "!!!!!##^&^&*^&*^   ^&#$#@((*&^") #true)
(check-expect (is-palindrome "racecar") #true)
(check-expect (is-palindrome "Ra C##E!!c  AR     )") #true)
(check-expect (is-palindrome "otherwise") #false)
(check-expect (is-palindrome "almost the same ems eht tsomla") #false)
(check-expect (is-palindrome "lolol67890") #false)
(check-expect (is-palindrome "123l456") #false)
(check-expect (is-palindrome "!!!!!##^&^&*^&*^aff   a^&#$d#@((*&^") #false)
(check-expect (is-palindrome "&^treeert^&") #true)
(check-expect (is-palindrome "ARERA") #true)
(check-expect (is-palindrome "cannottotototonnac") #false)

;; species :: String
;; length-in-inches :: Number
(define-struct fish (species length-in-inches))

; viable-fish :: List-of-fish, List-of-string -> List-of-fish
; given a list of fish and a list of invasive species, create a list with the non-invasive fish longer than 8 inches
(define (viable-fish fish-list invasive-list)
  (filter (lambda (a-fish) ; apply to each fish
            (and (not (member (fish-species a-fish) invasive-list))
                 (> (fish-length-in-inches a-fish) 8))) fish-list))

(define fish-1 (make-fish "Bass" 10))
(define fish-2 (make-fish "Bass" 8))
(define fish-3 (make-fish "Bass" 6))
(define fish-4 (make-fish "Sunfish" 10000))
(define fish-5 (make-fish "Sunfish" 1000))
(define fish-6 (make-fish "Sunfish" 100))
(define fish-7 (make-fish "25519 {} !@#$%^&*()" 8.01))
(define fish-8 (make-fish "25519 {} !@#$%^&*()" -9))

(check-expect (viable-fish '() '()) '())
(check-expect (viable-fish '() (list "Bass" "Sunfish" "25519 {} !@#$%^&*()" "Word")) '())
(check-expect (viable-fish (list fish-1 fish-2 fish-3 fish-4 fish-5 fish-6 fish-7 fish-8) '())
              (list fish-1 fish-4 fish-5 fish-6 fish-7))
(check-expect (viable-fish (list fish-1 fish-8 fish-2 fish-7 fish-3 fish-6 fish-4 fish-5)
                           (list "Bass"))
              (list fish-7 fish-6 fish-4 fish-5))
(check-expect (viable-fish (list fish-1 fish-8 fish-2 fish-7 fish-3 fish-6 fish-4 fish-5)
                           (list "25519 {} !@#$%^&*()" "Sunfish"))
              (list fish-1))
(check-expect (viable-fish (list fish-1 fish-8 fish-2 fish-7 fish-3 fish-6 fish-4 fish-5)
                           (list "25519 {} !@#$%^&*()" "Bass" "Sunfish"))
              '())
(check-expect (viable-fish (list fish-1 fish-1 fish-1 fish-2 fish-1 fish-4 fish-1 fish-7)
                           (list "25519 {} !@#$%^&*()"))
              (list fish-1 fish-1 fish-1 fish-1 fish-4 fish-1))