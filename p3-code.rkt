;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname p3-code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; check-valid? :: List-of-chars List-of-chars -> Boolean
; checks if every character in a list is contained in a list of valid characters
(define (check-valid? char-list valid-list)
  (cond
    [(empty? char-list) #true]
    [(cons? char-list) 
     (and (member (first char-list) valid-list)
          (check-valid? (rest char-list) valid-list))]))

(check-expect (check-valid? '() (list #\a #\b #\c)) #true)
(check-expect (check-valid? (list #\a #\b #\c) '()) #false)
(check-expect (check-valid? '() '()) #true)
(check-expect (check-valid? (list #\a #\a #\a #\b) '()) #false)
(check-expect (check-valid? (list #\a #\a #\a #\b) (list #\a #\b #\c)) #true)
(check-expect (check-valid? (list #\a #\a #\a #\b) (list #\b)) #false)

; valid-words :: List-of-strings List-of-chars -> List-of-strings
; filters a list of words, keeping a word if it only contains valid characters
(define (valid-words string-list valid-list)
  (cond
    [(empty? string-list) '()]
    [(cons? string-list)
     (if (check-valid? (string->list (first string-list)) valid-list)
         (cons (first string-list) (valid-words (rest string-list) valid-list))
         (valid-words (rest string-list) valid-list))]))

(check-expect (valid-words '() (list #\a #\b #\c)) '())
(check-expect (valid-words (list "abc") '()) '())
(check-expect (valid-words '() '()) '())
(check-expect (valid-words (list "abc" "ab" "a" "" "cd" "abcd" "dab" "0")
                           (list #\a #\b #\d))
              (list "ab" "a" "" "dab"))
(check-expect (valid-words (list "abc" "ab" "a" "cd" "abcd" "dab" "0")
                           (list #\x  #\y  #\z  #\1  #\2  #\3  #\4  #\5))
              '())
(check-expect (valid-words (list "abc" "ab" "a" "cd" "abcd" "dab" "00000000000000000000")
                           (list #\x  #\y  #\z  #\1  #\2  #\3  #\4  #\0))
              (list "00000000000000000000"))

;; sender-username :: String
;; sender-domain :: String
(define-struct email [sender-username sender-domain])

;; domain :: String
;; emails :: List-of-email
(define-struct email-server [domain emails])

;; excluded-domain :: String
;; emails :: List-of-email
(define-struct external-report [excluded-domain emails])

; filter-external :: Email-server -> Emails
; given a domain and a list of emails, identifies the emails that do not come from that domain
(define (filter-external domain email-list)
  (cond
    [(empty? email-list) '()]
    [(cons? email-list)
     (if (equal? domain (email-sender-domain (first email-list)))
         (filter-external domain (rest email-list))
         (cons (first email-list) (filter-external domain (rest email-list))))]))

(check-expect (filter-external "hi" (list (make-email "blah" "hi"))) '())
(check-expect (filter-external "hi" '()) '())
(check-expect (filter-external "hi" (list (make-email "blah" "hi")
                                          (make-email "blah" "other")
                                          (make-email "blah" "bye")
                                          (make-email "again" "hi")))
              (list (make-email "blah" "other") (make-email "blah" "bye")))
(check-expect (filter-external "" (list (make-email "blah" "hi")
                                        (make-email "blah" "other")
                                        (make-email "blah" "bye")
                                        (make-email "again" "hi")))
              (list (make-email "blah" "hi")
                    (make-email "blah" "other")
                    (make-email "blah" "bye")
                    (make-email "again" "hi")))

; external-senders :: List-of-email-server -> List-of-external-report
; given a list of emails from servers, creates a report containing the emails from each server with an external address
(define (external-senders server-list)
  (cond
    [(empty? server-list) '()]
    [(cons? server-list)
     (cons (make-external-report  (email-server-domain (first server-list))
                                  (filter-external (email-server-domain (first server-list))
                                                   (email-server-emails (first server-list))))
           (external-senders (rest server-list)))]))

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

; remove-every :: Any List-of-any -> List-of-any
; given a value and a list, returns the list without any occurances of the value
(define (remove-every any any-list)
  (cond
    [(empty? any-list) '()]
    [(cons? any-list)
     (if (equal? any (first any-list))
         (remove-every any (rest any-list))
         (cons (first any-list) (remove-every any (rest any-list))))]))

(check-expect (remove-every 3 (list 1 2 3 4 5 4 3 2 1 "3"))
              (list 1 2 4 5 4 2 1 "3"))
(check-expect (remove-every 3 '()) '())
(check-expect (remove-every "abc" (list 1 2 3 4 5 4 3 2 1 "3"))
              (list 1 2 3 4 5 4 3 2 1 "3"))
(check-expect (remove-every #true (list #true #true #true #true #true #true))
              '())


; unique :: List-of-any -> List-of-any
; given a list, returns the list without duplicate entries, keeping the first occurance of any element
(define (unique any-list)
  (cond
    [(empty? any-list) '()]
    [(cons? any-list)
     (cons (first any-list)
           (unique (remove-every (first any-list) (rest any-list))))]))

(check-expect (unique (list 1 2 3 4 5 4 3 2 1 "3")) (list 1 2 3 4 5 "3"))
(check-expect (unique '()) '())
(check-expect (unique (list 1 2 3 4 5 "3")) (list 1 2 3 4 5 "3"))
(check-expect (unique (list #true #true #true #true #true #true)) (list #true))
(check-expect (unique (list 1 2 3 4 5 4 3 2 1 1 1 2 2 2 3 3 3 6 0 6 7 8 0))
              (list 1 2 3 4 5 6 0 7 8))

;; name :: String
;; species :: String
;; weight-in-kg :: Number
(define-struct canada-penguin [name species weight-in-kg])


; healthy? :: Canada-penguin -> Boolean
; determines if a penguin is healthy based on its weight and species
(define (healthy? penguin)
  (or (and (equal? "Gentoo" (canada-penguin-species penguin))
           (<= (* #e2.2 (canada-penguin-weight-in-kg penguin)) 19)
           (>= (* #e2.2 (canada-penguin-weight-in-kg penguin)) #e9.9))
      (and (equal? "King" (canada-penguin-species penguin))
           (<= (* #e2.2 (canada-penguin-weight-in-kg penguin)) 40)
           (>= (* #e2.2 (canada-penguin-weight-in-kg penguin)) 21))))

(define penguin-1 (make-canada-penguin "Ada" "Gentoo" (/ 19 #e2.2)))
(define penguin-1-after (make-canada-penguin "Ada" "Gentoo" (/ 21 #e2.2)))
(define penguin-2 (make-canada-penguin "Bob" "Gentoo" (/ 20 #e2.2)))
(define penguin-3 (make-canada-penguin "Chip" "Gentoo" (/ #e9.5 #e2.2)))
(define penguin-4 (make-canada-penguin "Dana" "King" (/ 35 #e2.2)))
(define penguin-4-after (make-canada-penguin "Dana" "King" (/ 37 #e2.2)))
(define penguin-5 (make-canada-penguin "123456789" "King" (/ 15 #e2.2)))

(check-expect (healthy? penguin-1) #true)
(check-expect (healthy? penguin-1-after) #false)
(check-expect (healthy? penguin-2) #false)
(check-expect (healthy? penguin-3) #false)
(check-expect (healthy? penguin-4) #true)
(check-expect (healthy? penguin-4-after) #true)
(check-expect (healthy? penguin-5) #false)

; after-loan :: List-of-canada-penguin -> List-of-canada-penguin
; given a list of Canadian penguins, determines the outcomes of those that are selected to spend a month in the United States
(define (after-loan penguins)
  (cond
    [(empty? penguins) '()]
    [(cons? penguins)
     (if (healthy? (first penguins))
         (cons (make-canada-penguin
                (canada-penguin-name (first penguins))
                (canada-penguin-species (first penguins))
                (+ (canada-penguin-weight-in-kg (first penguins)) (/ 2 #e2.2)))
               (after-loan (rest penguins)))
         (after-loan (rest penguins)))]))

(check-expect (after-loan '()) '())
(check-expect (after-loan (list penguin-1)) (list penguin-1-after))
(check-expect (after-loan (list penguin-1-after)) '())
(check-expect (after-loan (list penguin-1 penguin-2 penguin-3 penguin-4 penguin-5))
              (list penguin-1-after penguin-4-after))
(check-expect (after-loan (list penguin-4 penguin-4 penguin-1 penguin-1 penguin-1-after))
              (list penguin-4-after penguin-4-after penguin-1-after penguin-1-after))

;; letter :: Char
;; authors :: List-of-String
(define-struct shelf [letter authors])

; misplaced-authors :: Char List-of-String -> Boolean
; given a character and list of authors, returns the authors whose names do not begin with the character 
(define (misplaced-authors letter authors)
  (cond
    [(empty? authors) '()]
    [(cons? authors)
     (if (equal? letter (first (string->list (first authors))))
         (misplaced-authors letter (rest authors))
         (cons (first authors) (misplaced-authors letter (rest authors))))]))

(define authors-1 (list "hilbert" "heisenberg" "h" "hello"))
(define authors-2 '())
(define authors-3 (list "bad" "generally" "good" "great"))
(define authors-4 (list "generally" "good" "bad" "great"))

(check-expect (misplaced-authors #\h authors-1) '())
(check-expect (misplaced-authors #\t authors-1) authors-1)
(check-expect (misplaced-authors #\h authors-2) '())
(check-expect (misplaced-authors #\t authors-2) '())
(check-expect (misplaced-authors #\b authors-3) (list "generally" "good" "great"))
(check-expect (misplaced-authors #\g authors-3) (list "bad"))
(check-expect (misplaced-authors #\h authors-4) authors-4)
(check-expect (misplaced-authors #\g authors-4) (list "bad"))

; fix-shelves :: List-of-Shelf -> List-of-Shelf
; given a list of shelves, determine which shelves contain a book whose author does not belong on the shelf
(define (fix-shelves shelf-list)
  (cond
    [(empty? shelf-list) '()]
    [(cons? shelf-list)
     (cond
       [(empty? (misplaced-authors (shelf-letter (first shelf-list)) (shelf-authors (first shelf-list)))) (fix-shelves (rest shelf-list))]
       [else (cons (make-shelf (shelf-letter (first shelf-list))
                               (misplaced-authors (shelf-letter (first shelf-list)) (shelf-authors (first shelf-list))))
                   (fix-shelves (rest shelf-list)))])]))

(check-expect (fix-shelves '()) '())
(check-expect (fix-shelves (list (make-shelf #\h authors-1))) '())
(check-expect (fix-shelves (list (make-shelf #\t authors-1))) (list (make-shelf #\t authors-1)))
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
