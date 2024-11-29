;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname p2-code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; score-by-length :: List-of-strings -> Number
; adds together the lengths of all the strings in a list
(define (score-by-length string-list)
  (cond
    [(empty? string-list) 0]
    [(cons? string-list) (+ (string-length (first string-list))
                            (score-by-length (rest string-list)))]))

(check-expect (score-by-length '()) 0)
(check-expect (score-by-length (list "")) 0)
(check-expect (score-by-length (list "hello")) 5)
(check-expect (score-by-length (list "hello" "world")) 10)
(check-expect (score-by-length (list "" "1234567890!@#$%^&*()" " a ")) 23)

; overlay-all :: List-of-images -> Image
; overlays a list of images, placing images that appear first in the list on top of images later in the list
(define (overlay-all image-list)
  (cond
    [(empty? image-list) (rectangle 10 10 "solid" "white")]
    [(cons? image-list) (overlay (first image-list)
                                 (overlay-all (rest image-list)))]))

(check-expect (overlay-all '())
              (rectangle 10 10 "solid" "white"))
(check-expect (overlay-all (list empty-image))
              (rectangle 10 10 "solid" "white"))
(check-expect (overlay-all (list (star 5 "solid" "yellow")))
              (overlay (star 5 "solid" "yellow") (rectangle 10 10 "solid" "white")))
(check-expect (overlay-all (list (rectangle 2 2 "solid" "green") empty-image (circle 7 "outline" "blue")))
              (overlay (rectangle 2 2 "solid" "green") (circle 7 "outline" "blue") (rectangle 10 10 "solid" "white")))
(check-expect (overlay-all (list (rectangle 10 10 "solid" "white") (triangle 3 "solid" "black")))
              (rectangle 10 10 "solid" "white"))

; bar-graph :: List-of-numbers -> Image
; create a bar graph with bar heights corresponding to the values in the list
(define (bar-graph height-list)
  (cond
    [(empty? height-list) (rectangle 1 1 "solid" "white")]
    [(cons? height-list) (beside/align "bottom" (rectangle 10 (first height-list) "solid" "black")
                                       (bar-graph (rest height-list)))]))

(check-expect (bar-graph '()) (rectangle 1 1 "solid" "white"))
(check-expect (bar-graph (list 1))
              (beside/align "bottom" (rectangle 10 1 "solid" "black") (rectangle 1 1 "solid" "white")))
(check-expect (bar-graph (list 1 2 3))
              (beside/align "bottom" (rectangle 10 1 "solid" "black") (rectangle 10 2 "solid" "black") (rectangle 10 3 "solid" "black") (rectangle 1 1 "solid" "white")))
(check-expect (bar-graph (list 100 10 0 1000))
              (beside/align "bottom" (rectangle 10 100 "solid" "black") (rectangle 10 10 "solid" "black") (rectangle 10 0 "solid" "black")  (rectangle 10 1000 "solid" "black") (rectangle 1 1 "solid" "white")))

; is-in? :: Any List-of-any -> Boolean
; check if a given value is contained inside list
(define (is-in? value any-list)
  (cond
    [(empty? any-list) #false]
    [(cons? any-list)
     (if (equal? value (first any-list))
         #true
         (is-in? value (rest any-list)))]))

(check-expect (is-in? 1 '()) #false)
(check-expect (is-in? 2 (list 1 2 3 4)) #true)
(check-expect (is-in? 5 (list "a" "b" "c" "d")) #false)
(check-expect (is-in? "a" (list 1 2 "b" "a")) #true)
(check-expect (is-in? "1" (list 1 2 "12" (list "1"))) #false)

; words-to-sentence :: List-of-strings -> String
; combine a list of strings into a single string, adding spaces between words
(define (words-to-sentence string-list)
  (cond
    [(empty? string-list) ""]
    [(cons? string-list)
     (cond
       [(empty? (rest string-list)) (first string-list)]
       [(cons? (rest string-list))
        (string-append (first string-list) " " (words-to-sentence (rest string-list)))])]))

(check-expect (words-to-sentence '()) "" )
(check-expect (words-to-sentence (list "")) "")
(check-expect (words-to-sentence (list "a")) "a")
(check-expect (words-to-sentence (list "Hello" "world")) "Hello world")
(check-expect (words-to-sentence (list "a " " b  " "  c   ")) "a   b     c   ")