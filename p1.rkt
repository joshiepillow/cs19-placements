;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname p1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; vietnam
(overlay (star 60 "solid" "yellow")
         (rectangle 300 200 "solid" "red"))

; chile
(above (beside (overlay (star 30 "solid" "white")
                        (rectangle 100 100 "solid" "blue"))
               (rectangle 200 100 "solid" "white"))
       (rectangle 300 100 "solid" "red"))

; suriname
(above (rectangle 300 40 "solid" "dark green")
       (rectangle 300 20 "solid" "white")
       (overlay (star 45 "solid" "yellow")
                (rectangle 300 80 "solid" "dark red"))
       (rectangle 300 20 "solid" "white")
       (rectangle 300 40 "solid" "dark green"))

; saint lucia
(overlay (overlay/align "middle" "bottom" (isosceles-triangle 98 90 "solid" "yellow")
                        (isosceles-triangle 160 45 "solid" "black")
                        (isosceles-triangle 180 45 "solid" "white"))
         (rectangle 300 200 "solid" "light blue"))

; turkey
(overlay/offset (beside (underlay/offset (circle 50 "solid" "white") 10 0
                                         (circle 42 "solid" "red"))
                        (rotate 30 (star 25 "solid" "white"))) 20 0
                (rectangle 300 200 "solid" "red"))