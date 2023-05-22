;Assignment 2 - Section G

;(a) Write a Lisp function SQUARE that takes a number as argument and returns the square of the number.
(defun SQUARE (x) (* x x))

;(b) Define a Lisp function SQR-PERIMETER-AREA that returns a list of the perimeter and the area of a square, given the length of one side. Thus (SQR-PERIMETER-AREA 2) should return (8 4).
(defun SQR-PERIMETER-AREA (l) (list (* 4 l) (* l l)))

;(c) Write a Lisp function ROTATE-LEFT that takes a list as argument and returns a new list in which the former first element has become the last element. Thus (ROTATE-LEFT '(A B C D)) should return (B C D A).
(defun ROTATE-LEFT (lst) (append (cdr lst) (list(car lst))))

;(d) Define a Lisp function SWITCH that takes as its argument a two-element list and returns a list consisting of the same two elements, but in the opposite order. Example: (switch '(A B)) => (B A).
(defun SWITCH (two-lst) 
  (list (cadr two-lst) (car two-lst)))

;(e) A point (x, y) in the plane can be represented as a list (x y). Use the function of (a) to write a Lisp
;function that takes two such lists as arguments and returns the distance between the corresponding
;points.(Recallthatthedistancebetweentwopoints(x ,y )and(x ,y )is (x  x) (y  y) .)

(defun distance (x y) 
  (sqrt (+ (square (- (car x) (car y))) ; here we take first element from the list
           (square (- (cadr x) (cadr y)))))) ;here we take second element from the list

;Define a Lisp function QUADRATIC that has three parameters A, B and C and that returns a list of the two roots of the equation Ax2 + Bx + C = 0. You should use the built-in function SQRT.
;Recall that the two roots are given by −B+ sqrt(B2 −4AC) and −B− sqrt(B2 −4AC) .
                                          ;2A               2A
(defun QUADRATIC (a b c) 
  (list (/ (+ (- b)
              (sqrt (- (* b b) (* 4 a c))))
            (* 2 a))
         (/ (- (- b)
                 (sqrt (- (* b b) (* 4 a c))))
            (* 2 a))))