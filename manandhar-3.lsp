;;; Student's Name: Bickey Manandhar
;;;Helping functions used : helping-func


;;; Solution to Problem 1
(defun MIN-2 (A B) 
  (if (or (not (numberp A)) (not (numberp B))) 
    'Error
    (if (<= A B) 
      A
      B)))

;;; Solution to Problem 2
(defun SAFE-AVG (A B) 
  (if (or (not (numberp A)) (not (numberp B))) 
    NIl
    (/ (+ A B) 2)))

;;; Solution to Problem 3
(defun ODD-GT-MILLION (A) 
  (if (and (INTEGERP A) (> A 1000000) (ODDP A)) 
    T
    NIL))

;;; Solution to Problem 4
(defun MULTIPLE-MEMBER (X Y) 
  (if (AND (or (symbolp X) (numberp X)) (listp Y)) 
    (member X (cdr (member X Y)))
    NIL))

;;; Solution to Problem 5
(defun MONTH->INTEGER (month) 
  (cond 
    ((equalp month 'JANUARY) 1)
    ((equalp month 'FEBRUARY) 2)
    ((equalp month 'MARCH) 3)
    ((equalp month 'APRIL) 4)
    ((equalp month 'MAY) 5)
    ((equalp month 'JUNE) 6)
    ((equalp month 'JULY) 7)
    ((equalp month 'AUGUST) 8)
    ((equalp month 'SEPTEMBER) 9)
    ((equalp month 'OCTOBER) 10)
    ((equalp month 'NOVEMBER) 11)
    ((equalp month 'DECEMBER) 12)
    (t 'ERROR)))

;;; Solution to Problem 6
(defun SCORE->GRADE (s) 
  (if (numberp s) 
    (cond 
      ((>= s 90) 'A)
      ((and (>= s 87) (< s 90)) 'A-)
      ((and (>= s 83) (< s 87)) 'B+)
      ((and (>= s 80) (< s 83)) 'B)
      ((and (>= s 77) (< s 80)) 'B-)
      ((and (>= s 73) (< s 77)) 'C+)
      ((and (>= s 70) (< s 73)) 'C)
      ((and (>= s 60) (< s 70)) 'D)
      ((< s 60) 'F)
    )
    NIL))

;;; Solution to Problem 7
(defun GT (X Y) 
  (AND 
    (AND (numberp X) (numberp Y))
    (> X Y)))

;;; Solution to Problem 8
(defun SAME-PARITY (x y) 
  (AND (AND (integerp x) (integerp y)) (not (oddp (+ x y)))))

;;;helping function for solution 9
(defun helping-func (x y) 
  (AND (AND (numberp x) (numberp y)) (not (zerop y))))

;;; Solution to Problem 9
(defun SAFE-DIV (a b) 
  (AND (helping-func a b) (/ a b)))




