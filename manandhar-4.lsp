;;; Student's Name: Bickey Manandhar

;;; Solution to Problem 1
(defun sum (L) 
  (if (null L) 
    0
      (+ (sum (cdr L)) (car L))))

;;; Solution to Problem 2
(defun neg-nums (L) 
  (if (null L) 
    NIl
    (let 
      ((X (neg-nums (cdr L))))
      (if (< (car L) 0) 
        (cons (car L) X)
        X))))

;;; Solution to Problem 3
(defun inc-list-2 (L n) 
  (if (null L) 
    NIL
      (cons (+ (car L) n) (inc-list-2 (cdr L) n))))

;;; Solution to Problem 4
(defun insert (n L) 
  (if (null L) 
    (list n)
      (cond 
        ((<= n (car L)) (cons n L))
        (t (cons (car L) (insert n (cdr L)))))))

;;; Solution to Problem 5
(defun isort (L) 
  (if (null L) 
    NIL
      (insert (car L) (isort (cdr L)))))

;;; Solution to Problem 6
(defun split-list (L) 
  (if (null L) 
    (list NIL NIL)
    (let 
      ((X (split-list (cdr L))))
      (append (list (cons (car L) (cadr X))) (list (car X))))))

;;; Solution to Problem 7
(defun partition (L p) 
  (if (null L) 
    (list nil nil)
    (let 
      ((X (partition (cdr L) p)))
      (if (< (car L) p) 
        (list (cons (car L) (car X)) (cadr X))
        (list (car X) (cons (car L) (cadr X)))))))

;;; Solution to Problem 8
(defun pos (e L) 
  (cond 
    ((endp L) 0)
    ((equal e (car L)) 1)
    (t
     (let 
       ((X (pos e (cdr L))))
       (if (equal X 0) 
         0
         (+ 1 X))))))

;;; Solution to Problem 9
(defun split-nums (N) 
  (if (zerop N) 
    (list '(0) NIL)
    (let 
      ((X (split-nums (- N 1))))
      (list 
        (if (evenp N) 
          (cons N (car X))
          (car X)
        )
        (if (oddp N) 
          (cons N (car (cdr X)))
          (car (cdr X)))))))

;;; Solution to Problem 10
(defun set-union (s1 s2) 
  (if (null s1) 
    s2
    (let 
      ((X (set-union (cdr s1) s2)))
      (if (member (car s1) X) 
        X
        (cons (car s1) X)))))

;;; Solution to Problem 11
(defun set-remove (x s) 
  (if (null s) 
    s
    (let 
      ((Z (set-remove x (cdr s))))
      (if (equal x (car s)) 
        Z
        (cons (car s) Z)))))

;;; Solution to Problem 12
(defun set-excl-union (s1 s2) 
  (cond 
    ((null s1) s2)
    ((null s2) s1)
    (t
     (let 
       ((X (set-excl-union (cdr s1) s2)))
       (if (member (car s1) s2) 
         (set-remove (car s1) X)
         (cons (car s1) X))))))

;;; Solution to Problem 13
(defun singletons (e) 
  (if (null e) 
    nil
    (let 
      ((X (singletons (cdr e))))
      (if (member (car e) (cdr e)) 
        (set-remove (car e) X)
        (cons (car e) X)))))




