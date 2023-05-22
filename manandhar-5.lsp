;;; Student's Name: Bickey Manandhar

;;;Solution to Problem 1

(defun index (n l) 
  (if (endp l) 
    'error
    (let 
      ((x (index (- n 1) (cdr l))))
      (if (equal n 1) 
        (car l)
        x))))

;;;Solution to Problem 2

(defun min-first (L) 
  (if (listp L) 
    (if (equal (cadr L) NIL) 
      L
      (let 
        ((X (min-first (cdr L))))
        (if (> (car L) (car X)) 
          (cons (car X) (cons (car L) (cdr X)))
          L)))))

;;new solution for problem 2 does while practicing for exam 1
(defun min-first (L)
  (if (null (cdr L)) l 
       (let ((X (min-first (cdr L))))
       (if (< (car L) (car X))
       (cons (car L) X)
       (append (list (car X) (car L)) (cdr X))))))

;;;Solution to Problem 3

(defun ssort (L) 
  (if (endp L) 
    nil
    (let* 
      ((L1 (min-first L)) 
        (X (ssort (cdr L1)))
      )
      (cons (car L1) X))))

;;; Solution to Problem 4

(defun partition (L p) 
  (if (null L) 
    (list nil nil)
    (let 
      ((X (partition (cdr L) p)))
      (if (< (car L) p) 
        (list (cons (car L) (car X)) (cadr X))
        (list (car X) (cons (car L) (cadr X)))))))

(defun qsort (L) 
  (if (endp L) 
    NIL
    (let 
      ((pL (partition (cdr L) (car L))))
      (append (qsort (car pL)) (cons (car L) (qsort (cadr pL)))))))

;;; Solution to Problem 5

(defun merge-lists (L1 L2) 
  (cond 
    ((endp L1) L2)
    ((endp L2) L1)
    ((< (car L1) (car L2)) (cons (car L1) (merge-lists (cdr L1) L2)))
    (t (cons (car L2) (merge-lists L1 (cdr L2))))))

;;; Solution to Problem 6

(defun split-list (L) 
  (if (null L) 
    (list NIL NIL)
    (let 
      ((X (split-list (cdr L))))
      (append (list (cons (car L) (cadr X))) (list (car X))))))

(defun msort (L) 
  (cond 
    ((endp L) NIL)
    ((endp (cdr L)) L)
    (t
     (let* 
       ((sp-list (split-list L)) 
         (l1 (msort (car sp-list)))
         (l2 (msort (cadr sp-list)))
       )
       (merge-lists l1 l2))))) 

;;; Solution to Problem 7 

(defun remove-adj-dupl (L) 
  (if (endp L) 
    nil
    (let 
      ((x (remove-adj-dupl (cdr L))))
      (if (equal (car L) (car x)) 
        x
        (cons (car L) x)))))

;;; Solution to Problem 8

(defun unrepeated-elts (L) 
  (cond 
    ((endp L) nil)
    ((or (endp (cdr L)) (not (equal (car L) (cadr L)))) 
     (cons (car L) (unrepeated-elts (cdr L)))
    )
    ((or (endp (cddr L)) (not (equal (car L) (caddr L))))
     (unrepeated-elts (cddr L))
    )
    (t (unrepeated-elts (cdr L))))) 

;;; Solution to Problem 9

(defun repeated-elts (L) 
  (cond 
    ((endp L) nil)
    ((or (endp (cdr L)) (not (equal (car L) (cadr L))))
     (repeated-elts (cdr L))
    )
    ((or (endp (cddr L)) (not (equal (car L) (caddr L))))
     (cons (car L) (repeated-elts (cddr L)))
    )
    (t (repeated-elts (cdr L))))) 

;;; Solution to Problem 10

(defun COUNT-REPETITIONS (L) 
  (if (endp L) 
    nil
    (let 
      ((x (COUNT-REPETITIONS (cdr L))))
      (if (equal (car L) (cadr L)) 
        (append (list (list (+ 1 (caar x)) (cadar x))) (cdr x))
        (append (list (list 1 (car L))) x)))))


;;; Solution to Problem 11

(defun subset (F L) 
  (if (endp L) 
    nil
    (let 
      ((x (subset F (cdr L))))
      (if (funcall F (car L)) 
        (cons (car L) x)
        x))))

;;;Solution to Problem 12 (i)    

(defun our-some (F L)
  (if (endp L)
      nil
      (if (funcall F (car L))
          L
          (our-some F (cdr L)))))

;;;Solution to Problem 12 (ii)

(defun our-every (F L)
  (if (endp L)
      t
      (if (funcall F (car L))
          (our-every F (cdr L))
          nil)))


;;;Solution to Problem 13

(defun partition1 (f L p)
  (if (null L)
      (list nil nil)
      (let ((x (partition1 f (cdr L) p)))
        (if (funcall f (car L) p)
            (list (cons (car L) (car x)) (cadr x))
            (list (car x) (cons (car L) (cadr x)))))))

(defun qsort1 (f L)
  (if (endp L)
      nil
      (let ((pL (partition1 f (cdr L) (car L))))
        (append (qsort1 f (car pL)) (cons (car L) (qsort1 f (cadr pL)))))))

;;;Solution to Problem 14
(defun foo (F L) 
  (if (endp L) 
    nil
    (let 
      ((x (foo F (cdr L))))
      (cons 
        (cons (funcall F (car L)) (cdr L))
        (mapcar 
          (lambda (lst) 
            (cons (car L) lst)
          )
          x)))))

;;;Solution to Problem 15 (a)

(defun TR-ADD (L acc) 
  (if (endp L) 
    acc
    (TR-ADD (cdr L) (+ acc (car L)))))


(defun TR-MUL (L acc) 
  (if (endp L) 
    acc
    (TR-MUL (cdr L) (* acc (car L)))))


(defun TR-FAC (n acc) 
  (if (= n 0) 
    acc
    (TR-FAC (- n 1) (* acc n))))

;;;Solution to Problem 15 (b)

(defun slow-primep (n) 
  (if (and (> n 1) (= (MOD (TR-FAC (- n 1) 1) n) (- n 1))) T NIL))

;;;to find the least prime number greater than 20,000 I defined function below,if n=20000, it returns 20011
(defun least-prime-number (n) 
  (if (slow-primep n) 
    n
    (least-prime-number (+ n 1))))

;;;Solution to Problem 16 (a)
(defun TRANSPOSE1 (m)
   (cond ((endp (cdr m)) (mapcar #'list (car m)))
	 (t (mapcar #'cons (car m) (TRANSPOSE1 (cdr m))))))

;;;Solution to Problem 16 (b)
(defun TRANSPOSE2 (m)
   (cond ((endp (cdar m)) (list (mapcar #'car m)))
	 (t (cons (mapcar #'car m) (TRANSPOSE2 (mapcar #'rest m))))))

;;;Solution to Problem 16 (c)
(defun TRANSPOSE3 (m)
   (apply #'mapcar #'list m))


