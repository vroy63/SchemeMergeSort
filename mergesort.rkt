(define L '(3 5 2 8 6 1 4 9 7))
(display "---- original list \n")
(display L) (newline)


; Define leftlist and rightlist
(define (leftlist lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (leftlist (cdr lst) (- n 1)))))

(define (rightlist lst n)
  (if (null? lst)
      '()
      (if (< n 1)
          lst
          (rightlist (cdr lst) (- n 1)))))

(display "---- left and right lists\n")
(display (leftlist L 4)) (newline) ; First half
(display (rightlist L 4)) (newline) ; Second half


; Define lefthalf and righthalf
(define (lefthalf lst)
  (leftlist lst (quotient (length lst) 2)))

(define (righthalf lst)
  (rightlist lst (quotient (length lst) 2)))

(display "---- left and right half\n")
(display "left halves\n")
(display (lefthalf L)) (newline)
(display (lefthalf (lefthalf L))) (newline)
(display "right halves\n")
(display (righthalf L)) (newline)
(display (righthalf (righthalf L))) (newline)


; Define merge
(define (merge lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((< (car lst1) (car lst2))
         (cons (car lst1) (merge (cdr lst1) lst2)))
        (else
         (cons (car lst2) (merge lst1 (cdr lst2))))))

(display "---- merge test 1\n")
(display (merge '(2 7 9) '(3 6 8))) (newline)
(display "---- merge test 2\n")
(display (merge (lefthalf L) (righthalf L))) (newline)


; Define mergesort
(define (mergesort lst)
  (if (> (length lst) 1)
      (merge (mergesort (lefthalf lst))
             (mergesort (righthalf lst)))
      lst))

(display "---- mergesort test\n")
(display (mergesort L)) (newline)