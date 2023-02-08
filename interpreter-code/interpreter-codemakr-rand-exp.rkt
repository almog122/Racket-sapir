(define (to-int float)
  (inexact->exact float))

(define (make-random-a-b a b)
  (let ((r (random)))
    (to-int (round (+ a (* r (- b a)))))))


(define (make-random-exp . args)
  1)
(define (get-rand-elem li)
  (list-ref li (to-int (make-random-a-b 0 (- (length li) 1)))))
;(get-rand-elem '(1 2 3 4 5))
                       
(define (println x)
  (display x)
  (newline))
(define exp-types
  '(app lambda if symbol int boolean))

(define (ints-to n)
  (cond ((= n 0) '())
        ((<= n 1) (list n))
        (else (append (ints-to (- n 1))
              (list n)))))

(ints-to 10)
(define count-calls 0)

(define (gen-rand-exp . type)
  (set! count-calls (+ 1 count-calls))
  (if (>= count-calls 5) 1 ( set! type type))
  (let ((exp-type
         (if (null? type)
             (get-rand-elem exp-types)
             (car type))))
    (println exp-type)
    (cond ((member type '(int boolean symbol))
           (let ((atom-type type))
             (println 'inner-let)
             (cond ((eq? atom-type 'int)
                    (to-int (make-random-a-b 0 100)))
                   ((eq? atom-type 'boolean)
                    (if (> 0.5 (random)) #t #f))
                   (else ;symbol
                    (gensym)))))
           ((eq? exp-type 'lambda)
            (let ((var-num
                   (if (eq? (length type) 2)
                       (cadr type)
                       (make-random-a-b 0 5))))
              (let ((var-list (map (lambda (x) (gen-rand-exp 'symbol))
                                   (ints-to var-num))))
                (list 'lambda var-list (gen-rand-exp) ))))
           ((eq? exp-type 'if)
            (list 'if (gen-rand-exp) (gen-rand-exp) (gen-rand-exp)))
           (else
            (println 'app!)
            (let ((var-num (make-random-a-b 0 5)))
              (cons
               (gen-rand-exp 'lambda var-num)
               (map (lambda (x) (gen-rand-exp))
                    (ints-to var-num))))))))
;(gen-rand-exp 'symbol)              