(use matchable)

(define fps
  (map-channel clock
    (lambda (c) (* (/ 1 c) 1000))))

(define hex-angle
  (map-channel time
    (lambda (t) (/ t (* pi 200)))))

(define key-events
  (filter-channel events
    (lambda (e)
      (eqv? (car e) 'key))))

(define movements
  (fold-channel
    (filter-channel key-events
      (lambda (k)
        (or (equal? k '(key pressed #\b))
            (equal? k '(key released #\b))
            (equal? k '(key pressed #\p))
            (equal? k '(key released #\p)))))
    (lambda (k prev)
      (match k
        (('key 'pressed #\b)  (cons #t (cdr prev)))
        (('key 'pressed #\p)  (cons (car prev) #t))
        (('key 'released #\b)  (cons #f (cdr prev)))
        (('key 'released #\p)  (cons (car prev) #f))))
    '(#f . #f)))
