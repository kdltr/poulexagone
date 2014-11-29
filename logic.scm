(use matchable)

; Utility

(define (xor a b)
  (and (or a b)
       (not (and a b))))


; Game logic

(define fps
  (map-channel clock
    (lambda (c) (if (zero? c)
                  0
                  (* (/ 1 c) 1000)))))

(define hex-angle
  (map-channel time
    (lambda (t) (/ t (* pi 300)))))

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
        (('key 'pressed #\b)  (list #t (second prev)))
        (('key 'pressed #\p)  (list (first prev) #t))
        (('key 'released #\b)  (list #f (second prev)))
        (('key 'released #\p)  (list (first prev) #f))))
    '(#f #f)))

(define player-speed (* 2 pi))

(define (combine-clock-movement clock input)
  (let ((dt (/ clock 1000)))
    (if (apply xor input)
      (* player-speed
         (if (car input) (- dt) dt))
      #f)))

(define player-position
  (fold-channel
    (combine combine-clock-movement clock movements)
    (lambda (delta pos)
      (if delta
        (+ pos delta)
        pos))
    0))

(define player-zone
  (map-channel
    player-position
    (lambda (pos)
      (modulo
        (inexact->exact
          (ceiling (/ (- pos (/ pi 6)) (/ pi 3))))
        6))))
(channel-enqueue player-zone 0)

(define wall-position
  (map-channel
    (fold-channel
      clock
      (lambda (dt pos)
        (+ (/ dt 4)
           pos))
      0)
    (lambda (p)
       (modulo (round (- p)) 600))))
