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
    (combine-channels combine-clock-movement clock movements)
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


(define walls-speed 1/4)

; Reuse this with a better frp system
; (define (make-wall zone width)
;   (fold-channel
;     clock
;     (lambda (dt wall)
;       (list (car wall)
;             (max 0 (- (cadr wall) (* dt walls-speed)))
;             (caddr wall)))
;     (list zone 600 width)))
;
; (define walls-clock
;   (filter-channel
;     (fold-channel
;       clock
;       (lambda (dt prev)
;         (if (> (+ dt prev) 500)
;           0
;           (+ dt prev)))
;       0)
;     zero?))


(define (make-wall zone width)
  (list zone 600 width))

(define make-walls
  (let ((last 0))
    (lambda (dt)
      (if (>= (+ last dt) 500)
        (begin
          (set! last 0)
          (map
            (lambda (i)
              (make-wall (random 6) 20))
            (iota (random 5))))
        (begin
          (set! last (+ last dt))
          '())))))

(define (update-wall dt wall)
  (list (car wall)
        (max 0 (- (cadr wall) (* dt walls-speed)))
        (caddr wall)))

(define (update-walls dt walls)
  (map (cut update-wall dt <>) walls))

(define walls
  (fold-channel
    clock
    (lambda (dt walls)
      (append (make-walls dt)
              (remove (o zero? cadr) (update-walls dt walls))))
    '()))

(define death-collision
  (combine-channels
    (lambda (zone walls)
      (filter (lambda (w)
                (and (= zone (car w))
                     (<= (cadr w) (+ hexagon-radius 15))
                     (>= (+ (cadr w) (caddr w)) (+ hexagon-radius 15))))
              walls))
    player-zone
    walls))

(define side-collisions
  (combine-channels
    (lambda (zone walls)
      (let ((low-walls (filter (lambda (w)
                                 (and (or (= (car w) (modulo (- zone 1) 6))
                                          (= (car w) (modulo (+ zone 1) 6)))
                                      (<= (cadr w) (+ hexagon-radius 15))
                                      (>= (+ (cadr w) (caddr w)) (+ hexagon-radius 15))))
                               walls)))
        (list (any (lambda (w) (= (car w) (modulo (- zone 1) 6))) low-walls)
              (any (lambda (w) (= (car w) (modulo (+ zone 1) 6))) low-walls))))
    player-zone
    walls))


; Initialization
(channel-enqueue player-zone 0)
