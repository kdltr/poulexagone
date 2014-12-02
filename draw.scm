(define cx (/ doodle-width 2))
(define cy (/ doodle-height 2))
(define d (sqrt
            (+ (expt doodle-width 2)
               (expt doodle-height 2))))

(define (vnorm x y)
  (let ((length (sqrt (+ (expt x 2) (expt y 2)))))
    (list (/ x length) (/ y length))))

(define (vmul k v)
  (map (cut * k <>) v))

(define line-to
  (cut apply cairo-line-to *c* <>))

(define move-to
  (cut apply cairo-move-to *c* <>))


; Game

(define normal-coordinates
  (let* ((rp 1) (rn (- rp))
         (sp (/ rp 2)) (sn (- sp))
         (tp (* rp (sin (/ pi 3)))) (tn (- tp)))
    (list
      (vnorm sn tn) (vnorm sp tn) (vnorm rp 0)
      (vnorm sp tp) (vnorm sn tp) (vnorm rn 0))))

(define hexagon-radius 40)

(define hexagon-coordinates
  (map (cut vmul hexagon-radius <>) normal-coordinates))

(define (apply-with p) (p normal-coordinates))

(define zones-coordinates
  (vector (map apply-with (list first second))
          (map apply-with (list second third))
          (map apply-with (list third fourth))
          (map apply-with (list fourth fifth))
          (map apply-with (list fifth sixth))
          (map apply-with (list sixth first))))

(define (draw-zone n color)
  (cairo-move-to *c* 0 0)
  (for-each line-to (map (cut vmul d <>) (vector-ref zones-coordinates n)))
  (cairo-close-path *c*)
  (apply cairo-set-source-rgb *c* color)
  (cairo-fill *c*))

(define (draw-hexagon fill stroke)
  (apply cairo-move-to *c* (car hexagon-coordinates))
  (for-each line-to (cdr hexagon-coordinates))
  (cairo-close-path *c*)
  (apply cairo-set-source-rgb *c* fill)
  (cairo-fill-preserve *c*)
  (apply cairo-set-source-rgb *c* stroke)
  (cairo-set-line-width *c* 3)
  (cairo-stroke *c*))


(define (draw-player)
  (cairo-set-source-rgb *c* 0.6 0.6 1)
  (cairo-rotate *c* (channel-value player-position))
  (cairo-translate *c* 0 (- (+ hexagon-radius 5)))
  (cairo-move-to *c* -5 0)
  (cairo-line-to *c* 5 0)
  (cairo-line-to *c* 0 -10)
  (cairo-close-path *c*)
  (cairo-fill *c*))

(define (draw-wall zone position width color)
  (let ((units (vector-ref zones-coordinates zone)))
    (move-to (vmul position (car units)))
    (line-to (vmul (+ position width) (car units)))
    (line-to (vmul (+ position width) (cadr units)))
    (line-to (vmul position (cadr units)))
    (cairo-close-path *c*)
    (apply cairo-set-source-rgb *c* color)
    (cairo-fill *c*)))

(define (draw-background c1 c2)
  (for-each
    (lambda (n)
      (draw-zone n (if (even? n) c1 c2)))
    (iota 6)))


; Overlay

(define (draw-overlay)
  ; fps
  (text 10 10 (sprintf "~A fps" (channel-value fps)))
  ; time elapsed
  (text 10 30
        (sprintf "~A" (/ (channel-value time) 1000)))
  ; player angle
  (text 10 50
        (sprintf "~A°" (channel-value player-position)))
  (text 10 70
        (sprintf "Zone ~A" (channel-value player-zone)))
  (when (not (null? (channel-value death-collision)))
    (text 10 90 "Collision")))

(define (draw-all)
  (cairo-save *c*)

  (cairo-translate *c* cx cy)
  ;; fancy effects
  (cairo-scale *c* 1 0.8)
  (cairo-rotate *c* (channel-value hex-angle))

  (draw-background '(0.4 0.4 0.6) '(0.1 0.1 0.3))

  ; walls
  (for-each
    (lambda (w)
      (apply (cut draw-wall <> <> <> '(1 1 0)) w))
    (channel-value walls))

  ; player
  (draw-hexagon '(0.1 0.1 0.3) '(0 0 1))
  (draw-player)

  (cairo-restore *c*)

  ; overlay
  (draw-overlay))
