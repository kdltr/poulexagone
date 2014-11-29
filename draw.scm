(define cx (/ doodle-width 2))
(define cy (/ doodle-height 2))
(define d (/ (sqrt
               (+ (expt doodle-width 2)
                  (expt doodle-height 2)))
             2))

(define (vnorm x y)
  (let ((length (sqrt (+ (expt x 2) (expt y 2)))))
    (list (/ x length) (/ y length))))

(define (vmul k v)
  (map (cut * k <>) v))


; Game

(define normal-coordinates
  (let* ((rp 1)
         (rn (- rp))
         (sp (/ rp 2))
         (sn (- sp))
         (tp (* rp (sin (/ pi 3))))
         (tn (- tp)))
    (list (vnorm rn 0)
          (vnorm sn tn)
          (vnorm sp tn)
          (vnorm rp 0)
          (vnorm sp tp)
          (vnorm sn tp))))

(define hexagon-coordinates
  (map (cut vmul 50 <>) normal-coordinates))

(define zero '(0 0))

(define bgdark
  (list zero
        (first hexagon-coordinates))
  )

(define (draw-level)
  (let* ((rp 50)
         (rn (- rp))
         (sp (/ rp 2))
         (sn (- sp))
         (tp (* rp (sin (/ pi 3))))
         (tn (- tp))
         (coords hexagon-coordinates)
         (bgcoords (list (first coords) ))
         )

    ; background
    (cairo-move-to *c* 0 0)
    (apply cairo-line-to *c* (vmul d (first coords)))
    (apply cairo-line-to *c* (vmul d (second coords)))
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d sp) (* d tn)) ; third
    (cairo-line-to *c* (* d rp) (* d 0))  ; forth
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d sp) (* d tp)) ; fifth
    (cairo-line-to *c* (* d sn) (* d tp)) ; sixth
    (cairo-close-path *c*)
    (cairo-set-source-rgb *c* 0.4 0.4 0.6)
    (cairo-fill *c*)

    (cairo-move-to *c* 0 0)
    (cairo-line-to *c* (* d rn) (* d 0))  ; first
    (cairo-line-to *c* (* d sn) (* d tp)) ; sixth
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d rp) (* d 0))  ; forth
    (cairo-line-to *c* (* d sp) (* d tp)) ; fifth
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d sn) (* d tn)) ; second
    (cairo-line-to *c* (* d sp) (* d tn)) ; third
    (cairo-close-path *c*)
    (cairo-set-source-rgb *c* 0.1 0.1 0.3)
    (cairo-fill *c*)

    ; hexagon
    (apply cairo-move-to *c* (car coords))
    (for-each
      (cut apply cairo-line-to *c* <>)
      (cdr coords))
    (cairo-close-path *c*)
    (cairo-fill-preserve *c*)
    (cairo-set-source-rgb *c* 0 0 1)
    (cairo-set-line-width *c* 3)
    (cairo-stroke *c*)))

(define (draw-player)
  (cairo-set-source-rgb *c* 0.6 0.6 1)
  (cairo-rotate *c* (channel-value player-position))
  (cairo-translate *c* 0 -60)
  (cairo-move-to *c* -5 0)
  (cairo-line-to *c* 5 0)
  (cairo-line-to *c* 0 -10)
  (cairo-close-path *c*)
  (cairo-fill *c*))


; Overlay

(define (draw-overlay)
  ; fps
  (text 10 10 (sprintf "~A fps" (channel-value fps)))
  ; time elapsed
  (text 10 30
        (sprintf "~A" (/ (channel-value time) 1000)))
  ; player angle
  (text 10 50
        (sprintf "~A°" (channel-value player-position))))

(define (draw-all)
  ; game board
  (cairo-save *c*)
  (cairo-translate *c* cx cy)
  (cairo-scale *c* 1 0.7)
  (cairo-rotate *c* (channel-value hex-angle))
  (draw-level)
  (draw-player)
  (cairo-restore *c*)

  ; overlay
  (draw-overlay))
