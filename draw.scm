(define cx (/ doodle-width 2))
(define cy (/ doodle-height 2))
(define d (sqrt
            (+ (expt doodle-width 2)
               (expt doodle-height 2))))


; Game

(define (level-background rp)
  (let* ((rn (- rp))
         (sp (/ rp 2))
         (sn (- sp))
         (tp (* rp (sin (/ pi 3))))
         (tn (- tp)))

    ; background
    (cairo-set-source-rgb *c* 0.4 0.4 0.6)
    (cairo-move-to *c* 0 0)
    (cairo-line-to *c* (* d rn) (* d 0))
    (cairo-line-to *c* (* d sn) (* d tn))
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d rp) (* d 0))
    (cairo-line-to *c* (* d sp) (* d tn))
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d sp) (* d tp))
    (cairo-line-to *c* (* d sn) (* d tp))
    (cairo-line-to *c* 0 0)
    (cairo-fill *c*)
    (cairo-set-source-rgb *c* 0.1 0.1 0.3)
    (cairo-line-to *c* (* d sn) (* d tp))
    (cairo-line-to *c* (* d rn) (* d 0))
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d rp) (* d 0))
    (cairo-line-to *c* (* d sp) (* d tp))
    (cairo-line-to *c* 0 0)
    (cairo-line-to *c* (* d sp) (* d tn))
    (cairo-line-to *c* (* d sn) (* d tn))
    (cairo-line-to *c* 0 0)
    (cairo-fill *c*)

    ; hexagon
    (cairo-move-to *c* rn 0)
    (cairo-line-to *c* sn tn)
    (cairo-line-to *c* sp tn)
    (cairo-line-to *c* rp 0)
    (cairo-line-to *c* sp tp)
    (cairo-line-to *c* sn tp)
    (cairo-line-to *c* rn 0)
    (cairo-fill-preserve *c*)
    (cairo-set-source-rgb *c* 0 0 1)
    (cairo-stroke *c*)))

(define (draw-level)
  (cairo-set-line-width *c* 3)
  (level-background 50))

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
