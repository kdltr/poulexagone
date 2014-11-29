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
  (let* ((rp 1)
         (rn (- rp))
         (sp (/ rp 2))
         (sn (- sp))
         (tp (* rp (sin (/ pi 3))))
         (tn (- tp)))
    (list (vnorm rn 0) (vnorm sn tn) (vnorm sp tn)
          (vnorm rp 0) (vnorm sp tp) (vnorm sn tp))))

(define hexagon-coordinates
  (map (cut vmul 50 <>) normal-coordinates))

(define (apply-with-scale p) (vmul d (p normal-coordinates)))
(define (zero v) '(0 0))

(define bg1-coordinates
  (map apply-with-scale (list first second zero third fourth zero fifth sixth)))

(define bg2-coordinates
  (map apply-with-scale (list first sixth zero fourth fifth zero second third)))

(define (draw-background coords color)
  (cairo-move-to *c* 0 0)
  (for-each line-to coords)
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

(define (draw-level)
  (draw-background bg1-coordinates '(0.4 0.4 0.6))
  (draw-background bg2-coordinates '(0.1 0.1 0.3)))

(define (draw-player)
  (cairo-set-source-rgb *c* 0.6 0.6 1)
  (cairo-rotate *c* (channel-value player-position))
  (cairo-translate *c* 0 -60)
  (cairo-move-to *c* -5 0)
  (cairo-line-to *c* 5 0)
  (cairo-line-to *c* 0 -10)
  (cairo-close-path *c*)
  (cairo-fill *c*))

(define (draw-wall zone position width)
  (let ((units (take normal-coordinates 2)))
    (move-to (vmul position (car units)))
    (line-to (vmul (+ position width) (car units)))
    (line-to (vmul (+ position width) (cadr units)))
    (line-to (vmul position (cadr units)))
    (cairo-close-path *c*)
    (cairo-set-source-rgb *c* 1 1 0)
    (cairo-fill *c*)))


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
        (sprintf "Zone ~A" (channel-value player-zone))))

(define (draw-all)
  ; game board
  (cairo-save *c*)
  (cairo-translate *c* cx cy)
  (cairo-scale *c* 1 0.8)
  (cairo-rotate *c* (channel-value hex-angle))
  (draw-level)
  (draw-wall 5 (channel-value wall-position) 30)
  (draw-hexagon '(0.1 0.1 0.3) '(0 0 1))
  (draw-player)
  (cairo-restore *c*)

  ; overlay
  (draw-overlay))
