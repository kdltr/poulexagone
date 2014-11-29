(use cairo
     doodle
     doodle-colors)

(include "frp")

(current-background black)

(define clock (make-channel))

(define time
  (fold-channel clock + 0))

(define events (make-channel))

(new-doodle)

(define pi cairo-pi)
(define *c* (doodle-context))
(define d (sqrt (+ (expt doodle-width 2) (expt doodle-height 2))))

(include "logic")
(include "draw")

(world-changes
  (lambda (evlist dt exit)
    ; logic run time
    (channel-enqueue clock (if (zero? dt) 1 dt))
    (for-each (cut channel-enqueue events <>) evlist)

    ; drawing callbacks
    ; (clear-screen)
    (cairo-new-path *c*)
    (draw-all)
    (cairo-close-path *c*)))

(run-event-loop)
