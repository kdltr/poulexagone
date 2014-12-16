(use cairo
     doodle
     doodle-colors)

(include "frp")

(current-background black)

(define clock (make-channel))
(define events (make-channel))

(define time
  (fold-channel + 0 clock))

(new-doodle)

(define pi cairo-pi)
(define *c* (doodle-context))

(include "logic")
(include "draw")

(world-changes
  (lambda (evlist dt exit)
    ; logic run time
    (channel-enqueue clock dt)
    (for-each (cut channel-enqueue events <>) evlist)

    ; drawing callbacks
    (cairo-new-path *c*)
    (draw-all)))

(run-event-loop)
