(use (prefix nanovg-gl2 nvg:)
     doodle
     doodle-colors
     gl)

(include "frp")

(current-background black)

(define clock (make-channel))
(define events (make-channel))

(define time
  (fold-channel + 0 clock))

(new-doodle)

(define pi 3.14159265358979323846264338327)
(define *c* (nvg:create-context))
(nvg:create-font! *c* "DejaVu" "/home/kooda/.guix-profile/share/fonts/truetype/DejaVuSansMono.ttf")


(include "logic")
(include "draw")

(world-changes
 (lambda (evlist dt exit)
   ;; logic run time
   (channel-enqueue clock dt)
   (for-each (cut channel-enqueue events <>) evlist)

   ;; drawing callbacks
   (gl:Clear (+ gl:COLOR_BUFFER_BIT gl:STENCIL_BUFFER_BIT))
   (nvg:begin-frame! *c* doodle-width doodle-height (/ doodle-width doodle-width))
   (draw-all)
   (nvg:end-frame! *c*)))

(run-event-loop)
