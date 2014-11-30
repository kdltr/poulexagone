; channel-enqueue
; subscribe
; unsubscribe)

(import scheme chicken)

(use srfi-1)

(define-record channel subscribers value)

(define make-channel
  (let ((%make-channel make-channel))
    (lambda ()
      (%make-channel '() (void)))))

(define (channel-enqueue channel value)
  (for-each (lambda (s) (s value)) (channel-subscribers channel))
  (channel-value-set! channel value))

(define (on-channel-receive channel proc)
  (channel-subscribers-set! channel (cons proc (channel-subscribers channel))))

(define (siphon-channel source-channel
                        #!optional
                        (destination-channel (make-channel))
                        (on-receive channel-enqueue))
  (on-channel-receive source-channel
    (lambda (msg) (on-receive destination-channel msg)))
  destination-channel)

(define (fold-channel channel proc seed)
  (let ((result (make-channel)))
    (channel-enqueue result seed)
    (on-channel-receive channel
      (let ((previous seed))
        (lambda (msg)
          (let ((value (proc msg previous)))
            (set! previous value)
            (channel-enqueue result value)))))
    result))

(define (map-channel channel proc)
  (siphon-channel channel
                  (make-channel)
                  (lambda (dest msg)
                    (channel-enqueue dest (proc msg)))))

(define (filter-channel channel pred?)
  (siphon-channel channel
                  (make-channel)
                  (lambda (dest msg)
                    (if (pred? msg) (channel-enqueue dest msg)))))

(define (combine-channels proc chan1 chan2)
  (let ((comb (make-channel)))
    (let ((val1 (void))
          (val2 (void)))
      (on-channel-receive chan1
        (lambda (m)
          (set! val1 m)
          (unless (eqv? val2 (void))
            (channel-enqueue comb (proc val1 val2)))))
      (on-channel-receive chan2
        (lambda (m)
          (set! val2 m)
          (unless (eqv? val1 (void))
            (channel-enqueue comb (proc val1 val2))))))
    comb))
