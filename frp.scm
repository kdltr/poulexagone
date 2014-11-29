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

(define (subscribe channel proc)
  (channel-subscribers-set! channel (cons proc (channel-subscribers channel))))

(define (unsubscribe channel proc)
  (channel-subscribers-set! channel
    (delete (channel-subscribers channel) proc)))

(define (siphon-channel source-channel #!optional (destination-channel (make-channel))
                                                  (on-receive channel-enqueue))
  (subscribe source-channel (lambda (msg) (on-receive destination-channel msg)))
  destination-channel)

(define (fold-channel channel proc seed)
  (siphon-channel channel
                  (make-channel)
                  (let ((previous seed))
                    (lambda (dest msg)
                      (let ((result (proc msg previous)))
                        (set! previous result)
                        (channel-enqueue dest result))))))

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

(define on-channel-receive subscribe)
