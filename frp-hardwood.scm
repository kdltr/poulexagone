(use hardwood data-structures)

(setup-thread (current-thread))

; Public API
; (make-channel)
; (channel-enqueue channel value)
; (fold-channel channel proc seed)
; (filter-channel channel pred?)
; (map-channel channel proc)
; (combine-channels proc . channels)
; (channel-value channel)

; Messages
; ('push value)
; ('subscribe channel)
; ('unsubscribe channel)
; (pid tag 'value)

(define (channel:loop proc value publishers subscribers)
  (recv
    (('push val)  (let ((res (proc value publishers subscribers val)))
                    (channel:loop proc
                                  res
                                  publishers
                                  subscribers)))
    (('subscribe channel)  (channel:loop proc
                                         value
                                         publishers
                                         (cons channel subscribers)))
    (('unsubscribe channel)  (channel:loop proc
                                           value
                                           publishers
                                           (delete channel subscribers)))
    ((pid tag ('value))  (! pid (list tag value))
                         (channel:loop proc
                                       value
                                       publishers
                                       subscribers))
    (('stop)  (for-each (cut channel-unsubscribe <> (self)) publishers))
    (unknown  (error "unknown message" unknown))
    ))

(define (channel-enqueue channel value)
  (! channel (list 'push value)))

(define (channel-subscribe publisher subscriber)
  (! publisher (list 'subscribe subscriber)))

(define (channel-unsubscribe publisher subscriber)
  (! publisher (list 'subscribe subscriber)))

(define (channel-value channel)
  (!? channel '(value)))

(define (channel-stop channel)
  (! channel '(stop)))

(define (dispatch-message subscribers message)
  (for-each (cut channel-enqueue <> message) subscribers)
  message)

(define (siphon-channel publishers on-receive #!optional (init 'frp-none)
                                                         (subscribers '()))
  (let ((new  (spawn channel:loop
                     on-receive
                     init
                     publishers
                     subscribers)))
    (for-each (cut channel-subscribe <> new) publishers)
    new))

(define (make-channel)
  (spawn channel:loop
         (lambda (v p s m) (dispatch-message s m))
         'frp-none
         '()
         '()))

(define (map-channel proc channel . rest)
  (siphon-channel (cons channel rest)
                  (lambda (v p s m)
                    (dispatch-message s
                                      (apply proc (map channel-value p))))))

(define (filter-channel pred? channel)
  (siphon-channel (list channel)
                  (lambda (v p s m)
                    (when (pred? m)
                      (dispatch-message s m)))))

(define (fold-channel proc seed channel . rest)
  (siphon-channel (cons channel rest)
                  (lambda (v p s m)
                    (dispatch-message s
                                      (apply proc (append (map channel-value p)
                                                          (list v)))))
                  seed))
