
; Public API
; (make-channel)
; (channel-enqueue channel value)
; (fold-channel channel proc seed)
; (filter-channel channel pred?)
; (map-channel channel proc)
; (combine-channels proc . channels)
; (channel-value channel)

(define-record %channel
               on-receive value publishers subscribers)

(define (channel-enqueue channel message)
  ((%channel-on-receive channel) channel message))

(define (dispatch-message source-channel message)
  (%channel-value-set! source-channel message)
  (for-each
    (cut channel-enqueue <> message)
    (%channel-subscribers source-channel)))

(define (channel-subscribe publisher subscriber)
  (%channel-subscribers-set! publisher
    (cons subscriber
          (%channel-subscribers publisher))))

(define (channel-unsubscribe publisher subscriber)
  (%channel-subscribers-set! publisher
    (delete subscriber
            (%channel-subscribers publisher))))

(define channel-value %channel-value)

(define (make-channel)
  (make-%channel dispatch-message 'frp-none '() '()))

(define (siphon-channel publishers on-receive #!optional (init 'frp-none)
                                                         (subscribers '()))
  (let ((new  (make-%channel on-receive
                             init
                             publishers
                             subscribers)))
    (for-each (cut channel-subscribe <> new) publishers)
    new))

(define (map-channel proc channel . rest)
  (siphon-channel (cons channel rest)
                  (lambda (c m)
                    (dispatch-message
                      c
                      (apply proc
                             (map channel-value
                                  (%channel-publishers c)))))))

(define (filter-channel pred? channel)
  (siphon-channel (list channel)
                  (lambda (c m)
                    (when (pred? m)
                      (dispatch-message c m)))))

(define (fold-channel proc seed channel . rest)
  (siphon-channel (cons channel rest)
                  (lambda (c m)
                    (dispatch-message
                      c
                      (apply proc
                             (append (map channel-value
                                          (%channel-publishers c))
                                     (list (%channel-value c))))))
                  seed))
