; A library for Functional Reactive Programming

; WARNING: this is a work in progress

; Public API
; (make-channel)
; (channel-enqueue channel value)
; (fold-channel proc seed channel ...)
; (filter-channel pred? channel)
; (map-channel proc channel ...)
; (channel-value channel)

; channel data type, heart of the FRP library
;   on-receive: procedure to call when receiving a message
;   value: current value of the channel
;   publishers: list of channels the channel is subscribed to
;   subscribers: list of subscribers to the channel
(define-record %channel
               on-receive value publishers subscribers)

; Send message to channel
(define (channel-enqueue channel message)
  ((%channel-on-receive channel) channel message))

; Default channel message receiver procedure
; register the incoming message and send it to all subscribers
(define (dispatch-message source-channel message)
  (%channel-value-set! source-channel message)
  (for-each
    (cut channel-enqueue <> message)
    (%channel-subscribers source-channel)))

; Subscribe subscriber to publisher’s messages
(define (channel-subscribe publisher subscriber)
  (%channel-subscribers-set! publisher
    (cons subscriber
          (%channel-subscribers publisher))))

; Unsubscribe subscriber to publisher’s messages
(define (channel-unsubscribe publisher subscriber)
  (%channel-subscribers-set! publisher
    (delete subscriber
            (%channel-subscribers publisher))))

; Get the value of a channel
(define channel-value %channel-value)

; Create a new empty channel with no subscribers or publishers
(define (make-channel)
  (make-%channel dispatch-message 'frp-none '() '()))

; Create a channel that subscribes to all publishers and apply on-receive on
; each incoming message from any of those
(define (siphon-channel publishers on-receive #!optional (init 'frp-none)
                                                         (subscribers '()))
  (let ((new  (make-%channel on-receive
                             init
                             publishers
                             subscribers)))
    (for-each (cut channel-subscribe <> new) publishers)
    new))

; Create a siphon channel that will dispatch the result of calling proc with
; the values of all publishers when receiving a message
(define (map-channel proc channel . rest)
  (siphon-channel (cons channel rest)
                  (lambda (c m)
                    (dispatch-message
                      c
                      (apply proc
                             (map channel-value
                                  (%channel-publishers c)))))))

; Create a siphon channel that will ditpatch incomming messages if they satisfy
; pred?
(define (filter-channel pred? channel)
  (siphon-channel (list channel)
                  (lambda (c m)
                    (when (pred? m)
                      (dispatch-message c m)))))

; Create a siphon channel that will the result of a calling proc with the
; values of the publishers and the last value of the channel
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
