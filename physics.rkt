(module physics racket
  (provide delta-v
           delta-p)

  (define (delta-v v0 a dt)
    (+ v0 (* a dt)))

  (define (delta-p v0 v dt)
    (* (/ (+ v0 v) 2) dt)))
