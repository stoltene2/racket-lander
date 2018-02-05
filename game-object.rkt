(module game-object racket
  (provide (all-defined-out))

  (require lens)

  (struct game-object [input
                       physics
                       draw])

  (struct posn [x y] #:transparent)
  (define-struct-lenses posn)

  (struct velocity [x y] #:transparent)
  (define-struct-lenses velocity)

  )
