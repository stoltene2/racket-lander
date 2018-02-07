(module game-object racket
  (provide (all-defined-out))

  (require lens)

  (struct game-object [input       ; Input   ; 'type game-object ke -> game-object
                       physics     ; Physics ; game-object world -> game-object
                       draw])      ; Draw    ; game-object scene -> scene

  (struct posn [x y] #:transparent)
  (define-struct-lenses posn)

  (struct velocity [x y] #:transparent)
  (define-struct-lenses velocity)

  )
