(module world racket
  (provide (all-defined-out))
  (require lens)

  (struct world [dt            ; Time since last frame
                 game-objects  ; Lander object
                 ] #:transparent)

  (define-struct-lenses world)



  )
