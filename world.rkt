(module world racket
  (provide (all-defined-out))
  (require lens)

  (struct world [dt      ; Time since last frame
                 lander  ; Lander object
                 ] #:transparent)

  (define-struct-lenses world)



  )
