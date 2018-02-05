(module game-object racket
  (provide (all-defined-out))
  (struct game-object [input
                       physics
                       draw]))
