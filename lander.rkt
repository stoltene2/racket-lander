(module lander racket
  (provide (all-defined-out))
  (require 2htdp/image
           lens
           "game-object.rkt")

  (struct lander game-object [ thrust?   ; Thrusters on?
                               pitch     ; Angle in radians
                               rotating  ; "cw" | "ccw" | "off"
                               posn      ; Location in the world
                               angular-v ; Angular velocity
                               v         ; Lander velocity
                               ] #:transparent)
  (define-struct-lenses lander)

  (define lander-velocity-x (lens-compose velocity-x-lens lander-v-lens))
  (define lander-velocity-y (lens-compose velocity-y-lens lander-v-lens))

  (define lander-posn-x (lens-compose posn-x-lens lander-posn-lens))
  (define lander-posn-y (lens-compose posn-y-lens lander-posn-lens))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Drawing

  (define image/ufo  (bitmap/file "assets/ufo.png"))
  (define flame/orange-red (overlay/align "center" "bottom" (triangle 15 'solid 'orange) (triangle 30 'solid 'red)))
  (define image/ufo-thrust (overlay/offset image/ufo 0 20 flame/orange-red))

  (define (draw-lander l scene)
    (define p (lander-posn l))
    (define ship (if (lander-thrust? l)
                     image/ufo-thrust
                     image/ufo))

    ;; Rotates with degrees

    (place-image (rotate (lander-pitch l) ship)
                 (posn-x p)
                 (posn-y p)
                 scene))

  )
