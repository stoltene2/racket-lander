(module lander racket
  (provide (all-defined-out))
  (require 2htdp/universe
           2htdp/image
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

  (define lander-velocity-x-lens (lens-compose velocity-x-lens lander-v-lens))
  (define lander-velocity-y-lens (lens-compose velocity-y-lens lander-v-lens))

  (define lander-posn-x-lens (lens-compose posn-x-lens lander-posn-lens))
  (define lander-posn-y-lens (lens-compose posn-y-lens lander-posn-lens))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Input
  ;; type = 'key-up, 'key-down
  (define (lander-input type lander ke)
    (cond [(symbol=? type 'key-up) (lander-key-up lander ke)]
          [(symbol=? type 'key-down) (lander-key-down lander ke)]
          [else lander]))

  (define (lander-key-down l ke)
    (cond [(key=? ke "up") (lens-set lander-thrust?-lens l #t)]
          [(key=? ke "left") (lens-set lander-rotating-lens l "ccw")]
          [(key=? ke "right") (lens-set lander-rotating-lens l "cw")]
          [else l]))

  (define (lander-key-up l ke)
    (cond [(key=? ke "up") (lens-set lander-thrust?-lens l #f)]
          [(key=? ke "left") (lens-set lander-rotating-lens l "off")]
          [(key=? ke "right") (lens-set lander-rotating-lens l "off")]
          [else l]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Physics

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
