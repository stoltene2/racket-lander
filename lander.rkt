(module lander racket
  (provide (all-defined-out))
  (require 2htdp/universe
           2htdp/image
           lens
           "world.rkt"
           "physics.rkt"
           "game-object.rkt")

  (define MAX-THRUST -250)

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
  (define (lander-input type l ke)
    (cond [(symbol=? type 'key-up) (lander-key-up l ke)]
          [(symbol=? type 'key-down) (lander-key-down l ke)]
          [else l]))

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

  (define (lander-physics l w)
    (define gravity 20)
    (define p (lander-posn l))
    (define dt (world-dt w))

    (define thrust (if (lander-thrust? l)
                       MAX-THRUST
                       0))

    (define ACCELERATION-omega
      (cond [(string=? (lander-rotating l) "ccw") (* 2 pi)]
            [(string=? (lander-rotating l) "cw") (- (* 2 pi))]
            [else 0]))

    (define new-angular-v (lander-angular-v l))
    (define new-angle (+ (lander-pitch l)
                         ACCELERATION-omega))

    (define thrust-x (* thrust (sin (degrees->radians new-angle))))
    (define thrust-y (* thrust (cos (degrees->radians new-angle))))

    (define ACCELERATION-y (+ gravity thrust-y))
    (define v0_y (lens-view lander-velocity-y-lens l))
    (define v_y (delta-v v0_y ACCELERATION-y dt))
    (define dy (delta-p v0_y v_y dt))

    (define ACCELERATION-x thrust-x)
    (define v0_x (lens-view lander-velocity-x-lens l))
    (define v_x (delta-v v0_x ACCELERATION-x dt))
    (define dx (delta-p v0_x v_x dt))

    (define new-posn (posn (+ (posn-x p) dx)
                           (+ (posn-y p) dy)))

    (lander (game-object-input l)
            (game-object-physics l)
            (game-object-draw l)
            (lander-thrust? l)
            new-angle
            (lander-rotating l)
            new-posn
            new-angular-v
            (velocity v_x v_y))
    )
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
