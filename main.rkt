#lang racket

(require racket/base
         2htdp/universe
         2htdp/image
         lens
         "physics.rkt")

(define WIDTH 1000)
(define HEIGHT 800)
(define TICK-RATE 1/28)
(define MAX-LANDING-V 20)
(define MAX-THRUST -35)

(define image/ufo  (bitmap/file "assets/ufo.png"))
(define flame/orange-red (overlay/align "center" "bottom" (triangle 15 'solid 'orange) (triangle 30 'solid 'red)))
(define image/ufo-thrust (overlay/offset image/ufo 0 20 flame/orange-red))

(define EMPTY-SCENE (empty-scene WIDTH HEIGHT))

(struct posn [x y] #:transparent)
(define-struct-lenses posn)

(struct velocity [x y] #:transparent)
(define-struct-lenses velocity)

(struct lander [thrust?   ; Thrusters on?
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

(struct world [time lander] #:transparent)
(define-struct-lenses world)

(define world-lander-thrust?-lens (lens-compose lander-thrust?-lens
                                                   world-lander-lens))

(define world-lander-rotating-lens (lens-compose lander-rotating-lens
                                                 world-lander-lens))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (INITIAL-WORLD) (world (current-milliseconds)
                             (lander #f ; Not thrusting
                                     (random -180 180) ; Initial pitch
                                     "off" ; Not rotating
                                     (posn (/ WIDTH 2) 10) ; Positioned near top
                                     0 ; Angular velocity
                                     (velocity (random -40 40)
                                               (random 20)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-world w)
  (define l (world-lander w))

  (define gravity 20)

  (define last-t (world-time w))
  (define current-t (current-milliseconds))

  (define p (lander-posn l))
  (define dt (/ (- current-t last-t) 1000.0))

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

  ;; Maybe not needed here since this should really be
  ;; radians. Degrees are needed for rendering the angle
  (define (deg->rad d)
    (/ (* 2 pi d) 360))

  (define thrust-x (* thrust (sin (deg->rad new-angle))))
  (define thrust-y (* thrust (cos (deg->rad new-angle))))

  (define ACCELERATION-y (+ gravity thrust-y))
  (define v0_y (lens-view lander-velocity-y l))
  (define v_y (delta-v v0_y ACCELERATION-y dt))
  (define dy (delta-p v0_y v_y dt))

  (define ACCELERATION-x thrust-x)
  (define v0_x (lens-view lander-velocity-x l))
  (define v_x (delta-v v0_x ACCELERATION-x dt))
  (define dx (delta-p v0_x v_x dt))

  (define new-posn (posn (+ (posn-x p) dx)
                         (+ (posn-y p) dy)))


  ;; Ideas
  ;; Use thrush+ so I don't need to keep updating things
  ;; (thrush+ w (λ (w) (lens-set world-time-lens w current-t))
  ;;            (λ (w) (lens-set world-lander-posn-lens w (velocity v_x v_y))))

  (world
   ;; Time
   current-t

   ;; Lander
   (lander (lander-thrust? l)
           new-angle
           (lander-rotating l)
           new-posn
           new-angular-v
           (velocity v_x v_y))))


(define (key-down w ke)
  (cond [(key=? ke "up") (lens-set world-lander-thrust?-lens w #t)]
        [(key=? ke "left") (lens-set world-lander-rotating-lens w "ccw")]
        [(key=? ke "right") (lens-set world-lander-rotating-lens w "cw")]
        [(key=? ke "r") (INITIAL-WORLD)]
        [else w]))

(define (key-up w ke)
  (cond [(key=? ke "up") (lens-set world-lander-thrust?-lens w #f)]
        [(key=? ke "left") (lens-set world-lander-rotating-lens w "off")]
        [(key=? ke "right") (lens-set world-lander-rotating-lens w "off")]
        [else w]))

(define (draw-world w)
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

  (define l (world-lander w))
  (define lander+scene (draw-lander l EMPTY-SCENE))

  (define (velocity+scene l scene)
    (define v_y (velocity-y (lander-v l)))
    (define v_x (velocity-x (lander-v l)))

    (define y-vel-string
      (format "Y vel: ~a"
              (real->decimal-string v_y 2)))

    (define x-vel-string
      (format "X vel: ~a"
              (real->decimal-string v_x 2)))

    (define angle-string
      (format "pitch: ~a" (real->decimal-string (lander-pitch l))))

    (define color (if (or (> v_y MAX-LANDING-V)
                          (> v_x MAX-LANDING-V))
                      "orange"
                      "green"))

    (overlay/align "left" "top"
                   (above (text y-vel-string 12 color)
                          (text x-vel-string 12 color)
                          (text angle-string 12 "blue")) scene))

  (velocity+scene l lander+scene))

(define (render-end w)
  (define lander (world-lander w))
  (define v (lander-v lander))
  (if (and (< (velocity-y v) MAX-LANDING-V)
           (< (velocity-x v) MAX-LANDING-V))
      (overlay (text "YOU WIN!!!" 50 "green") EMPTY-SCENE)
      (overlay (text "YOU LOSE!!!" 50 "red") EMPTY-SCENE)))

(define (dead? w)
  (define p (lander-posn (world-lander w)))
  (> (posn-y p) HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start)

  (big-bang (INITIAL-WORLD)
    (on-tick next-world TICK-RATE)
    (on-key key-down)
    (on-release key-up)
    (to-draw draw-world)
    (stop-when dead? render-end)))


;; TODO: I don't think this is working right now. I might need something inside of my info.rkt
;; raco test main.rkt
;; (module+ test
;;   (require rackunit)
;;   (check-equal? 2 3 "uh oh"))
