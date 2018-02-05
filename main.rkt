#lang racket

(require racket/base
         2htdp/universe
         2htdp/image
         lens
         "game-object.rkt"
         "physics.rkt"
         "lander.rkt")


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Decorations - Things the player sees but cannot interact with.
Props - Things that can be touched and interacted with and usually visible.
Zones - Invisible areas the player interacts with. i.e. death zones, win zones, cut scenes.

TODO:
 - Add id to each game-object
 - Add posn and velocity to each game-object

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
(define WIDTH 1000)
(define HEIGHT 800)
(define TICK-RATE 1/28)
(define MAX-LANDING-V 20)
(define MAX-THRUST -35)

(define EMPTY-SCENE (empty-scene WIDTH HEIGHT))

(struct world [time lander] #:transparent)
(define-struct-lenses world)

(define world-lander-thrust?-lens (lens-compose lander-thrust?-lens
                                                   world-lander-lens))

(define world-lander-rotating-lens (lens-compose lander-rotating-lens
                                                 world-lander-lens))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (INITIAL-WORLD) (world (current-milliseconds)
                               (lander lander-input ;Lander input
                                       (λ (x) x) ;Lander physics
                                       draw-lander
                                       #f ; Not thrusting
                                       (random -180 180) ; Initial pitch
                                       "off" ; Not rotating
                                       (posn (/ WIDTH 2) 10) ; Positioned near top
                                       0 ; Angular velocity
                                       (velocity (random -40 40)
                                                 (random 20))

                                       )))


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
    (cond [(string=? (lander-rotating l) "ccw") (* 4 pi)]
          [(string=? (lander-rotating l) "cw") (- (* 4 pi))]
          [else 0]))

  (define new-angular-v (lander-angular-v l))
  (define new-angle (+ (lander-pitch l)
                       ACCELERATION-omega))

  (define thrust-x (* thrust (sin (degrees->radians new-angle))))
  (define thrust-y (* thrust (cos (degrees->radians new-angle))))

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
   (lander (game-object-input l)
           (game-object-physics l)
           (game-object-draw l)
           (lander-thrust? l)
           new-angle
           (lander-rotating l)
           new-posn
           new-angular-v
           (velocity v_x v_y))))


(define (world-key-down w ke)
  (define l (world-lander w))
  (define lander-f (game-object-input (world-lander w)))
  (cond [(key=? ke "r") (INITIAL-WORLD)]
        [else (lens-set world-lander-lens w (lander-f 'key-down l ke))]))

(define (world-key-up w ke)
  (define l (world-lander w))
  (define lander-f (game-object-input (world-lander w)))
  (lens-set world-lander-lens w (lander-f 'key-up l ke)))

(define (draw-world w)
  (define l (world-lander w))
  (define lander+scene ((game-object-draw l) l EMPTY-SCENE))

  (define (velocity+scene l scene)
    (define v (velocity-y (lander-v l)))

    (define vel-string
      (format "vel: ~a"
              (real->decimal-string v 2)))

    (define angle-string
      (format "pitch: ~a" (real->decimal-string (lander-pitch l))))
    (define color (if (> v MAX-LANDING-V)
                      "orange"
                      "green"))

    (overlay/align "left" "top"
                   (above (text vel-string 12 color)
                          (text angle-string 12 "blue")) scene))

  (if (lens-view world-lander-thrust?-lens w)
      (overlay/align "right" "top"
                     (text "THRUST" 30 "red") (velocity+scene l lander+scene))
      (overlay/align "right" "top"
                     (text "not thrusting" 20 "black") (velocity+scene l lander+scene))))

(define (render-end w)
  (define lander (world-lander w))
  (define v (lander-v lander))
  (if (< (velocity-y v) MAX-LANDING-V)
      (overlay (text "YOU WIN!!!" 50 "green") EMPTY-SCENE)
      (overlay (text "YOU LOSE!!!" 50 "red") EMPTY-SCENE)))

(define (dead? w)
  (define p (lander-posn (world-lander w)))
  (> (posn-y p) HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start)

  (big-bang (INITIAL-WORLD)
    (on-tick next-world TICK-RATE)
    (on-key world-key-down)
    (on-release world-key-up)
    (to-draw draw-world)
    (stop-when dead? render-end)))
