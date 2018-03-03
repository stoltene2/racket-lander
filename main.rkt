#lang racket

(require racket/base
         2htdp/universe
         2htdp/image
         lens
         "world.rkt"
         "game-object.rkt"
         "physics.rkt"
         "lander.rkt")

;; Use struct/lens
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Decorations - Things the player sees but cannot interact with.
Props - Things that can be touched and interacted with and usually visible.
Zones - Invisible areas the player interacts with. i.e. death zones, win zones, cut scenes.

TODO:
 - Add id to each game-object
 - Add posn and velocity to each game-object
 - Add an array of game objects
   - The HUD needs us to register the lander id so we can look the
     lander up from the game objects array.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

(struct ground game-object [posn] #:transparent)

(define world-ground (ground (λ (x) x) ; Input   ; 'type game-object ke -> game-object
                             (λ (x) x) ; Physics ; game-object world -> game-object
                             (λ (x scene) scene) ; Draw    ; game-object scene -> scene
                             (posn 0 40)))

(define WIDTH 1000)
(define HEIGHT 800)
(define TICK-RATE 1/28)
(define MAX-LANDING-V 20)


(define EMPTY-SCENE (empty-scene WIDTH HEIGHT))

(define last-time (current-milliseconds))

;; TODO These will go away when we have an ID
(define world-lander-thrust?-lens (lens-compose lander-thrust?-lens
                                                first-lens
                                                world-game-objects-lens))

(define world-lander-rotating-lens (lens-compose lander-rotating-lens
                                                 first-lens
                                                 world-game-objects-lens))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-lander) (lander lander-input          ; Lander input
                             lander-physics        ; Lander physics
                             draw-lander           ; Draw lander
                             #f                    ; Not thrusting
                             (random -180 180)     ; Initial pitch
                             "off"                 ; Not rotating
                             (posn (/ WIDTH 2) 10) ; Positioned near top
                             0                     ; Angular velocity
                             (velocity (random -40 40)
                                       (random 20))))

(define (INITIAL-WORLD) (world TICK-RATE ; 10 ms since last frame
                               (list (new-lander))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-world w)
  (define current-time (current-milliseconds))
  (define new-world (lens-set world-dt-lens w (/ (- current-time last-time) 1000.0)))
  (set! last-time current-time)

  ;; Note: I will need this when physics returns world instead of game-object
  ;; (define (apply-physics go w)
  ;;   (define physics-f (game-object-physics go))
  ;;   (physics-f go w))

  ;; (define new-new-world
  ;;   (foldl apply-physics w (world-game-objects w)))

  #|

  Maybe I should search all game objects for collisions, process them
  and then call physics on each one.

|#
  (lens-transform world-game-objects-lens new-world
                  (λ (game-objs)
                    (map (λ (go)
                           (define f (game-object-physics go))
                           (f go w))
                         game-objs))))


(define (world-key-down w ke)
  (cond [(key=? ke "r") (INITIAL-WORLD)]
        [else (lens-transform world-game-objects-lens w
                              (λ (game-objs)
                                (map (λ (go)
                                       ((game-object-input go) 'key-down go ke))
                                     game-objs)))]))

(define (world-key-up w ke)
  (lens-transform world-game-objects-lens w
                              (λ (game-objs)
                                (map (λ (go)
                                       ((game-object-input go) 'key-up go ke))
                                     game-objs))))

(define (draw-world w)
  ;; Change to get lander by ID from the world
  (define next-scene (foldl (λ (go scene)
                              ((game-object-draw go) go scene))
                            EMPTY-SCENE
                            (world-game-objects w)))

  (define (velocity+scene l scene)
    (define v_y (lens-view lander-velocity-y-lens l))
    (define v_x (lens-view lander-velocity-x-lens l))

    (define y-vel-string
      (format "Y vel: ~a" (real->decimal-string v_y 2)))

    (define x-vel-string
      (format "X vel: ~a" (real->decimal-string v_x 2)))

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

  ;;TODO: Fix this to look lander up by id
  (velocity+scene (first (world-game-objects w)) next-scene))

(define (render-end w)
  ;;TODO: Update to get lander by ID
  (define l (first (world-game-objects w)))
  (define v (lander-v l))
  (if (and (< (velocity-y v) MAX-LANDING-V)
           (< (velocity-x v) MAX-LANDING-V))
      (overlay (text "YOU WIN!!!" 50 "green") EMPTY-SCENE)
      (overlay (text "YOU LOSE!!!" 50 "red") EMPTY-SCENE)))

(define (dead? w)
  (define l (first (world-game-objects w)))
  (define p (lander-posn l))
  (> (posn-y p) HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start)

  (big-bang (INITIAL-WORLD)
    (on-tick next-world TICK-RATE)
    (on-key world-key-down)
    (on-release world-key-up)
    (to-draw draw-world)
    (stop-when dead? render-end)))
