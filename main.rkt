#lang racket

(require racket/base
         2htdp/universe
         2htdp/image
         struct-update)


(define WIDTH 1000)
(define HEIGHT 800)
(define TICK-RATE 1/30)

(define EMPTY-SCENE (empty-scene WIDTH HEIGHT))

(struct world [thrust?] #:transparent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-world w)
  w)

(define (key-down w ke)
  (cond [(key=? ke "up") (world #t)]
        [else w]))

(define (key-up w ke)
  (cond [(key=? ke "up") (world #f)]
        [else w]))

(define (draw-world w)
  (if (world-thrust? w)
      (overlay (text "THRUST" 30 "red") EMPTY-SCENE)
      (overlay (text "not thrusting" 20 "black") EMPTY-SCENE)))

(define (render-end w) EMPTY-SCENE)

(define (dead? w) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (start)
  (define initial-world (world #f))

  (big-bang initial-world
    (on-tick next-world TICK-RATE)
    (on-key key-down)
    (on-release key-up)
    (to-draw draw-world)
    (stop-when dead? render-end)))
