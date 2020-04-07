(use-modules (chickadee)
             (chickadee scripting)
             (chickadee math)
             (chickadee math vector)
             (chickadee math matrix)
             (chickadee math rect)
             (chickadee render font)
             (chickadee render sprite)
             (chickadee render texture)
             (system repl coop-server))

(define repl (spawn-coop-repl-server))

;; Constants
(define WINDOW-WIDTH 288)
(define WINDOW-HEIGHT 512)
(define GROUND-WIDTH 336)
(define GROUND-HEIGHT 112)
(define TUBE-WIDTH 52)
(define TUBE-HEIGHT 320)
(define BIRD-WIDTH 34)
(define BIRD-HEIGHT 24)

;; Textures
(define background-sprite #f)
(define flappy-bird-sprite #f)
(define ground-sprite #f)
(define tube-sprite #f)
(define flappy-bird-up-flap-tex #f)
(define flappy-bird-mid-flap-tex #f)
(define flappy-bird-down-flap-tex)

;; Mutables
(define flappy-bird-x (/ WINDOW-WIDTH 2.0))
(define flappy-bird-y (/ WINDOW-HEIGHT 2.0))
(define flappy-bird-drop-velocity 4.0)
(define flappy-bird-flap-velocity 50.0)
(define ground-x 0.0)
(define ground-x-offset GROUND-WIDTH)
(define ground-velocity 3.0)
(define tube-x-distance 250.0)
(define tube-y-distance 100.0)
(define paused #f)

;; Helpers
(define (reset-y-pos)
  (set-rect-y! flappy-bird-rect (/ WINDOW-HEIGHT 2.0)))

(define (generate-tube-rect-pair)
  (let ([offset (random GROUND-HEIGHT)])
    `(,(make-rect WINDOW-WIDTH (- offset) TUBE-WIDTH TUBE-HEIGHT)
      .
      ,(make-rect WINDOW-WIDTH (- (+ TUBE-HEIGHT tube-y-distance) offset)
                  TUBE-WIDTH TUBE-HEIGHT))))

(define (lastt lst)
  (if (= (length lst) 1)
      (car lst)
      (lastt (cdr lst))))

;; Declare Continuations
(define flap/c #f)

;; Create initial tube set
(define list-of-tube-rects `(,(generate-tube-rect-pair)))

(define (load)
  (set! background-sprite (load-image "./assets/sprites/background-day.png"))
  (set! ground-sprite (load-image "./assets/sprites/base.png"))
  (set! tube-sprite (load-image "./assets/sprites/pipe-green.png"))
  (set! flappy-bird-up-flap-tex (load-image "./assets/sprites/bluebird-upflap.png"))
  (set! flappy-bird-mid-flap-tex (load-image "./assets/sprites/bluebird-midflap.png"))
  (set! flappy-bird-down-flap-tex (load-image "./assets/sprites/bluebird-downflap.png"))
  (set! flappy-bird-sprite flappy-bird-up-flap-tex))


(define flappy-bird-rect (make-rect flappy-bird-x flappy-bird-y BIRD-WIDTH BIRD-HEIGHT))
(define ground-rect (make-rect 0 0 GROUND-WIDTH GROUND-HEIGHT))

(define (draw alpha)
  (draw-sprite background-sprite #v(0.0 0.0))
  
  ;; Draw tubes so they're behind the ground
  (let loop ([lst list-of-tube-rects])
    (if (null? lst)
        0
        (begin
          (draw-sprite tube-sprite #v((rect-x (caar lst)) (rect-y (caar lst))))
          (draw-sprite tube-sprite
                       #v((rect-x (cdar lst)) (+ TUBE-HEIGHT (rect-y (cdar lst))))
                       #:scale #v(1.0 -1.0))
          (loop (cdr lst)))))
  
  (draw-sprite ground-sprite #v(ground-x 0.0))
  (draw-sprite ground-sprite #v(ground-x-offset 0.0))
  (draw-sprite flappy-bird-sprite #v((rect-x flappy-bird-rect) (rect-y flappy-bird-rect))))

(define (update elapsed-time)
  (update-agenda (if paused 0 1))

  ;; Update REPL and Agenda
  (poll-coop-repl-server repl))

(define (handle-mouse-press button clicks x-pos y-pos)
  (if (eqv? button 'left)
      (flap/c)))

(define gravity 
  (script
   (forever
    (set-rect-y! flappy-bird-rect (- (rect-y flappy-bird-rect) flappy-bird-drop-velocity))
    (sleep 1))))

(define flap-bird
  (script
   (yield (lambda (c) (set! flap/c c)))
   (set! flappy-bird-sprite flappy-bird-down-flap-tex)
   (tween 10 (rect-y flappy-bird-rect) (+ flappy-bird-flap-velocity (rect-y flappy-bird-rect))
          (lambda (y)
            (set-rect-y! flappy-bird-rect y)))
   (set! flappy-bird-sprite flappy-bird-up-flap-tex)))

(define animate-ground
  (script
   (forever
    (begin
      ;; Tile Ground
      (if (<= ground-x (- GROUND-WIDTH))
          (set! ground-x (- WINDOW-WIDTH 10))
          (set! ground-x (- ground-x ground-velocity)))
      (if (<= ground-x-offset (- GROUND-WIDTH))
          (set! ground-x-offset (+ ground-x GROUND-WIDTH))
          (set! ground-x-offset (- ground-x-offset ground-velocity)))
      (sleep 1)))))

(define tube-generation
  (at 60
      (script
       (forever
        (begin (let loop ([lst list-of-tube-rects]) 
                 (if (null? lst)
                     0
                     (begin
                       (let ([tube-pair (car lst)]
                             [new-value (- (rect-x (caar lst)) ground-velocity)])
                         (if (<= new-value (- TUBE-WIDTH))
                             ;; remove offscreen tube from list
                             (begin 
                               (set! list-of-tube-rects (cdr lst))
                               (loop list-of-tube-rects))
                             ;; move tube over
                             (begin
                               (set-rect-x! (car tube-pair) new-value)
                               (set-rect-x! (cdr tube-pair) new-value)
                               (loop (cdr lst))))))))
               
               ;;Generate New Tubes
               (if (<= (rect-x (car (lastt list-of-tube-rects))) (- WINDOW-WIDTH tube-x-distance))
                   (append! list-of-tube-rects `(,(generate-tube-rect-pair))))

               ;; Check for collisions
               (let loop ([lor list-of-tube-rects])
                 (if (null? lor)
                     0
                     (if (or (rect-intersects? (caar lor) flappy-bird-rect)
                             (rect-intersects? (cdar lor) flappy-bird-rect)
                             (rect-intersects? ground-rect flappy-bird-rect))
                         (set! paused #t)
                         (loop (cdr lor)))))
               (sleep 1))))))

(run-game
 #:load load
 #:draw draw
 #:update update
 #:window-title "Flappy Bird!"
 #:window-width WINDOW-WIDTH
 #:window-height WINDOW-HEIGHT
 #:mouse-press handle-mouse-press)

