#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; Space Invaders

;; Assets

(define WORLD-IMAGE (bitmap "assets/background.png"))
(define PLAYER-IMAGE (bitmap "assets/player.png"))

;; Constants:

(define WORLD-WIDTH  (image-width WORLD-IMAGE))
(define WORLD-HEIGHT (image-height WORLD-IMAGE))
(define WORLD-WIDTH/2 (/ WORLD-WIDTH 2))
(define WORLD-HEIGHT/2 (/ WORLD-HEIGHT 2))

(define PLAYER-WIDTH (image-width PLAYER-IMAGE))
(define PLAYER-HEIGHT (image-height PLAYER-IMAGE))
(define PLAYER-WIDTH/2 (/ PLAYER-WIDTH 2))
(define PLAYER-HEIGHT/2 (/ PLAYER-HEIGHT 2))
(define PLAYER-SPEED 5)

;; Structs

(struct player (position) #:mutable)
(struct world (close player) #:mutable)

;; Functions:

(define (move-player-x! ws d)
  (let* ([p   (world-player ws)]
         [pos (player-position p)]
         [dx (cond [(equal? d "left") (- PLAYER-SPEED)]
                    [(equal? d "right") PLAYER-SPEED]
                    [else 0])]
         [nx (+ (posn-x pos) dx)]
         [dx (if (and (> nx PLAYER-WIDTH/2) (< nx (- WORLD-WIDTH PLAYER-WIDTH/2))) dx 0)]
         [nx (+ (posn-x pos) dx)]
         [x (set-posn-x! pos nx)])
    ws))

(define (close-game! ws)
  (let ([x (set-world-close! ws true)]) ws))


(define (render ws)
  (let ([p (world-player ws)])
    (place-images
     (list PLAYER-IMAGE) 
     (list (player-position p)) 
     WORLD-IMAGE)))


(define (handle-key ws k)
    (cond [(key=? k "q") (close-game! ws)]
          [(or (key=? k "left") (key=? k "a"))  (move-player-x! ws "left")]
          [(or (key=? k "right") (key=? k "d")) (move-player-x! ws "right")]
          [else ws]))



;; Initial state

(define initial-player-position (make-posn WORLD-WIDTH/2 (- WORLD-HEIGHT PLAYER-HEIGHT/2)))
(define initial-player (player initial-player-position))

(define initial-world (world false initial-player))


;; Main

(define main
  (big-bang initial-world
    (name "Space Invaders")
    (to-draw render)
    (on-key handle-key)
    (close-on-stop true)
    (stop-when world-close)))