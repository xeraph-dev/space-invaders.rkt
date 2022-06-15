;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Space Invaders

;; Assets

(define WORLD-IMAGE (bitmap "assets/background.png"))
(define PLAYER-IMAGE (bitmap "assets/player.png"))
(define BULLET-IMAGE (bitmap "assets/bullet.png"))

;; Constants:

(define WORLD-WIDTH  (image-width  WORLD-IMAGE))
(define WORLD-HEIGHT (image-height WORLD-IMAGE))
(define WORLD-WIDTH/2  (/ WORLD-WIDTH  2))
(define WORLD-HEIGHT/2 (/ WORLD-HEIGHT 2))

(define PLAYER-WIDTH  (image-width  PLAYER-IMAGE))
(define PLAYER-HEIGHT (image-height PLAYER-IMAGE))
(define PLAYER-WIDTH/2  (/ PLAYER-WIDTH  2))
(define PLAYER-HEIGHT/2 (/ PLAYER-HEIGHT 2))
(define PLAYER-SPEED 5)

(define BULLET-WIDTH  (image-width  BULLET-IMAGE))
(define BULLET-HEIGHT (image-height BULLET-IMAGE))
(define BULLET-SPEED 5)

;; Structs
(define-struct world (close player bullets))

;; Functions:

(check-expect (world-close! (make-world false (make-posn 0 0) empty) true)
              (make-world true (make-posn 0 0) empty))

(define (world-close! ws c)
  (make-world c (world-player ws) (world-bullets ws)))

(check-expect (world-player! (make-world false (make-posn 0 0) empty) (make-posn 1 1))
              (make-world false (make-posn 1 1) empty))

(define (world-player! ws p)
  (make-world (world-close ws) p (world-bullets ws)))

(check-expect (world-bullets! (make-world false (make-posn 0 0) empty) empty)
              (make-world false (make-posn 0 0) empty))
(check-expect (world-bullets! (make-world false (make-posn 0 0) (list (make-posn 0 0))) empty)
              (make-world false (make-posn 0 0) empty))
(check-expect (world-bullets! (make-world false (make-posn 0 0) (list (make-posn 0 0)))
                              (list (make-posn 1 1)))
              (make-world false (make-posn 0 0) (list (make-posn 1 1))))

(define (world-bullets! ws bs)
  (make-world (world-close ws) (world-player ws) bs))

(check-expect (add-world-bullet! (make-world false (make-posn 0 0) empty) (make-posn 0 0))
              (make-world false (make-posn 0 0) (list (make-posn 0 0))))
(check-expect (add-world-bullet!
               (make-world false (make-posn 0 0) (list (make-posn 1 1)))
               (make-posn 0 0))
              (make-world false (make-posn 0 0) (list (make-posn 0 0) (make-posn 1 1))))

(define (add-world-bullet! ws b)
  (make-world (world-close ws) (world-player ws) (cons b (world-bullets ws))))

(check-expect (move-player! (make-world false (make-posn 0 0) empty) "test")
              (make-world false (make-posn 0 0) empty))
(check-expect (move-player! (make-world false (make-posn 0 0) empty) "left")
              (make-world false (make-posn (- PLAYER-SPEED) 0) empty))
(check-expect (move-player! (make-world false (make-posn 0 0) empty) "right")
              (make-world false (make-posn PLAYER-SPEED 0) empty))

(define (move-player! ws d)
  (world-player! ws (make-posn
                     (+ (posn-x (world-player ws))
                        (cond [(equal? d "left") (- PLAYER-SPEED)]
                              [(equal? d "right") PLAYER-SPEED]
                              [else 0]))
                     (posn-y (world-player ws)))))


(check-expect (restrict-player-move (make-world false (make-posn WORLD-WIDTH/2 0) empty) )
              (make-world false (make-posn WORLD-WIDTH/2 0) empty))
(check-expect (restrict-player-move (make-world false 
                                                (make-posn (add1 (- WORLD-WIDTH PLAYER-WIDTH/2)) 0) 
                                                empty))
              (make-world false (make-posn (- WORLD-WIDTH PLAYER-WIDTH/2) 0) empty))
(check-expect (restrict-player-move (make-world false 
                                                (make-posn (sub1 PLAYER-WIDTH/2) 0) 
                                                empty))
              (make-world false (make-posn PLAYER-WIDTH/2 0) empty))
(check-expect (restrict-player-move (make-world false (make-posn -5 0) empty))
              (make-world false (make-posn PLAYER-WIDTH/2 0) empty))
(check-expect (restrict-player-move (make-world false (make-posn (+ WORLD-WIDTH 5) 0) empty))
              (make-world false (make-posn (- WORLD-WIDTH PLAYER-WIDTH/2) 0) empty))

(define (restrict-player-move ws)
  (cond [(> (posn-x (world-player ws)) (- WORLD-WIDTH PLAYER-WIDTH/2))
         (world-player! ws (make-posn (- WORLD-WIDTH PLAYER-WIDTH/2) (posn-y (world-player ws))))]
        [(< (posn-x (world-player ws)) PLAYER-WIDTH/2) 
         (world-player! ws (make-posn PLAYER-WIDTH/2 (posn-y (world-player ws))))]
        [else ws]))

(check-expect (player-shot! (make-world false (make-posn 0 0) empty))
              (make-world false (make-posn 0 0) (list (make-posn 0 (- PLAYER-HEIGHT)))))
(check-expect (player-shot! (make-world false (make-posn 0 0) (list (make-posn 0 0))))
              (make-world false (make-posn 0 0) (list (make-posn 0 (- PLAYER-HEIGHT))
                                                      (make-posn 0 0))))

(define (player-shot! ws)
  (add-world-bullet! ws (make-posn
                         (posn-x (world-player ws))
                         (- (posn-y (world-player ws)) PLAYER-HEIGHT))))

(check-expect (close-game! (make-world false (make-posn 0 0) empty))
              (make-world true (make-posn 0 0) empty))

(define (close-game! ws)
  (world-close! ws true))


(check-expect (bullet-images empty) empty)
(check-expect (bullet-images (list (make-posn 0 0)))
              (list BULLET-IMAGE))
(check-expect (bullet-images (list (make-posn 0 0) (make-posn 0 0)))
              (list BULLET-IMAGE BULLET-IMAGE))

(define (bullet-images bs)
  (if (empty? bs) empty
      (cons BULLET-IMAGE (bullet-images (cdr bs)))))

(check-expect (bullet-positions empty) empty)
(check-expect (bullet-positions (list (make-posn 0 0)))
              (list (make-posn 0 0)))
(check-expect (bullet-positions (list (make-posn 0 0) (make-posn 0 0)))
              (list (make-posn 0 0) (make-posn 0 0)))

(define (bullet-positions bs)
  (if (empty? bs) empty
      (cons (car bs) (bullet-positions (cdr bs)))))

(check-expect (move-bullet! (make-posn 0 BULLET-SPEED))
              (make-posn 0 0))

(define (move-bullet! b)
  (make-posn (posn-x b) (- (posn-y b) BULLET-SPEED)))

(check-expect (move-bullets! (list (make-posn 0 BULLET-SPEED)))
              (list (make-posn 0 0)))

(define (move-bullets! bs)
  (if (empty? bs) empty
      (cons (move-bullet! (car bs)) (move-bullets! (cdr bs)))))

(check-expect (move-world-bullets!
               (make-world false (make-posn 0 0) (list (make-posn 0 BULLET-SPEED))))
              (make-world false (make-posn 0 0) (list (make-posn 0 0))))

(define (move-world-bullets! ws)
  (world-bullets! ws (move-bullets! (world-bullets ws))))

(check-expect (destroy-bullets! (list (make-posn 0 0)))
              (list (make-posn 0 0)))
(check-expect (destroy-bullets! (list (make-posn 0 0) (make-posn 0 (sub1 (- BULLET-HEIGHT)))))
              (list (make-posn 0 0)))
(check-expect (destroy-bullets! (list (make-posn 0 (sub1 (- BULLET-HEIGHT)))))
              empty)

(define (destroy-bullets! bs)
  (if (empty? bs) empty
      (if (> (posn-y (car bs)) (- BULLET-HEIGHT))
          (cons (car bs) (destroy-bullets! (cdr bs)))
          (destroy-bullets! (cdr bs)))))


(check-expect (destroy-world-bullets! (make-world false (make-posn 0 0) (list (make-posn 0 0))))
              (make-world false (make-posn 0 0) (list (make-posn 0 0))))
(check-expect (destroy-world-bullets! 
               (make-world false
                           (make-posn 0 0)
                           (list (make-posn 0 0) (make-posn 0 (sub1 (- BULLET-HEIGHT))))))
              (make-world false (make-posn 0 0) (list (make-posn 0 0))))
(check-expect (destroy-world-bullets!
               (make-world false
                           (make-posn 0 0)
                           (list (make-posn 0 (sub1 (- BULLET-HEIGHT))))))
              (make-world false (make-posn 0 0) empty))

(define (destroy-world-bullets! ws)
  (world-bullets! ws (destroy-bullets! (world-bullets ws))))

(define (render ws)
  (place-images
   (cons PLAYER-IMAGE (bullet-images (world-bullets ws)))
   (cons (world-player ws) (bullet-positions (world-bullets ws)))
   WORLD-IMAGE))

(define (handle-key ws k)
  (cond [(key=? k "q") (close-game! ws)]
        [(key=? k "left")  (restrict-player-move (move-player! ws "left"))]
        [(key=? k "right") (restrict-player-move (move-player! ws "right"))]
        [(key=? k " ") (player-shot! ws)]
        [else ws]))

(define (handle-tick ws)
  (destroy-world-bullets! (move-world-bullets! ws)))

;; Initial state

(define initial-player (make-posn WORLD-WIDTH/2 (- WORLD-HEIGHT PLAYER-HEIGHT/2)))
(define initial-world (make-world false initial-player empty))


;; Main

(define main
  (big-bang initial-world
    (name "Space Invaders")
    (to-draw render WORLD-WIDTH WORLD-HEIGHT)
    (on-key handle-key)
    (on-tick handle-tick)
    (close-on-stop true)
    (stop-when world-close)))