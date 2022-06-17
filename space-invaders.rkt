;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require threading)

;; Space Invaders

;; Constants:

(define WORLD-IMAGE (bitmap "assets/background.png"))
(define WORLD-WIDTH  (image-width  WORLD-IMAGE))
(define WORLD-HEIGHT (image-height WORLD-IMAGE))
(define WORLD-WIDTH/2  (/ WORLD-WIDTH  2))
(define WORLD-HEIGHT/2 (/ WORLD-HEIGHT 2))

(define PLAYER-IMAGE (bitmap "assets/player.png"))
(define PLAYER-WIDTH  (image-width  PLAYER-IMAGE))
(define PLAYER-HEIGHT (image-height PLAYER-IMAGE))
(define PLAYER-WIDTH/2  (/ PLAYER-WIDTH  2))
(define PLAYER-HEIGHT/2 (/ PLAYER-HEIGHT 2))
(define PLAYER-SPEED 5)

(define BULLET-IMAGE (bitmap "assets/bullet.png"))
(define BULLET-WIDTH  (image-width  BULLET-IMAGE))
(define BULLET-HEIGHT (image-height BULLET-IMAGE))
(define BULLET-WIDTH/2  (/ BULLET-WIDTH  2))
(define BULLET-HEIGHT/2 (/ BULLET-HEIGHT 2))
(define BULLET-SPEED 5)

(define ENEMY-IMAGE (bitmap "assets/enemy.png"))
(define ENEMY-WIDTH  (image-width  ENEMY-IMAGE))
(define ENEMY-HEIGHT (image-height ENEMY-IMAGE))
(define ENEMY-WIDTH/2  (/ ENEMY-WIDTH  2))
(define ENEMY-HEIGHT/2 (/ ENEMY-HEIGHT 2))
(define ENEMY-SPEED 2)

;; Structs

(define-struct vel (x y))
(define-struct entity (pos vel))
(define-struct world (player bullets enemies level))


(define (new-enemies n)
  (if (= n 0) empty
      (cons (make-entity (make-posn (random WORLD-WIDTH) (- (random WORLD-HEIGHT)))
                         (make-vel ENEMY-SPEED ENEMY-SPEED))
            (new-enemies (sub1 n)))))
(define new-vel (make-vel 0 0))
(define new-player (make-entity (make-posn WORLD-WIDTH/2 (- WORLD-HEIGHT PLAYER-HEIGHT/2)) new-vel))
(define new-world (make-world new-player empty (new-enemies 5) 1))
(define (make-new-world ws win) 
  (make-world (world-player ws)
              empty
              (new-enemies (* (if win (add1 (world-level ws)) (world-level ws)) 5))
              (if win (add1 (world-level ws)) (world-level ws))))

;; Functions

(define (enemy-bullet-touching? e b)
  (< (sqrt (+ (sqr (- (posn-x e) (posn-x b))) (sqr (- (posn-y e) (posn-y b)))))
     (sqrt (+ (sqr ENEMY-WIDTH/2) (sqr ENEMY-HEIGHT/2)))))

(define (update-pos-x p x)
  (make-posn x (posn-y p)))

(define (update-pos-y p y)
  (make-posn (posn-x p) y))

(define (update-entity-pos e p)
  (make-entity p (entity-vel e)))

(define (update-entity-pos-x e x)
  (update-entity-pos e (update-pos-x (entity-pos e) x)))

(define (update-entity-pos-y e y)
  (update-entity-pos e (update-pos-y (entity-pos e) y)))

(define (update-vel-x v x)
  (make-vel x (vel-y v)))

(define (update-vel-y v y)
  (make-vel (vel-x v) y))

(define (update-entity-vel e v)
  (make-entity (entity-pos e) v))

(define (update-entity-vel-x e x)
  (update-entity-vel e (update-vel-x (entity-vel e) x)))

(define (update-entity-vel-y e y)
  (update-entity-vel e (update-vel-y (entity-vel e) y)))

(define (update-world-player ws p)
  (make-world p (world-bullets ws) (world-enemies ws) (world-level ws)))

(define (update-world-player-pos-x ws x)
  (make-world (update-entity-pos-x (world-player ws) x)
              (world-bullets ws)
              (world-enemies ws)
              (world-level ws)))

(define (update-world-player-pos-y ws y)
  (make-world (update-entity-pos-y (world-player ws) y)
              (world-bullets ws)
              (world-enemies ws)
              (world-level ws)))

(define (update-world-player-vel-x ws x)
  (make-world (update-entity-vel-x (world-player ws) x)
              (world-bullets ws)
              (world-enemies ws)
              (world-level ws)))

(define (update-world-player-vel-y ws y)
  (make-world (update-entity-vel-y (world-player ws) y)
              (world-bullets ws)
              (world-enemies ws)
              (world-level ws)))

(define (update-entity-pos+vel e)
  (update-entity-pos e (if (and (positive? (vel-x (entity-vel e))) (positive? (vel-y (entity-vel e))))
                           (make-posn (+ (posn-x (entity-pos e))
                                         (sqrt (+ (sqr (vel-x (entity-vel e))) 
                                                  (sqr (vel-y (entity-vel e))))))
                                      (+ (posn-y (entity-pos e))
                                         (sqrt (+ (sqr (vel-x (entity-vel e))) 
                                                  (sqr (vel-y (entity-vel e)))))))
                           (make-posn (entity-pos-x+vel-x e) (entity-pos-y+vel-y e)))))

(define (update-world-bullets ws bs)
  (make-world (world-player ws)
              bs
              (world-enemies ws)
              (world-level ws)))

(define (update-world-enemies ws es)
  (make-world (world-player ws)
              (world-bullets ws)
              es
              (world-level ws)))

(define (spawn-bullet p)
  (make-entity (make-posn (posn-x (entity-pos p))
                          (- (posn-y (entity-pos p)) PLAYER-HEIGHT))
               (make-vel 0 (- BULLET-SPEED))))

(define (spawn-world-bullet ws)
  (update-world-bullets ws (cons (spawn-bullet (world-player ws)) (world-bullets ws))))


(define (vel-x<0 v)
  (< (vel-x v) 0))

(define (vel-x>0 v)
  (> (vel-x v) 0))

(define (entity-vel-x>0 e)
  (vel-x>0 (entity-vel e)))

(define (entity-vel-x<0 e)
  (vel-x<0 (entity-vel e)))

(define (world-player-vel-x>0 ws)
  (entity-vel-x>0 (world-player ws)))

(define (world-player-vel-x<0 ws)
  (entity-vel-x<0 (world-player ws)))

(define (entity-pos-x+vel-x e)
  (+ (posn-x (entity-pos e)) (vel-x (entity-vel e))))

(define (entity-pos-y+vel-y e)
  (+ (posn-y (entity-pos e)) (vel-y (entity-vel e))))

(define (restrict-world-player-movement ws)
  (cond [(> (posn-x (entity-pos (world-player ws))) (- WORLD-WIDTH PLAYER-WIDTH/2))
         (update-world-player-pos-x ws (- WORLD-WIDTH PLAYER-WIDTH/2))]
        [(< (posn-x (entity-pos (world-player ws))) PLAYER-WIDTH/2)
         (update-world-player-pos-x ws PLAYER-WIDTH/2)]
        [else ws]))

(define (bullet-images bs)
  (if (empty? bs) empty
      (cons BULLET-IMAGE (bullet-images (cdr bs)))))

(define (entity-images xs i)
  (if (empty? xs) empty
      (cons )))

(define (bullet-positions bs)
  (if (empty? bs) empty
      (cons (entity-pos (car bs)) (bullet-positions (cdr bs)))))

(define (enemy-images es)
  (if (empty? es) empty
      (cons ENEMY-IMAGE (enemy-images (cdr es)))))

(define (enemy-positions es)
  (if (empty? es) empty
      (cons (entity-pos (car es)) (enemy-positions (cdr es)))))

(define (move-world-player ws)
  (update-world-player ws (update-entity-pos+vel (world-player ws))))

(define (move-bullets bs)
  (if (empty? bs) empty
      (cons (update-entity-pos+vel (car bs)) (move-bullets (cdr bs)))))

(define (move-world-bullets ws)
  (update-world-bullets ws (move-bullets (world-bullets ws))))

(define (destroy-bullets bs)
  (if (empty? bs) empty
      (if (> (posn-y (entity-pos (car bs))) (- BULLET-HEIGHT))
          (cons (car bs) (destroy-bullets (cdr bs)))
          (destroy-bullets (cdr bs)))))

(define (destroy-world-bullets ws)
  (update-world-bullets ws (destroy-bullets (world-bullets ws))))


(define (move-enemies es)
  (if (empty? es) empty
      (cons (update-entity-pos+vel (car es)) (move-enemies (cdr es)))))

(define (move-world-enemies ws)
  (update-world-enemies ws (move-enemies (world-enemies ws))))


(define (restrict-world-enemy-movement ws e)
  (cond [(> (posn-x (entity-pos e)) (- WORLD-WIDTH ENEMY-WIDTH/2))
         (make-entity (make-posn (- WORLD-WIDTH ENEMY-WIDTH/2) (posn-y (entity-pos e)))
                      (make-vel (- (vel-x (entity-vel e))) (vel-y (entity-vel e))))]
        [(< (posn-x (entity-pos e)) ENEMY-WIDTH/2)
         (make-entity (make-posn ENEMY-WIDTH/2 (posn-y (entity-pos e)))
                      (make-vel (- (vel-x (entity-vel e))) (vel-y (entity-vel e))))]
        [else e]))

(define (restrict-world-enemy-movements ws es)
  (if (empty? es) empty
      (cons (restrict-world-enemy-movement ws (car es)) 
            (restrict-world-enemy-movements ws (cdr es)))))

(define (restrict-world-enemies-movement ws)
  (update-world-enemies ws (restrict-world-enemy-movements ws (world-enemies ws))))


(define (destroy-bullets-touch-enemy ws bs)
  (if (empty? bs) empty
      (cons (car bs)
            (destroy-bullets-touch-enemy ws (cdr bs)))))

(define (enemy-touch-bullet? x xs)
  (if (empty? xs) false
      (or (enemy-bullet-touching? (entity-pos x) (entity-pos (car xs)))
          (enemy-touch-bullet? x (cdr xs)))))

(define (destroy-enemies-touch-bullets ws xs1 xs2)
  (if (empty? xs1) empty
      (if (enemy-touch-bullet? (car xs1) xs2)
          (destroy-enemies-touch-bullets ws (cdr xs1) xs2)
          (cons (car xs1) (destroy-enemies-touch-bullets ws (cdr xs1) xs2)))))

(define (destroy-world-bullets-touch-enemies ws)
  (make-world (world-player ws)
              (destroy-enemies-touch-bullets ws (world-bullets ws) (world-enemies ws))
              (destroy-enemies-touch-bullets ws (world-enemies ws) (world-bullets ws))
              (world-level ws)))

(define (enemy-is-in-end? es)
  (if (empty? es) false
      (or (>= (posn-y (entity-pos (car es))) (- WORLD-HEIGHT ENEMY-HEIGHT/2)) 
          (enemy-is-in-end? (cdr es)))))

(define (game-over? ws)
  (if (enemy-is-in-end? (world-enemies ws))
      (make-new-world ws false)
      ws))

(define (win-game? ws)
  (if (empty? (world-enemies ws))
      (make-new-world ws true)
      ws))


;;Big Bang functions

(define (render-ui ws)
  (text (string-append "Level " (number->string (world-level ws)) "\n"
                       "Enemies " (number->string (length (world-enemies ws))))
        18 "orange"))

(define (render ws)
  (place-images
   (cons PLAYER-IMAGE 
         (append (list (render-ui ws))
                 (bullet-images (world-bullets ws))
                 (enemy-images (world-enemies ws))))
   (cons (entity-pos (world-player ws)) 
         (append (list (make-posn (+ 5 (/ (image-width (render-ui ws)) 2))
                                  (+ 5 (/ (image-height (render-ui ws)) 2))))
                 (bullet-positions (world-bullets ws))
                 (enemy-positions (world-enemies ws))))
   WORLD-IMAGE))

(define (handle-key ws k)
  (cond [(key=? k "q") (stop-with ws)]
        [(key=? k "left")  (update-world-player-vel-x ws (- PLAYER-SPEED))]
        [(key=? k "right") (update-world-player-vel-x ws PLAYER-SPEED)]
        [(key=? k " ") (spawn-world-bullet ws)]
        [else ws]))

(define (handle-release ws k)
  (cond [(or (and (key=? k "left" ) (world-player-vel-x<0 ws))
             (and (key=? k "right") (world-player-vel-x>0 ws)))
         (update-world-player-vel-x ws 0)]
        [else ws]))

(define (handle-tick ws)
  (~> ws
      move-world-player
      restrict-world-player-movement
      move-world-bullets
      destroy-world-bullets
      move-world-enemies
      restrict-world-enemies-movement
      destroy-world-bullets-touch-enemies
      game-over?
      win-game?))

;; Main

(define main
  (big-bang new-world
    (name "Space Invaders")
    (to-draw render WORLD-WIDTH WORLD-HEIGHT)
    (on-key handle-key)
    (on-release handle-release)
    (on-tick handle-tick)
    (close-on-stop true)))