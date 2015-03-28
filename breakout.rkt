#lang racket

(require 2htdp/image
         2htdp/universe)

(struct game-state (ball bricks score paddle) #:transparent)
(struct ball (position velcoity speed) #:transparent)
(struct brick (position dead?) #:transparent)
(struct posn (x y) #:transparent)
(struct paddle (position speed) #:transparent)

(define brick-width 64)
(define brick-height 32)

(define/contract (ball-in-brick? a-ball a-brick)
  (-> ball? brick? boolean?)
  (match-define (ball (posn ball-x ball-y) _ _) a-ball)
  (match-define (brick (posn brick-x brick-y) _) a-brick)
  (and (<= brick-x ball-x (+ brick-x brick-width))
       (<= brick-y ball-y (+ brick-y brick-height))))

(module+ test
  (require rackunit)
  (check-true (ball-in-brick? (ball (posn 5 5)  #f #f)
                              (brick (posn 0 0) #f)))
  (check-true (ball-in-brick? (ball (posn 5 5) #f #f)
                              (brick (posn 5 5) #f)))
  (check-false (ball-in-brick? (ball (posn 5 5) #f #f)
                               (brick (posn 6 6) #f))))

(define paddle-width 128)
(define paddle-height 32)

(define (distance position-1 position-2)
  (match-define (posn x1 y1) position-1)
  (match-define (posn x2 y2) position-2)
  (sqrt (expt (- x1 x2) 2)
        (expt (- y1 y2) 2)))

(define (closest-side a-posn a-brick)
  (match-define (brick (posn brick-x brick-y) _) a-brick)
  (define top-side-distance
    (/ (+ (distance a-posn (posn brick-x brick-y))
          (distance a-posn (posn (+ brick-x brick-width) brick-y)))
       2))
  (define bottom-side-distance
    (/ (+ (distance a-posn (posn brick-x (+ brick-y brick-height)))
          (distance a-posn (posn (+ brick-x brick-width) (+ brick-y brick-height))))
       2))
  (define left-side-distance
    (/ (+ (distance a-posn (posn brick-x brick-y))
          (distance a-posn (posn brick-x (+ brick-y brick-height))))
       2))
  (define right-side-distance
    (/ (+ (distance a-posn (posn (+ brick-x brick-width) brick-y))
          (distance a-posn (posn (+ brick-x brick-width) (+ brick-y brick-height))))
       2))
  (define min-distance
    (min top-side-distance bottom-side-distance
         left-side-distance right-side-distance))
  (cond [(= min-distance top-side-distance)
         'top]
        [(= min-distance bottom-side-distance)
         'bottom]
        [(= min-distance left-side-distance)
         'left]
        [(= min-distance right-side-distance)]))
  

;; Does nothing if the brick is not colliding.
;; Sets dead flag to true if brick is colliding.
(define (update-brick a-brick a-ball)
  (match-define (brick position dead?) a-brick)
  (brick position (or (ball-in-brick? a-ball a-brick)
                      dead?)))

(module+ test
  (check-true (brick-dead? (update-brick (brick (posn 0 0) #f)
                                         (ball (posn 1 1) #f #f))))
  (check-true (brick-dead? (update-brick (brick (posn 2 2) #t)
                                         (ball (posn 3 3) #f #f))))
  (check-false (brick-dead? (update-brick (brick (posn 3 3) #f)
                                          (ball (posn 2 2) #f #f)))))
  

;; Updates all bricks and removes any that are
;; dead.
(define (update-bricks bricks a-ball)
  (filter (negate brick-dead?)
          (for/list ([a-brick bricks])
            (update-brick a-brick a-ball))))

(module+ test
  (check-true (null? (update-bricks (list (brick (posn 0 0) #t)
                                          (brick (posn 32 0) #t)
                                          (brick (posn 65 0) #t))
                                    (ball (posn 100 100) #f #f))))
  (check-equal? 1 (length (update-bricks (list (brick (posn 0 0) #t)
                                               (brick (posn 32 0) #f))
                                         (ball (posn 100 100) #f #f)))))


;; TODO The ball only changes dy when it hits a brick. (But it looks fine! :D)
;; Moves ball a few units based on its velcoity
;; If ball is in a brick, change direction.
;; If ball is at edge of screen change direction.
;; If ball hits paddle, change direction.
(define (update-ball a-ball bricks a-paddle game-width game-height)
  (match-define (ball (posn ball-x ball-y) (posn ball-dx ball-dy) speed) a-ball)
  (match-define (paddle (posn paddle-x paddle-y) _) a-paddle)
  (define new-ball-dx
    (if (or (< ball-x 0) (> ball-x game-width))
        (- ball-dx)
        ball-dx))
  (define new-ball-dy
    (cond [(and (< paddle-x ball-x (+ paddle-x paddle-width))
                (< paddle-y ball-y (+ paddle-y paddle-height)))
           (- (abs ball-dy))]
          [(or (< ball-y 0) (> ball-y game-height))
           (- ball-dy)]
          [(ormap (curry ball-in-brick? a-ball) bricks)
           (- ball-dy)]
          [else ball-dy]))
  (ball (posn (+ ball-x (* new-ball-dx speed))
              (+ ball-y (* new-ball-dy speed)))
        (posn new-ball-dx new-ball-dy)
        speed))


(define brick-points 500)

;; Increments score if ball is collding with
;; a brick.
(define (update-score score a-ball bricks)
  (if (ormap (curry ball-in-brick? a-ball) bricks)
      (+ score brick-points)
      score))

(define game-width 384)
(define game-height 600)

(define (update-game-state a-game-state)
  (match-define (game-state a-ball bricks score a-paddle) a-game-state)
  (game-state (update-ball a-ball bricks a-paddle game-width game-height)
              (update-bricks bricks a-ball)
              (update-score score a-ball bricks)
              a-paddle))

(define draw-brick
  (let ([brick-image (rectangle brick-width brick-height 'solid 'blue)])
    (λ (a-brick scene)
      (match-define (brick (posn x y) _) a-brick)
      (underlay/align/offset 'left 'top scene x y brick-image))))

(define (draw-bricks bricks scene)
  (for/fold ([scene scene])
            ([a-brick bricks])
    (draw-brick a-brick scene)))

(define draw-paddle
  (let ([paddle-image (rectangle paddle-width paddle-height 'solid 'green)])
    (λ (a-paddle scene)
      (match-define (paddle (posn x y) _) a-paddle)
      (underlay/align/offset 'left 'top scene x y paddle-image))))

(define draw-ball
  (let ([ball-image (circle 8 'solid 'red)])
    (λ (a-ball scene)
      (match-define (ball (posn x y) _ _) a-ball)
      (underlay/align/offset 'left 'top scene x y ball-image))))

(define (draw-score score scene)
  (underlay/align 'left 'top scene
                  (text (format "Score: [~a]" score) 25 'purple)))

(define draw-game-state
  (let ([empty-scene (rectangle game-width game-height 'solid 'white)])
    (λ (a-game-state)
      (match-define (game-state a-ball bricks score a-paddle) a-game-state)
      (draw-score score
                  (draw-ball a-ball
                             (draw-paddle a-paddle
                                          (draw-bricks bricks empty-scene)))))))

(define (move-paddle a-paddle direction)
  (match-define (paddle (posn x y) speed) a-paddle)
  (cond [(eq? direction 'right)
         (define new-x (+ x speed))
         (paddle (posn (if (> (+ new-x paddle-width) game-width)
                           (- game-width paddle-width)
                           new-x)
                       y)
                 speed)]
        [(eq? direction 'left)
         (define new-x (- x speed))
         (paddle (posn (if (< 0 new-x)
                           0
                           new-x)
                       y)
                 speed)]))

(define (new-game-state)
  (game-state (ball (posn game-width game-height)
                    (posn (sqrt 2) (sqrt 2))
                    5)
              (for*/list ([i 6]
                          [j 8])
                (brick (posn (* i 64) (* j 32)) #f))
              0
              (paddle (posn 0 (- game-height paddle-height)) 10)))
  
(module+ main
  (big-bang (new-game-state)
            [name "Breakout"]
            [on-tick update-game-state]
            [to-draw draw-game-state]
            [on-key (λ (a-game-state key)
                      (match-define (game-state a-ball bricks score a-paddle) a-game-state)
                      (game-state a-ball
                                  bricks
                                  score
                                  (cond [(key=? key "left")
                                         (move-paddle a-paddle 'left)]
                                        [(key=? key "right")
                                         (move-paddle a-paddle 'right)]
                                        [else a-paddle])))]))
      
