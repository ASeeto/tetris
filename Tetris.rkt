;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ((lib "guess.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "guess.ss" "teachpack" "htdp")))))
;;+---------------------------------------+
;;|                                       |
;;|  Fundamentals of Computer Science 1   |
;;|        Assignment 9 - TETRIS          |
;;|                                       |
;;|           Alexander Seeto             |
;;|             Henry Duong               |
;;|                                       |
;;+---------------------------------------+

(require 2htdp/image)
(require 2htdp/universe)

;;----------------- PROBLEM A1 ------------------------

;; A Block is a (make-block Number Number Color)
(define-struct block (x y color))

;; A Tetra is a (make-tetra Posn BSet)
;; The center point is the point around which the tetra rotates
;; when it spins.
(define-struct tetra (center blocks))

;; A Set of Blocks (BSet) is one of:
;; - empty
;; - (cons Block BSet)
;; Order does not matter.  Repetitions are NOT allowed.

;; A World is a (make-world Tetra BSet Number)
;; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile score))


;;************* Constants  ***********************

(define CELL-SIZE 20)
(define CELL-SIZE/2 (/ CELL-SIZE 2))
(define GRID-WIDTH 10)
(define GRID-HEIGHT 20)

(define SCREEN-WIDTH (* CELL-SIZE GRID-WIDTH))
(define SCREEN-HEIGHT (* CELL-SIZE GRID-HEIGHT))

(define B-START-X (floor (/ GRID-WIDTH 2)))
(define B-START-Y -2)

(define EMPTY-BOARD (empty-scene SCREEN-WIDTH SCREEN-HEIGHT))

;; Tetras
(define O
 (make-tetra  (make-posn (- B-START-X .5) (+ B-START-Y .5))
              (list (make-block    B-START-X       B-START-Y    "green")
                    (make-block (- B-START-X 1) (+ B-START-Y 1) "green")
                    (make-block    B-START-X    (+ B-START-Y 1) "green")
                    (make-block (- B-START-X 1)    B-START-Y    "green"))))
(define I
 (make-tetra  (make-posn B-START-X B-START-Y)
              (list (make-block    B-START-X    B-START-Y "blue")
                    (make-block (+ B-START-X 1) B-START-Y "blue")
                    (make-block (- B-START-X 1) B-START-Y "blue")
                    (make-block (- B-START-X 2) B-START-Y "blue"))))
(define L
 (make-tetra (make-posn B-START-X B-START-Y)
             (list (make-block    B-START-X       B-START-Y    "purple")
                   (make-block    B-START-X    (+ B-START-Y 1) "purple")
                   (make-block (- B-START-X 1)    B-START-Y    "purple")
                   (make-block (- B-START-X 2)    B-START-Y    "purple"))))
(define J
 (make-tetra (make-posn B-START-X B-START-Y)
             (list (make-block B-START-X B-START-Y "aqua")
                   (make-block (- B-START-X 1) (+ B-START-Y 1) "aqua")
                   (make-block (+ B-START-X 1)    B-START-Y    "aqua")
                   (make-block (- B-START-X 1)    B-START-Y    "aqua"))))
(define T
 (make-tetra  (make-posn B-START-X B-START-Y)
              (list (make-block    B-START-X      B-START-Y    "orange")
                    (make-block    B-START-X   (+ B-START-Y 1) "orange")
                    (make-block (- B-START-X 1)   B-START-Y    "orange")
                    (make-block (+ B-START-X 1)   B-START-Y    "orange"))))
(define Z
 (make-tetra (make-posn B-START-X B-START-Y)
             (list (make-block    B-START-X       B-START-Y    "pink")
                   (make-block    B-START-X    (+ B-START-Y 1) "pink")
                   (make-block (+ B-START-X 1) (+ B-START-Y 1) "pink")
                   (make-block (- B-START-X 1)    B-START-Y    "pink"))))

(define S
 (make-tetra (make-posn B-START-X B-START-Y)
             (list (make-block    B-START-X       B-START-Y    "red")
                   (make-block    B-START-X    (+ B-START-Y 1) "red")
                   (make-block (+ B-START-X 1)    B-START-Y    "red")
                   (make-block (- B-START-X 1) (+ B-START-Y 1) "red"))))


;;*************  Examples  ***********************
(define world1 (make-world I empty 0))
(define random-center (make-posn 5 5))
(define bset1 empty)
(define bset2 empty)
(define bset3 (cons (make-block 6 1 "blue")
                               empty))
(define bset4 (list (make-block 8 1 "blue")
                    (make-block 7 1 "blue")
                    (make-block 5 1 "blue")))
(define bset5 (list (make-block 8 1 "blue")
                    (make-block 7 1 "blue")
                    (make-block 5 1 "blue")
                    (make-block 4 1 "blue")))
(define bset6 (list (make-block 0 1 "blue")
                    (make-block 1 1 "blue")
                    (make-block 2 1 "blue")
                    (make-block 3 1 "blue")
                    (make-block 4 1 "blue")
                    (make-block 5 1 "blue")
                    (make-block 6 1 "blue")
                    (make-block 7 1 "blue")
                    (make-block 8 1 "blue")
                    (make-block 9 1 "blue")))


;;*************  Conversion Functions  ***********

;; cell->pixel : Number -> Number
;; Converts a grid X or Y coord to a screen X or Y coord
(define (cell->pixel n)
 (+ (* n CELL-SIZE) CELL-SIZE/2))
;; Tests
(check-expect (cell->pixel 0) CELL-SIZE/2)
(check-expect (cell->pixel 3) (+ (* 3 CELL-SIZE) CELL-SIZE/2))


;;*************  Rendering  **********************

;; img+scene : Image Number Number Scene -> Scene
;; Draws the image with the given x and y in the scene
(define (img+scene img x y scn)
 (place-image img
              (cell->pixel x)
              (cell->pixel y)
              scn))
;; Tests
(check-expect (img+scene (square 10 "solid" "blue")
                        10
                        10
                        (empty-scene 300 300))
             (place-image (square 10 "solid" "blue")
                          (+ (* 10 CELL-SIZE) CELL-SIZE/2)
                          (+ (* 10 CELL-SIZE) CELL-SIZE/2)
                          (empty-scene 300 300)))
(check-expect (img+scene (circle 50 "solid" "blue")
                        5
                        5
                        (empty-scene 300 300))
             (place-image (circle 50 "solid" "blue")
                          (+ (* 5 CELL-SIZE) CELL-SIZE/2)
                          (+ (* 5 CELL-SIZE) CELL-SIZE/2)
                          (empty-scene 300 300)))

;; scene+block : Block Scene -> Scene
;; Draws the block in the scene
(define (scene+block blk scn)
 (img+scene (overlay
             (square CELL-SIZE "outline" "black")
             (square CELL-SIZE "solid" (block-color blk)))
            (block-x blk)
            (block-y blk)
            scn))
;; Tests
(check-expect (scene+block (make-block 40 60 "blue")
                          (empty-scene 100 100))
             (img+scene (square CELL-SIZE "solid" "blue")
                        40
                        60
                        (empty-scene 100 100)))
(check-expect (scene+block (make-block 150 150 "red")
                          (empty-scene 300 300))
             (img+scene (square CELL-SIZE "solid" "red")
                        150
                        150
                        (empty-scene 300 300)))

;; scene+bset : BSet Scene -> Scene
;; Draws the BSet in the Scene
(define (scene+bset bset scn)
  (foldr scene+block scn bset))
;; Tests
(check-expect (scene+bset empty (empty-scene 100 100))
             (empty-scene 100 100))
(check-expect (scene+bset (cons (make-block 5 5 "blue")
                               (cons
                                (make-block 3 3 "red")
                                empty))
                         (empty-scene 100 100))
             (scene+block (make-block 5 5 "blue")
                          (scene+block (make-block 3 3 "red")
                                       (empty-scene 100 100))))

;; scene+tetra : Tetra Scene -> Scene
;; Draws the Tetra in the Scene
(define (scene+tetra t scn)
 (scene+bset (tetra-blocks t) scn))
;; Tests
(check-expect (scene+tetra O (empty-scene 100 100))
             (scene+bset (tetra-blocks O) (empty-scene 100 100)))
(check-expect (scene+tetra L (empty-scene 300 300))
             (scene+bset (tetra-blocks L) (empty-scene 300 300)))

;; score+scene : Number Scene -> Scene
;; Draws the score in the Scene
(define (score+scene score scn)
 (place-image (text (number->string score) 20 "black")
              30 
              40 
              scn))
;; Tests
(check-expect (score+scene 4 (empty-scene 300 300))
              (place-image (text (number->string 4) 20 "black")
                           30 
                           40 
                           (empty-scene 300 300)))
(check-expect (score+scene 48 (empty-scene 150 150))
              (place-image (text (number->string 48) 20 "black")
                           30 
                           40 
                           (empty-scene 150 150)))
              
;; world->scene : World -> Scene
;; Draws the world in the scene
(define (world->scene w)
 (score+scene (world-score w)
              (scene+bset (world-pile w)
                          (scene+tetra (world-tetra w)
                                       EMPTY-BOARD))))
;; Tests
(check-expect (world->scene (make-world O empty 0))
             (score+scene 0
                          (scene+bset empty
                                      (scene+tetra O
                                                   EMPTY-BOARD))))
(check-expect (world->scene (make-world L
                                       (cons (make-block 5 5 "blue")
                                               empty)
                                       1))
             (score+scene 1
                          (scene+bset (cons (make-block 5 5 "blue")
                                            empty)
                                      (scene+tetra L
                                                   EMPTY-BOARD))))

;;;************  Moving  *************************

;; move-center : Posn Number Number -> Posn
;; Moves the center according to the given numbers,
;; 1st num changes x and 2nd num changes y
(define (move-center p nx ny)
 (make-posn (+ (posn-x p) nx)
            (+ (posn-y p) ny)))
;; Tests
(check-expect (move-center (make-posn 3 3) 1 1)
             (make-posn 4 4))
(check-expect (move-center (make-posn 3 3) -1 -1)
             (make-posn 2 2))

;; move-b : Block Number Number -> Block
;; Moves the block left, right, or down by one grid point
(define (move-b b x y)
  (make-block (+ (block-x b) x)
              (+ (block-y b) y)
              (block-color b)))
;; Tests
(check-expect (move-b (make-block 0 0 "blue") 1 1)
             (make-block 1 1 "blue"))
(check-expect (move-b (make-block 0 0 "blue") 0 0)
             (make-block 0 0 "blue"))

;; move-bset : BSet Number Number -> BSet
;; Moves the bset left, right or down by one grid point
(define (move-bset bset x y)
 (map (lambda (b) (move-b b x y)) bset))
;; Tests
(check-expect (move-bset (list (make-block 0 0 "blue")
                               (make-block 1 2 "red" ))
                         1 1)
             (list (make-block 1 1 "blue")
                   (make-block 2 3 "red")))
(check-expect (move-bset empty 0 0)
              empty)

;; move-tetra : Tetra Number Number -> Tetra
;; Moves the tetra left, right or down by one grid point
(define (move-tetra t x y)
 (make-tetra (move-center (tetra-center t) x y)
             (move-bset (tetra-blocks t) x y)))
;; Tests
(check-expect (move-tetra O -1 0)
             (make-tetra (move-center (tetra-center O) -1 0)
                         (move-bset (tetra-blocks O) -1 0)))
(check-expect (move-tetra L -1 0)
             (make-tetra (move-center (tetra-center L) -1 0)
                         (move-bset (tetra-blocks L) -1 0)))

;;*************  Rotating  ***********************

;; block-rotate : Posn Block Keyevent
;; Rotates the block 90 degrees counter or clockwise
(define (block-rotate c b ke)
  (cond
    [(key=? "s" ke)
     (make-block (+ (posn-x c) (- (posn-y c) (block-y b)))
                 (+ (posn-y c) (- (block-x b) (posn-x c)))
                 (block-color b))]
    [else
     (make-block (+ (posn-x c) (- (block-y b) (posn-y c)))
                 (+ (posn-y c) (- (posn-x c) (block-x b)))
                 (block-color b))]))
;; Tests
(check-expect (block-rotate random-center (make-block 10 10 "blue") "a")
              (make-block 10 0 "blue"))
(check-expect (block-rotate random-center (make-block 8 0 "blue") "s")
              (make-block 10 8 "blue"))

;; bset-rotate : Posn BSet Keyevent -> BSet
;; Rotates the bset 90 degrees cw or ccw around the posn.
(define (bset-rotate c bset ke)
  (map (lambda (b) (block-rotate c b ke)) bset))
;; Tests
(check-expect (bset-rotate random-center 
                           (list (make-block 10 10 "blue")
                                 (make-block 10 5  "blue"))
                           "a")
              (list (make-block 10 0 "blue")
                    (make-block 5 0 "blue")))
(check-expect (bset-rotate random-center 
                           (list (make-block 6 0  "blue")
                                 (make-block 5 0  "blue"))
                           "s")
              (list (make-block 10 6 "blue")
                    (make-block 10 5 "blue")))


;; tetra-rotate : Tetra Keyevent -> Tetra
;; Rotates the tetra 90 degrees cw or ccw.
(define (tetra-rotate t ke)
 (make-tetra (tetra-center t)
             (bset-rotate (tetra-center t)(tetra-blocks t) ke)))
;; Tests
(check-expect (tetra-rotate (make-tetra random-center
                                        (list (make-block 8 0 "blue")
                                              (make-block 7 0  "blue")))
                            "s") 
              (make-tetra random-center
                          (list (make-block 10 8 "blue")
                                (make-block 10 7 "blue"))))
(check-expect (tetra-rotate (make-tetra random-center
                                            (list (make-block 10 10 "blue")
                                                  (make-block 10 5  "blue")))
                                "a") 
              (make-tetra random-center
                          (list (make-block 10 0 "blue")
                                (make-block 5 0 "blue"))))

;;*************  Collisions  *********************

;;; OUT OF GRID
;; in-grid? : Block -> Boolean
;; Is the block out of the grid?
(define (in-grid? b)
 (or (> (cell->pixel (block-x b))
        SCREEN-WIDTH)
     (< (cell->pixel (block-x b))
        0)
     (> (cell->pixel (block-y b))
        SCREEN-HEIGHT)))
;; Tests
(check-expect (in-grid? (make-block 0 0 "blue"))
              false)
(check-expect (in-grid? (make-block -1 0 "blue"))
              true)
(check-expect (in-grid? (make-block (+ 1 SCREEN-WIDTH) 0 "blue"))
              true)
(check-expect (in-grid? (make-block 0 (+ 1 SCREEN-HEIGHT) "blue"))
              true)

;; block-out-of-grid? : Block Posn Keyevent -> Boolean
;; Will the next move, move the block out of the grid?
(define (block-out-of-grid? b c ke)
 (cond
   [(key=? ke "down")
    (in-grid? (move-b b 0 1))]
   [(key=? ke "left")
    (in-grid? (move-b b -1 0))]
   [(key=? ke "right")
    (in-grid? (move-b b 1 0))]
   [(key=? ke "a")
    (in-grid? (block-rotate c b ke))]
   [(key=? ke "s")
    (in-grid? (block-rotate c b ke))]))
;; Tests
(check-expect (block-out-of-grid? (make-block (+ 1 SCREEN-HEIGHT) 0 "blue")
                                  (make-posn 1 1)
                                  "down")
              (in-grid? (move-b 
                         (make-block (+ 1 SCREEN-HEIGHT) 0 "blue")
                         0 1)))

(check-expect (block-out-of-grid? (make-block 0 0 "blue")
                                  (make-posn 1 1)
                                  "down")
              (in-grid? (move-b
                         (make-block 0 0 "blue")
                         0 1)))

(check-expect (block-out-of-grid? (make-block 0 0 "blue")
                                  (make-posn 1 1)
                                  "left")
              (in-grid? (move-b
                         (make-block 0 0 "blue")
                         -1 0)))

(check-expect (block-out-of-grid? (make-block 1 0 "blue")
                                  (make-posn 1 1)
                                  "left")
              (in-grid? (move-b 
                         (make-block 1 0 "blue")
                         -1 0)))

(check-expect (block-out-of-grid? (make-block (+ 1 SCREEN-WIDTH) 0 "blue")
                                  (make-posn 1 1)
                                  "right")
              (in-grid? (move-b
                         (make-block (+ 1 SCREEN-WIDTH) 0 "blue")
                         1 0)))

(check-expect (block-out-of-grid? (make-block 0 0 "blue")
                                  (make-posn 1 1)
                                  "right")
              (in-grid? (move-b
                         (make-block 0 0 "blue")
                         1 0)))

(check-expect (block-out-of-grid? (make-block 1 0 "blue")
                                  (make-posn 1 2)
                                  "a")
              (in-grid? (block-rotate
                         (make-posn 1 2)
                         (make-block 1 0 "blue")
                         "a")))
                                  
(check-expect (block-out-of-grid? (make-block 1 4 "blue")
                                  (make-posn 1 1)
                                  "a")
              (in-grid? (block-rotate
                         (make-posn 1 1)
                         (make-block 1 4 "blue")
                         "a")))

(check-expect (block-out-of-grid? (make-block 1 4 "blue")
                                  (make-posn 1 1)
                                  "s")
              (in-grid? (block-rotate
                         (make-posn 1 1)
                         (make-block 1 4 "blue")
                         "s")))

(check-expect (block-out-of-grid? (make-block 1 0 "blue")
                                  (make-posn 1 1)
                                  "s")
              (in-grid? (block-rotate
                         (make-posn 1 1)
                         (make-block 1 0 "blue")
                         "s")))

;; bset-out-of-grid? : BSet Posn Keyevent -> Boolean
;; Will the next move, move the bset out of the grid?
(define (bset-out-of-grid? bset c ke)
   (ormap (lambda (b) (block-out-of-grid? b c ke)) bset))
;; Tests
(check-expect (bset-out-of-grid? empty
                                 (make-posn 1 1)
                                 "down")
              false)

(check-expect (bset-out-of-grid? 
               (list (make-block (+ 1 SCREEN-HEIGHT) 0 "blue"))
                     (make-posn 1 1)
                     "down")
              (block-out-of-grid? (make-block (+ 1 SCREEN-HEIGHT) 0 "blue")
                                  (make-posn 1 1)
                                  "down"))

(check-expect (bset-out-of-grid? 
               (list (make-block 0 0 "blue"))
                     (make-posn 1 1)
                     "down")
              (block-out-of-grid? (make-block 0 0 "blue")
                                  (make-posn 1 1)
                                  "down"))

(check-expect (bset-out-of-grid? 
               (list (make-block 0 0 "blue"))
                     (make-posn 1 1)
                     "left")
              (block-out-of-grid? (make-block 0 0 "blue")
                                  (make-posn 1 1)
                                  "left"))

(check-expect (bset-out-of-grid? 
               (list (make-block 1 0 "blue"))
                     (make-posn 1 1)
                     "left")
              (block-out-of-grid? (make-block 1 0 "blue")
                                  (make-posn 1 1)
                                  "left"))

(check-expect (bset-out-of-grid? 
               (list (make-block (+ 1 SCREEN-WIDTH) 0 "blue"))
                     (make-posn 1 1)
                     "right")
              (block-out-of-grid? (make-block (+ 1 SCREEN-WIDTH) 0 "blue")
                                  (make-posn 1 1)
                                  "right"))

(check-expect (bset-out-of-grid? 
               (list (make-block 0 0 "blue"))
                     (make-posn 1 1)
                     "right")
              (block-out-of-grid? (make-block 0 0 "blue")
                                  (make-posn 1 1)
                                  "right"))

(check-expect (bset-out-of-grid? 
               (list (make-block 1 0 "blue"))
                     (make-posn 1 2)
                     "a")
              (block-out-of-grid? (make-block 1 0 "blue")
                                  (make-posn 1 2)
                                  "a"))

(check-expect (bset-out-of-grid? 
               (list (make-block 1 4 "blue"))
                     (make-posn 1 1)
                     "a")
              (block-out-of-grid? (make-block 1 4 "blue")
                                  (make-posn 1 1)
                                  "a"))

(check-expect (bset-out-of-grid? 
               (list (make-block 1 4 "blue"))
                     (make-posn 1 1)
                     "s")
              (block-out-of-grid? (make-block 1 4 "blue")
                                  (make-posn 1 1)
                                  "s"))

(check-expect (bset-out-of-grid? 
               (list (make-block 1 0 "blue"))
                     (make-posn 1 1)
                     "s")
              (block-out-of-grid? (make-block 1 0 "blue")
                                  (make-posn 1 1)
                                  "s"))

;; tetra-out-of-grid? : Tetra Keyevent -> Boolean
;; Will the next move, move the tetra out of the grid?
(define (tetra-out-of-grid? t ke)
 (bset-out-of-grid? (tetra-blocks t) (tetra-center t) ke))
;; Tests
(check-expect (tetra-out-of-grid? O "down")
              (bset-out-of-grid? (tetra-blocks O)
                                 (tetra-center O)
                                 "down"))
(check-expect (tetra-out-of-grid? O "left")
              (bset-out-of-grid? (tetra-blocks O)
                                 (tetra-center O)
                                 "left"))
(check-expect (tetra-out-of-grid? O "right")
              (bset-out-of-grid? (tetra-blocks O)
                                 (tetra-center O)
                                 "right"))
(check-expect (tetra-out-of-grid? O "a")
              (bset-out-of-grid? (tetra-blocks O)
                                 (tetra-center O)
                                 "a"))
(check-expect (tetra-out-of-grid? O "s")
              (bset-out-of-grid? (tetra-blocks O)
                                 (tetra-center O)
                                 "s"))

;;; LINE UP
;; block=? : Block Block -> boolean
;; Do the blocks equal each other?
(define (block=? b1 b2)
 (and
  (= (block-y b1) (block-y b2))
  (= (block-x b1) (block-x b2))))
;; Tests
(check-expect (block=? (make-block 1 4 "blue")
                       (make-block 1 4 "blue"))
              true)

(check-expect (block=? (make-block 1 2 "blue")
                       (make-block 1 4 "blue"))
              false)

;; bl-helper : Block Block Posn Keyevent -> Boolean
;; Will the next move collide the block (b1)
;; with a block (b2) in the pile?
(define (bl-helper b1 b2 c ke)
 (cond
   [(key=? ke "down")
    (block=? (move-b b1 0 1)
             b2)]
   [(key=? ke "left")
    (block=? (move-b b1 -1 0)
             b2)]
   [(key=? ke "right")
    (block=? (move-b b1 1 0)
             b2)]
   [(key=? ke "a")
    (block=? (block-rotate c b1 ke)
             b2)]
   [(key=? ke "s")
    (block=? (block-rotate c b1 ke)
             b2)]))
;; Tests
(check-expect (bl-helper (make-block 1 3 "blue")
                         (make-block 1 4 "blue")
                         (make-posn 5 5)
                         "down")
              (block=? (move-b (make-block 1 3 "blue")
                               0 1)
                       (make-block 1 4 "blue")))

(check-expect (bl-helper (make-block 2 4 "blue")
                         (make-block 1 4 "blue")
                         (make-posn 5 5)
                         "left")
              (block=? (move-b (make-block 2 4 "blue")
                               -1 0)
                       (make-block 1 4 "blue")))

(check-expect (bl-helper (make-block 1 4 "blue")
                         (make-block 2 4 "blue")
                         (make-posn 5 5)
                         "right")
              (block=? (move-b (make-block 1 4 "blue")
                               1 0)
                       (make-block 2 4 "blue")))

(check-expect (bl-helper (make-block 10 5 "blue")
                         (make-block 5 0 "blue")
                         (make-posn 5 5)
                         "a")
              (block=? (block-rotate
                        (make-posn 5 5)
                        (make-block 10 5 "blue")
                        "a")
                       (make-block 5 0 "blue")))

(check-expect (bl-helper (make-block 8 0 "blue")
                         (make-block 10 8 "blue")
                         (make-posn 5 5)
                         "s")
              (block=? (block-rotate
                        (make-posn 5 5)
                        (make-block 8 0 "blue")
                        "s")
                       (make-block 10 8 "blue")))

;; block-lineup? : Block BSet Posn Keyevent -> Boolean
;; Will the next move collide the block with the pile?
(define (block-lineup? b bset c ke)
  (ormap (lambda (b2) (bl-helper b b2 c ke)) bset))  
;; Tests
(check-expect (block-lineup? (make-block 0 0 "blue")
                             (list (make-block 0 1 "blue"))
                             (make-posn 5 5)
                             "down")
              (bl-helper (make-block 0 0 "blue")
                         (make-block 0 1 "blue")
                         (make-posn 5 5)
                         "down"))

(check-expect (block-lineup? (make-block 1 0 "blue")
                             (list (make-block 0 0 "blue"))
                             (make-posn 5 5)
                             "left")
              (bl-helper (make-block 1 0 "blue")
                         (make-block 0 0 "blue")
                         (make-posn 5 5)
                         "left"))

(check-expect (block-lineup? (make-block 0 0 "blue")
                             (list (make-block 1 0 "blue"))
                             (make-posn 5 5)
                             "right")
              (bl-helper (make-block 0 0 "blue")
                         (make-block 1 0 "blue")
                         (make-posn 5 5)
                         "right"))

(check-expect (block-lineup? (make-block 10 5 "blue")
                             (list (make-block 5 0 "blue"))
                             (make-posn 5 5)
                             "a")
              (bl-helper (make-block 10 5 "blue")
                         (make-block 5 0 "blue")
                         (make-posn 5 5)
                         "a"))

(check-expect (block-lineup? (make-block 5 0 "blue")
                             (list (make-block 10 5 "blue"))
                             (make-posn 5 5)
                             "s")
              (bl-helper (make-block 5 0 "blue")
                         (make-block 10 5 "blue")
                         (make-posn 5 5)
                         "s"))

;; bset-lineup? : BSet BSet Posn Keyevent -> Boolean
;; Will the next move collide the bset with the pile?
(define (bset-lineup? bset pile c ke)
  (ormap (lambda (b) (block-lineup? b pile c ke)) bset))
;; Tests
(check-expect (bset-lineup? (list (make-block 10 5 "blue"))
                            (list (make-block 10 6 "blue"))
                            (make-posn 5 5)
                            "down")
              (block-lineup? (make-block 10 5 "blue")
                             (list (make-block 10 6 "blue"))
                             (make-posn 5 5)
                             "down"))

(check-expect (bset-lineup? (list (make-block 11 5 "blue"))
                            (list (make-block 10 5 "blue"))
                            (make-posn 5 5)
                            "left")
              (block-lineup? (make-block 11 5 "blue")
                             (list (make-block 10 5 "blue"))
                             (make-posn 5 5)
                             "left"))

(check-expect (bset-lineup? (list (make-block 9 5 "blue"))
                            (list (make-block 10 5 "blue"))
                            (make-posn 5 5)
                            "right")
              (block-lineup? (make-block 9 5 "blue")
                             (list (make-block 10 5 "blue"))
                             (make-posn 5 5)
                             "right"))

(check-expect (bset-lineup? (list (make-block 10 5 "blue"))
                            (list (make-block 5 0 "blue"))
                            (make-posn 5 5)
                            "a")
              (block-lineup? (make-block 10 5 "blue")
                             (list (make-block 5 0 "blue"))
                             (make-posn 5 5)
                             "a"))

(check-expect (bset-lineup? (list (make-block 5 0 "blue"))
                            (list (make-block 10 5 "blue"))
                            (make-posn 5 5)
                            "a")
              (block-lineup? (make-block 5 0 "blue")
                             (list (make-block 10 5 "blue"))
                             (make-posn 5 5)
                             "a"))

;; tetra-lineup? : Tetra BSet Keyevent -> Boolean
;; Will the next move collide the tetra with the pile?
(define (tetra-lineup? t pile ke)
 (bset-lineup? (tetra-blocks t) pile (tetra-center t) ke))
;; Tests
(check-expect (tetra-lineup? O
                             (list (make-block 10 6 "blue"))
                             "down")
              (bset-lineup? (tetra-blocks O)
                            (list (make-block 10 6 "blue"))
                            (tetra-center O)
                            "down"))

(check-expect (tetra-lineup? O
                             (list (make-block 10 6 "blue"))
                             "left")
              (bset-lineup? (tetra-blocks O)
                            (list (make-block 10 6 "blue"))
                            (tetra-center O)
                            "left"))

(check-expect (tetra-lineup? O
                             (list (make-block 10 6 "blue"))
                             "right")
              (bset-lineup? (tetra-blocks O)
                            (list (make-block 10 6 "blue"))
                            (tetra-center O)
                            "right"))

(check-expect (tetra-lineup? O
                             (list (make-block 10 6 "blue"))
                             "a")
              (bset-lineup? (tetra-blocks O)
                            (list (make-block 10 6 "blue"))
                            (tetra-center O)
                            "a"))

(check-expect (tetra-lineup? O
                             (list (make-block 10 6 "blue"))
                             "s")
              (bset-lineup? (tetra-blocks O)
                            (list (make-block 10 6 "blue"))
                            (tetra-center O)
                            "s"))

;;*************  New World  ********************

;; block-under-c : Block -> Boolean
;; Is the block under the top of the grid?
(define (block-under-c b)
 (>= (block-y b) 0))
;; Tests
(check-expect (block-under-c (make-block 30 30 "blue"))
              true)
(check-expect (block-under-c (make-block 30 -1 "blue"))
              false)

;; blocks-in-grid : BSet -> Number
;; Is the blocks in the bset under the top of the grid?
(define (blocks-in-grid bset)
 (cond
   [(empty? bset) 0]
   [(block-under-c (first bset)) (+ 1 (blocks-in-grid (rest bset)))]
   [else (+ 0 (blocks-in-grid (rest bset)))]))
;; Tests
(check-expect (blocks-in-grid empty)
              0)
(check-expect (blocks-in-grid (cons (make-block 30 -1 "blue")
                                    (cons (make-block 30 30 "blue")
                                          empty)))
              1)

;; cal-score : BSet BSet Number -> Number
;; Calculates the score
(define (cal-score old-pile pile s)
  (cond
    [(or (empty? old-pile)
         (empty? pile))
     s]
    [(> (length old-pile) (length pile))
     (+ (- (length old-pile) (length pile)) s)]
    [else s]))
;; Tests
(check-expect (cal-score bset1
                         bset2
                         0)
              0)
(check-expect (cal-score bset4
                         bset3
                         0)
              2)
(check-expect (cal-score bset4
                         bset5
                         3)
              3)

;; count-b-y : BSet Number -> Number
;; Counts the blocks with the Y(Number) grid cordinate.
(define (count-b-y pile y)
  (cond
    [(empty? pile) 0]
    [(= (block-y (first pile)) y)
     (+ 1 (count-b-y (rest pile)
                     y))]
    [else (+ 0 (count-b-y (rest pile)
                          y))]))
;; Tests
(check-expect (count-b-y bset1 0) 0)
(check-expect (count-b-y bset3 1) 1)
(check-expect (count-b-y bset4 1) 3)

;; delete-row : BSet Number -> BSet
;; Deletes the row
(define (delete-row pile y)
  (cond
    [(empty? pile) pile]
    [(= (block-y (first pile)) y)
     (delete-row (rest pile) y)]
    [else (cons (first pile)
                (delete-row (rest pile) y))]))
;; Tests
(check-expect (delete-row bset1 1) bset1)
(check-expect (delete-row bset3 1) empty)
(check-expect (delete-row bset4 1) empty)

;; move-down-pile : BSet Number -> BSet
;; Moves down the blocks in the bset by 1 grid point after the deletion
(define (move-down-pile pile y)
  (cond
    [(empty? pile) pile]
    [(= y GRID-HEIGHT) pile]
    [(< (block-y (first pile)) y)
     (cons (move-b (first pile) 0 1)
           (move-down-pile (rest pile) y))]
    [else (cons (first pile) (move-down-pile (rest pile) y))]))
;; Tests
(check-expect (move-down-pile bset1 1) bset1)
(check-expect (move-down-pile bset3 GRID-HEIGHT) bset3)
(check-expect (move-down-pile bset4 3) (list (make-block 8 2 "blue")
                                             (make-block 7 2 "blue")
                                             (make-block 5 2 "blue")))
(check-expect (move-down-pile bset5 1) (list (make-block 8 1 "blue")
                                             (make-block 7 1 "blue")
                                             (make-block 5 1 "blue")
                                             (make-block 4 1 "blue")))


;; find-del-row : BSet Number -> BSet
;; Find the row to delete
(define (find-del-row pile y)
  (cond
    [(empty? pile) pile]
    [(= y -1) pile]
    [(= (count-b-y pile y) GRID-WIDTH)
     (find-del-row (move-down-pile (delete-row pile y) 
                                   y) 
                 y)]
    [else (find-del-row pile (- y 1))]))
;; Tests
(check-expect (find-del-row bset1 0)
              empty)
(check-expect (find-del-row bset3 -1) bset3)
(check-expect (find-del-row bset6 1)
              empty)
(check-expect (find-del-row bset5 0)
              bset5)
 
;; create-new-world : BSet BSet Number -> World
;; Creates a new world 
(define (create-new-world t-blocks pile s)
 (make-world (select-tetra (random 7))
             (find-del-row(append t-blocks
                                  pile)
                        GRID-HEIGHT)
             (cal-score (append t-blocks
                                  pile) 
                        (find-del-row(append t-blocks
                                  pile)
                        GRID-HEIGHT)
                        s)))
;; Tests
(check-member-of (world-tetra (create-new-world (tetra-blocks O)
                                empty
                                0))
                 (select-tetra 0)
                 (select-tetra 1)
                 (select-tetra 2)
                 (select-tetra 3)
                 (select-tetra 4)
                 (select-tetra 5)
                 (select-tetra 6))
(check-expect (world-pile (create-new-world (tetra-blocks O)
                                            empty
                                            0))
              (find-del-row (append (tetra-blocks O)
                                    empty)
                            GRID-HEIGHT))
(check-expect (world-pile (create-new-world (tetra-blocks L)
                                            (tetra-blocks O)
                                            0))
              (find-del-row (append (tetra-blocks L)
                                    (tetra-blocks O))
                            GRID-HEIGHT))
(check-expect (world-score (create-new-world (tetra-blocks O)
                                             empty
                                             0))
              (cal-score (append (tetra-blocks O)
                                 empty) 
                         (find-del-row(append (tetra-blocks O)
                                              empty)
                                      GRID-HEIGHT)
                         0))

;; select-tetra : Number -> Tetra
;; Creates a tetra based on the number given
(define (select-tetra n)
 (cond
   [(= n 0) O]
   [(= n 1) I]
   [(= n 2) L]
   [(= n 3) J]
   [(= n 4) T]
   [(= n 5) Z]
   [(= n 6) S]))
;; Tests
(check-expect (select-tetra 0)
              O)
(check-expect (select-tetra 1)
              I)
(check-expect (select-tetra 2)
              L)
(check-expect (select-tetra 3)
              J)
(check-expect (select-tetra 4)
              T)
(check-expect (select-tetra 5)
              Z)
(check-expect (select-tetra 6)
              S)


;;*************  Interaction  ********************

;; key-press : World Keyevent -> World
;; World changes accoring to key pressed:
;; Down - Tetra moves down
;; Left - Tetra moves left
;; Right - Tetra moves right
;; A - Tetra rotates counter-clock-wise
;; S - Tetra rotates clock-wise
;; Else - World does not change
(define (key-press w ke)
 (cond [(key=? ke "down")
        (cond
          [(or (tetra-out-of-grid? (world-tetra w) ke)
               (tetra-lineup? (world-tetra w)(world-pile w) ke))
           (create-new-world (tetra-blocks (world-tetra w))
                             (world-pile w)
                             (world-score w))]
          [else (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w))
                                         (tetra-blocks (world-tetra w)))
                             0 1)
                            (world-pile w)
                            (world-score w))])]
       [(key=? ke "left")
        (cond
          [(or (tetra-out-of-grid? (world-tetra w) ke)
               (tetra-lineup? (world-tetra w)(world-pile w) ke)) 
           w]
          [else (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w))
                                         (tetra-blocks (world-tetra w)))
                             -1 0)
                            (world-pile w)
                            (world-score w))])]
       [(key=? ke "right")
        (cond
          [(or (tetra-out-of-grid? (world-tetra w) ke)
               (tetra-lineup? (world-tetra w)(world-pile w) ke)) 
           w]
          [else (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w))
                                         (tetra-blocks (world-tetra w)))
                             1 0)
                            (world-pile w)
                            (world-score w))])]
       [(key=? ke "a")
        (cond
          [(or (tetra-out-of-grid? (world-tetra w) ke)
               (tetra-lineup? (world-tetra w)(world-pile w) ke)) 
           w]
          [else
           (make-world (tetra-rotate
                        (make-tetra (tetra-center (world-tetra w))
                                    (tetra-blocks (world-tetra w)))
                        ke)
                       (world-pile w)
                       (world-score w))])]
       [(key=? ke "s")
        (cond
          [(or (tetra-out-of-grid? (world-tetra w) ke)
               (tetra-lineup? (world-tetra w)(world-pile w) ke)) 
           w]
          [else
           (make-world (tetra-rotate
                        (make-tetra (tetra-center (world-tetra w))
                                    (tetra-blocks (world-tetra w)))
                        ke)
                       (world-pile w)
                       (world-score w))])]
       [else w]))
;; Tests
(define w1 (make-world O empty 0))
(define w2 (make-world O (cons (make-block 6 8 "blue")
                               empty)
                       4))
(define w3 (make-world 
            (make-tetra 
             (make-posn 5 4)
             (cons (make-block 5 5 "blue")
                  empty))
            (cons (make-block 5 6 "blue")
                  (cons (make-block 6 5 "red")
                        (cons (make-block 4 5 "green")
                              (cons (make-block 6 4 "green")
                                    (cons (make-block 4 4 "green")
                                          empty)))))
                       4))
(define w4 (make-world (make-tetra 
                        (make-posn 0 50)
                        (cons (make-block 0 50 "blue")
                             empty))
                       empty
                       0))
(define w5 (make-world (make-tetra 
                        (make-posn SCREEN-WIDTH 50)
                        (cons (make-block SCREEN-WIDTH 50 "blue")
                             empty))
                       empty
                       0))
(define w6 (make-world (make-tetra 
                        (make-posn 50 SCREEN-HEIGHT)
                        (cons (make-block 50 SCREEN-HEIGHT "blue")
                             empty))
                       empty
                       0))

;; No collisions with pile that is empty
(check-expect (key-press w1 "down")
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w1))
                                         (tetra-blocks (world-tetra w1)))
                             0 1)
                            (world-pile w1)
                            (world-score w1)))
(check-expect (key-press w1 "left")
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w1))
                                         (tetra-blocks (world-tetra w1)))
                             -1 0)
                            (world-pile w1)
                            (world-score w1)))
(check-expect (key-press w1 "right")
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w1))
                                         (tetra-blocks (world-tetra w1)))
                             1 0)
                            (world-pile w1)
                            (world-score w1)))
(check-expect (key-press w1 "a")
              (make-world (tetra-rotate
                             (make-tetra (tetra-center (world-tetra w1))
                                         (tetra-blocks (world-tetra w1)))
                             "a")
                            (world-pile w1)
                            (world-score w1)))
(check-expect (key-press w1 "s")
              (make-world (tetra-rotate
                             (make-tetra (tetra-center (world-tetra w1))
                                         (tetra-blocks (world-tetra w1)))
                             "s")
                            (world-pile w1)
                            (world-score w1)))
;; No collisions with pile that is not empty
(check-expect (key-press w2 "down")
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w2))
                                         (tetra-blocks (world-tetra w2)))
                             0 1)
                            (world-pile w2)
                            (world-score w2)))
(check-expect (key-press w2 "left")
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w2))
                                         (tetra-blocks (world-tetra w2)))
                             -1 0)
                            (world-pile w2)
                            (world-score w2)))
(check-expect (key-press w2 "right")
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w2))
                                         (tetra-blocks (world-tetra w2)))
                             1 0)
                            (world-pile w2)
                            (world-score w2)))
(check-expect (key-press w2 "a")
              (make-world (tetra-rotate
                             (make-tetra (tetra-center (world-tetra w2))
                                         (tetra-blocks (world-tetra w2)))
                             "a")
                            (world-pile w2)
                            (world-score w2)))
(check-expect (key-press w2 "s")
              (make-world (tetra-rotate
                             (make-tetra (tetra-center (world-tetra w2))
                                         (tetra-blocks (world-tetra w2)))
                             "s")
                            (world-pile w2)
                            (world-score w2)))
;; Collisions tetris line-up
#;; create-new-world uses (random 7) unable to do check-expect
 ;; but create-new-world has check-expect done
(check-expect (key-press w3 "down")
              (create-new-world (tetra-blocks (world-tetra w3))
                             (world-pile w3)
                             (world-score w3)))
(check-expect (key-press w3 "left")
              w3)
(check-expect (key-press w3 "right")
              w3)
(check-expect (key-press w3 "a")
              w3)
(check-expect (key-press w3 "s")
              w3)
;; Collisions out of grid
(check-expect (key-press w4 "left")
              w4)
(check-expect (key-press w4 "right")
              w4)
#;; create-new-world uses (random 7) unable to do check-expect
 ;; but create-new-world has check-expect done
(check-expect (key-press w6 "down")
              (create-new-world (tetra-blocks (world-tetra w6))
                             (world-pile w6)
                             (world-score w6)))
(check-expect (key-press w4 "a")
              w4)
(check-expect (key-press w4 "s")
              w4)
;; Different key
(check-expect (key-press w1 "g")
              w1)
              

;; tick : World -> World
;; Tetra in the world moves down in each tick
;; New world is created when blocks lineup
(define (tick w)
 (cond
   [(tetra-out-of-grid? (world-tetra w) "down")
    (create-new-world (tetra-blocks (world-tetra w))
                      (world-pile w)
                      (world-score w))]
   [(tetra-lineup? (world-tetra w)(world-pile w) "down")
    (create-new-world (tetra-blocks (world-tetra w))
                      (world-pile w)
                      (world-score w))]
   [else (make-world (move-tetra
                      (make-tetra (tetra-center (world-tetra w))
                                  (tetra-blocks (world-tetra w)))
                      0 1)
                     (world-pile w)
                     (world-score w))]))
;; Tests
(check-expect (tick w1)
              (make-world (move-tetra
                             (make-tetra (tetra-center (world-tetra w1))
                                         (tetra-blocks (world-tetra w1)))
                             0 1)
                            (world-pile w1)
                            (world-score w1)))
#;; create-new-world uses (random 7) unable to do check-expect
 ;; but create-new-world has check-expect done
 ;; Tests line-up
(check-expect (tick w3)
              (create-new-world (tetra-blocks (world-tetra w3))
                             (world-pile w3)
                             (world-score w3)))
#;; create-new-world uses (random 7) unable to do check-expect
 ;; but create-new-world has check-expect done
 ;; Tests out-of-grid
(check-expect (tick w6)
              (create-new-world (tetra-blocks (world-tetra w6))
                             (world-pile w6)
                             (world-score w6)))


;;*************  GAME OVER  ********************
;; block-over-c? : Block -> Boolean
;; Is the block over the top of the grid?
(define (block-over-c? b)
 (< (block-y b) 0))
;; Tests
(check-expect (block-over-c? (make-block 1 1 "blue"))
              false)
(check-expect (block-over-c? (make-block -1 -1 "blue"))
              true)

;; pile-over-c? : BSet -> Boolean
;; Is the pile beyond the top of the grid?
(define (pile-over-c? bset)
  (ormap block-over-c? bset))
;; Tests
(check-expect (pile-over-c? empty)
              false)
(check-expect (pile-over-c? (cons (make-block 1 1 "blue")
                                  (cons
                                   (make-block 1 5 "blue")
                                   empty)))
              false)
(check-expect (pile-over-c? (cons (make-block 1 1 "blue")
                                  (cons
                                   (make-block 1 -1 "blue")
                                   empty)))
              true)

;; game-over? : World -> Boolean
;; Did the last tetra land beyond the top of the grid?
(define (game-over? w)
 (pile-over-c? (world-pile w)))
;; Tests
(check-expect (game-over? (make-world O 
                                      (cons (make-block 0 -1 "blue")
                                              empty)
                                      1))
              (pile-over-c? (cons (make-block 0 -1 "blue")
                                              empty)))
(check-expect (game-over? (make-world O 
                                      (cons (make-block 0 50 "blue")
                                              empty)
                                      1))
              (pile-over-c? (cons (make-block 0 50 "blue")
                                              empty)))

;; final-scene : World -> Scene
;; Creates the final scene of the world with the score
(define (final-scene w)
 (place-image (overlay
               (text (string-append
                     "GAME OVER, Score: " (number->string (world-score w)))
                    10 
                    "black")
               (empty-scene SCREEN-WIDTH (/ SCREEN-HEIGHT 8)))
              (/ SCREEN-WIDTH 2)
              (/ SCREEN-HEIGHT 2)
              (scene+bset (world-pile w)
                          (scene+tetra (world-tetra w)
                                       EMPTY-BOARD))))
;; Tests
(check-expect (final-scene w1)
              (place-image 
               (overlay
                (text (string-append
                       "GAME OVER, Score: " (number->string (world-score w1)))
                      10 
                      "black")
                (empty-scene SCREEN-WIDTH (/ SCREEN-HEIGHT 8)))
              (/ SCREEN-WIDTH 2)
              (/ SCREEN-HEIGHT 2)
              (scene+bset empty
                          (scene+tetra O
                                       EMPTY-BOARD))))
(check-expect (final-scene w3)
              (place-image 
               (overlay
                (text (string-append
                       "GAME OVER, Score: " (number->string (world-score w3)))
                      10 
                      "black")
                (empty-scene SCREEN-WIDTH (/ SCREEN-HEIGHT 8)))
              (/ SCREEN-WIDTH 2)
              (/ SCREEN-HEIGHT 2)
              (scene+bset (world-pile w3)
                          (scene+tetra (world-tetra w3)
                                       EMPTY-BOARD))))

;;************* RUN PROGRAM RUNNN ****************
(define last1 (big-bang world1
                      (on-tick tick 1/2)
                      (on-key key-press)
                      (on-draw world->scene)
                      (stop-when game-over? final-scene)))