#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "chess-logic.rkt")


(define-struct Point
  ([x : Integer]
   [y : Integer]))

(define-struct ChessWorld
  ([side : Integer]   
   [clicked : (Optional Point)]
   [promote : (Optional Move)]
   [game : ChessGame]))

(: piece : (Listof PieceType))
(define piece (list 'Rook 'Knight 'Bishop 'Queen 'King 'Pawn))


(: w-chess : (Listof String))
(define w-chess '("♖" "♘" "♗" "♕" "♔" "♙"))

(: b-chess : (Listof String))
(define b-chess '("♜" "♞" "♝" "♛" "♚" "♟"))

(: file : (Listof File))
(define file (list 'A 'B 'C 'D 'E 'F 'G 'H))

(define black-next
  (list (Move (Loc 'A 1) (Loc 'B 2) (Piece 'Knight 'White) 'None 'None)))

(define white-next
  (list (Move (Loc 'A 1) (Loc 'B 2) (Piece 'Knight 'Black) 'None 'None)))


(define t1
  (ChessWorld 40  'None 'None (ChessGame starting-board black-next)))

(define t2
  (ChessWorld 40 (Some (Point 8 8)) 'None (ChessGame starting-board white-next))) 

(define t3
  (ChessGame
   (strings->board '("R---K--R"
                     "-r------"
                     "--------"
                     "-PppPPp-"
                     "-------P"
                     "--------"
                     "------P-"
                     "----k--r"))
   (list (Move (Loc 'F 7) (Loc 'F 5) (Piece 'Pawn 'Black) 'None 'None))))

(define t4
  (ChessGame
   (strings->board '("R---K--R"
                     "-r------"
                     "--------"
                     "-PppPPp-"
                     "-------P"
                     "--------"
                     "--------"
                     "----k-Qr"))
   (list (Move (Loc 'F 7) (Loc 'F 5) (Piece 'Pawn 'Black) 'None 'None))))

(define t5
  (ChessWorld
   40
   'None
   'None
   t4))

(define t6
  (ChessWorld 40 'None 'None
              (ChessGame
               (strings->board (list "RNBQKBNR"
                                     "PPPPPPPP"
                                     "--------"
                                     "--------"
                                     "--------"
                                     "-p------"
                                     "p-pppppp"
                                     "rnbqkbnr"))
               (list (Move (Loc 'B 2) (Loc 'B 3) (Piece 'Pawn 'White) 'None 'None)))))

(define t7
  (ChessWorld 40
              (Some (Point 2 7))
              'None
              (ChessGame
               starting-board
               '())))
               


(: sublist : All (A) (Listof  A) Integer Integer -> (Listof A))
;;given a list return the sublist from index integer 1 to index integer 2
(define (sublist as n1 n2)
  (match as
    ['() '()]
    [(cons first rest)
     (cond
       [(> n1 0) (sublist rest (sub1 n1) (sub1 n2))]
       [(<= n1 0 n2) (cons first (sublist rest (sub1 n1) (sub1 n2)))]
       [else '()])]))
(check-expect (sublist '(1 2 3 4 5) 2 3)  '(3 4))

(: modified-chess-row : Integer String String  (Listof Square) -> Image)
;;produce an image of a row of n squares chess board
;;pieces in the the square are determined by the list input
;;the color of squares alternated by the two strings, start by the first
;;side of each square determined by input
(define (modified-chess-row side c1 c2  ls)
  (match ls
    ['() empty-image]
    [(cons first rest)
     (match first
       ['None (beside (overlay (square side "outline" "black")
                                (square side "solid" c1))
                       (modified-chess-row side c2 c1 (list-tail ls 1)))]
       [(Some (Piece type player))
        (if (symbol=? player 'White)
          (beside (overlay (text (list-ref w-chess (index-of symbol=? piece type))
                                 (cast (exact-ceiling (* 3/5 side)) Byte) "black")
                           (overlay (square side "outline" "black")
                                    (square side "solid" c1)))
                  (modified-chess-row side c2 c1 (list-tail ls 1)))
          (beside (overlay (text (list-ref b-chess (index-of symbol=? piece type))
                                 (cast (exact-ceiling (* 3/5 side)) Byte) "black")
                           (overlay (square side "outline" "black")
                                    (square side "solid" c1)))
                  (modified-chess-row side c2 c1 (list-tail ls 1))))])])) 
        
(modified-chess-row 40 "brown" "beige" (list (Some (Piece 'Knight 'Black)) 'None))
(modified-chess-row 40 "brown" "beige" (list 'None (Some (Piece 'King 'Black))))

(: modified-board->image : Integer Board -> Image)
;;draw the image of a board
;;side of each square determined by input 
(define (modified-board->image n b)
  (above (modified-chess-row n "brown" "beige" (sublist b 56 63))
         (modified-chess-row n "beige" "brown" (sublist b 48 55))
         (modified-chess-row n "brown" "beige" (sublist b 40 47))
         (modified-chess-row n "beige" "brown" (sublist b 32 39))
         (modified-chess-row n "brown" "beige" (sublist b 24 31))
         (modified-chess-row n "beige" "brown" (sublist b 16 23))
         (modified-chess-row n "brown" "beige" (sublist b 8 15))
         (modified-chess-row n "beige" "brown" (sublist b 0 7)))) 
(modified-board->image 40 starting-board) 
(modified-board->image 80 starting-board) 
(modified-board->image 20 starting-board) 



(: new-chess-world : Integer -> ChessWorld)
;;create a ChessWorld with a new game
;;side of each square is determined by the integer
(define (new-chess-world n)
  (if (< n 40)
      (error "square side too small, should be greater or equal to 40")
      (ChessWorld n 
                  'None
                  'None
                  (ChessGame starting-board '()))))
(check-error (new-chess-world  30) "square side too small, should be greater or equal to 40")


(: world-from-game : ChessGame Integer -> ChessWorld)
;;create a ChessWorld with a given game
;;side of each square is determined by the integer
(define (world-from-game game n)
  (if (< n 40)
      (error "square side too small, should be greater or equal to 40")
      (ChessWorld n 
                  'None
                  'None
                  game)))
(check-error (world-from-game t3 30) "square side too small, should be greater or equal to 40")

(: row-of-squares : Integer Real -> Image)
;;draw the given number of squares with given lenght in a row
(define (row-of-squares n side)
  (if (> n 0)
      (beside (square side "outline" "black")
              (row-of-squares (sub1 n) side))
      empty-image))
(row-of-squares 10 40) 



(: grids : Integer Integer Real -> Image)
;;given how many squares the grids have in a row and a column
;;and the side length of each square in the grid
;;draw the grid
(define (grids row col side)
  (if (> col 0)
      (above (row-of-squares row side)
             (grids row (sub1 col) side))
      empty-image))
(grids 10 2 40)



(: pos-of-square : Point Real Integer Integer  -> (Optional Point))
;;given the location of the mouse, side of each square in the grid
;;and num of squares in a row and a col
;;return the Point of the square where the mouse is at
;;with 'None if mouse not in the grid or
;;(Loc a b), where a b denotes which column and which row the square is at
(define (pos-of-square loc side r c)
  (match loc
    [(Point x y)
     (local {(define r1 (exact-ceiling (/ x side)))
             (define c1 (exact-ceiling (/ y side)))}
     (if (and (<= 1 r1 r) (<= 1 c1 c))
         (Some (Point r1 c1))
         'None))]))
(check-expect (pos-of-square (Point 1 1) 10 1 2) (Some (Point 1 1)))
(check-expect (pos-of-square (Point 21 3) 10 2 3) 'None)

(: point=? : Point Point -> Boolean)
;;check if two points are the same
(define (point=? p1 p2)
  (match* (p1 p2)
    [((Point a b) (Point x y))
     (and (= a x)
          (= b y))]))
(check-expect (point=? (Point 1 1) (Point 1 1)) #t)
(check-expect (point=? (Point 1 2) (Point 1 1)) #f)

(: end-line? : Player Loc -> Boolean)
;;check if a piece with a given dst can possibly be a promotion
(define (end-line? color dst)
  (match dst
    [(Loc _ r)
     (or (and (symbol=? color 'White) (= r 8))
         (and (symbol=? color 'Black) (= r 1)))]))
(check-expect (end-line? 'White (Loc 'A 8)) #t) 
(check-expect (end-line? 'Black (Loc 'A 8)) #f)

(: en-passant? :  Loc Loc Board -> Boolean)
;;check if a move is possibily an en-passant
(define (en-passant?  src dst b)
  (match* (src dst)
    [((Loc f1 r1) (Loc f2 r2))
     (match (board-ref b dst)
       ['None (and (not (symbol=? f1 f2))
                   (= 1 (abs (- r1 r2))))]
       [_ #f])]))
(check-expect (en-passant? (Loc 'A 4) (Loc 'B 3) starting-board) #t)
  

(: point->loc : Point -> Loc)
;;convert a point (with 1 <= x y <= 8) to a loc
;;if not possible return (Loc 'A 1)
(define (point->loc p)
  (match p
    [(Point x y)
     (if (and (<= 1 x 8)
              (<= 1 y 8))
         (Loc (list-ref file (sub1 x)) (cast (- 9 y) Rank))
         (Loc 'A 1))]))
(check-expect (point->loc (Point 1 2)) (Loc 'A 7))


(: handle-click : ChessWorld Integer Integer Mouse-Event -> ChessWorld)
;;This function determines which square, if any, was clicked on,
;;and implements the multi-step interactions needed for piece movement in chess.
(define (handle-click w x y event)
  (match* (event w)
    [("button-down" (ChessWorld side c p game))
     (match (pos-of-square (Point x y) side 8 8)
       ['None w]
       [(Some point)
        (match c
          ['None (ChessWorld side (Some point) p game)]
          [(Some cp)
           (if (point=? cp point)
               (ChessWorld side 'None 'None game)
               (local {(define src (point->loc cp))
                       (define dst (point->loc point))
                       (define b (ChessGame-board game))}
               (match (board-ref b src)
                 ['None w]
                 [(Some piece)
                  (match piece
                    [(Piece 'Pawn color)
                     (cond
                       [(end-line? color dst)
                        (if (legal-move? game (Move src dst piece (board-ref b dst) (Some 'Queen)))
                            (ChessWorld side  c (Some (Move src dst piece (board-ref b dst) (Some 'Queen))) game)
                            (ChessWorld side  c 'None game))]
                       [(en-passant? src dst (ChessGame-board game))
                        (if (legal-move? game (Move src dst piece (Some (Piece 'Pawn (other-player color))) 'None))
                            (ChessWorld side  'None p (apply-move game (Move src dst piece (Some (Piece 'Pawn (other-player color))) 'None)))
                            (ChessWorld side  c 'None game))]
                       [else (if
                              (legal-move? game (Move src dst piece (board-ref b dst) 'None))
                              (ChessWorld side  'None 'None (apply-move game (Move src dst piece (board-ref b dst) 'None)))
                              (ChessWorld side  c 'None game))])]
                    [_
                      (if (legal-move? game (Move src dst piece (board-ref b dst) 'None))
                          (ChessWorld side  'None p (apply-move game (Move src dst piece (board-ref b dst) 'None)))
                          (ChessWorld side  c 'None game))])])))])])]    
    [(_ _) w]))
(check-expect (handle-click t5 1 1 "move") t5)
(check-expect (handle-click t5 1 1 "button-down")
              (ChessWorld 40 (Some (Point 1 1)) 'None t4))
(check-expect (handle-click t7 60 220 "button-down") t6)
                            

(: key : ChessWorld String -> ChessWorld)
;;if ChessWorld-promote has a move, then promote the piece according to key entered
;;if ChessWorld-promote is 'None, pressing keys won't change
(define (key w s)
  (match w
    [(ChessWorld side point 'None game) w]
    [(ChessWorld side point (Some (Move src dst piece captured _)) game)
     (match s
       ["q" (ChessWorld side 'None 'None (apply-move game (Move src dst piece captured (Some 'Queen))))]
       ["b" (ChessWorld side 'None 'None (apply-move game (Move src dst piece captured (Some 'Bishop))))]
       ["k" (ChessWorld side 'None 'None (apply-move game (Move src dst piece captured (Some 'Knight))))]
       ["r" (ChessWorld side 'None 'None (apply-move game (Move src dst piece captured (Some 'Rook))))])]))
(check-expect (key (ChessWorld 40 (Some (Point 7 7))
                               (Some (Move (Loc 'G 2) (Loc 'G 1) (Piece 'Pawn 'Black) 'None (Some 'Queen))) t3)
                   "r")
              (ChessWorld 40  'None 'None
                               (apply-move t3 (Move (Loc 'G 2) (Loc 'G 1) (Piece 'Pawn 'Black) 'None (Some 'Rook)))))
                
          
   
    
  
    
  

(: stop? : ChessWorld -> Boolean)
;;stop if the length of clicked in world is equal to row * col
;;and the mouse is off the grid
(define (stop? w)
  (match w
    [(ChessWorld _ _ _ game)
     (or (checkmate? game)
         (stalemate? game))]))
(check-expect (stop? (world-from-game test5 40)) #f)
(check-expect (stop? (world-from-game test6 40)) #t)
(check-expect (stop? (world-from-game test7 40)) #t)


(: draw-chess-world : ChessWorld -> Image)
;;draw the grid
;;clicked squares should have a solid circle in it(double click erase the circle)
;;change the color of the square if the mouse is at it
(define (draw-chess-world w)
  (match w
    [(ChessWorld side c p (ChessGame b history))
     (local {(: fx : Image Real -> (Point Image -> Image))
             (define (fx i adjust)
               (lambda ([loc : Point] [a : Image])
                 (match loc
                   [(Point x y) (overlay/xy i
                                            (- 0 (+ (* side (sub1 x)) adjust))
                                            (- 0 (+ (* side (sub1 y)) adjust))
                                            a)])))
             (define turn (turn? history))
             (define space (rectangle (* side 8) 
                                      (cast (exact-ceiling (* 3/4 side)) Byte)                                           
                                      "solid" "white"))
             (: hx : ChessGame -> Image)
             (define (hx game)
               (overlay
               (cond
                 [(checkmate? game)
                  (text (string-append
                         (symbol->string turn) " loses")
                        (cast (exact-ceiling (* side 3/10)) Byte) "black")]
                 [(in-check? game)
                   (text (string-append                          
                         (symbol->string turn) " is in-check")
                        (cast (exact-ceiling (* side 3/10)) Byte) "black")]
                 
                 [(stalemate? game)
                  (text "draw"
                        (cast (exact-ceiling (* side 3/10)) Byte) "black")]
                 [else (text (string-append
                                      "   "
                                      (symbol->string turn)
                                      "'s "
                                      "Turn"
                                      "   ")
                                     (cast (exact-ceiling (* side 3/10)) Byte) "black")])
               space))
                  
             (define interface
               (above (hx (ChessGame b history))
                      (overlay (text "PS: When a pawn reaches the other side’s end line,"
                                     (cast (exact-ceiling (* side 3/10)) Byte) "black")
                               space)
                      (overlay (text "Then press keyboard to choose what to promote to"
                                     (cast (exact-ceiling (* side 3/10)) Byte) "black")
                               space)
                      (overlay (text "q for “Queen”,r for “Rook”,k for “Knight”,b for “Bishop”"
                                     (cast (exact-ceiling (* side 3/10)) Byte) "black")
                               space)))}                                     
       (match c
         ['None
          (above (modified-board->image side b)
                 interface)]
         [(Some p)
          ((fx (square side "solid" (color 10 20 0 150)) 0)
           p
           (above (modified-board->image side b)
                 interface))]))]))
                          
(draw-chess-world t1)
(draw-chess-world t2)
(draw-chess-world (world-from-game test6 40))
(draw-chess-world (world-from-game test7 40))
(draw-chess-world (world-from-game test1 40))
(draw-chess-world (new-chess-world 40))
(draw-chess-world (world-from-game t3 40))


  

(: play-new : Integer -> ChessWorld)
;;Your play functions should build a ChessWorld from new game
;;and run big-bang on that world
(define (play-new n)
  (big-bang (new-chess-world n) : ChessWorld
  [to-draw draw-chess-world]
  [on-key key]
  [on-mouse handle-click]
  [stop-when stop?]))

(: play-from : ChessGame Integer -> ChessWorld)
;;Your play functions should build a ChessWorld from an in-progress game
;;and run big-bang on that world  
(define (play-from game n)
  (big-bang (world-from-game game n) : ChessWorld
            [to-draw draw-chess-world]
            [on-key key]
            [on-mouse handle-click]
            [stop-when stop?]))

(test)
(play-new 70)
               
