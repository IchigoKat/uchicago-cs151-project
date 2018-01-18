#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")
(require "chess-logic.rkt")

;; ==== ==== ==== ====
;; external interface

(provide board->image
         piece
         w-chess
         b-chess
         sublist)

(: piece : (Listof PieceType))
(define piece (list 'Rook 'Knight 'Bishop 'Queen 'King 'Pawn))


(: w-chess : (Listof String))
(define w-chess '("♖" "♘" "♗" "♕" "♔" "♙"))

(: b-chess : (Listof String))
(define b-chess '("♜" "♞" "♝" "♛" "♚" "♟"))

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


(: chess-row : String String  (Listof Square) -> Image)
;;produce an image of a row of n squares chess board
;;pieces in the the square are determined by the list input
;;the color of squares alternated by the two strings, start by the first
(define (chess-row c1 c2  ls)
  (match ls
    ['() empty-image]
    [(cons first rest)
     (match first
       ['None (beside (overlay (square 40 "outline" "black")
                                (square 40 "solid" c1))
                       (chess-row c2 c1 (list-tail ls 1)))]
       [(Some (Piece type player))
        (if (symbol=? player 'White)
          (beside (overlay (text (list-ref w-chess (index-of symbol=? piece type)) 24 "black")
                           (overlay (square 40 "outline" "black")
                                    (square 40 "solid" c1)))
                  (chess-row c2 c1 (list-tail ls 1)))
          (beside (overlay (text (list-ref b-chess (index-of symbol=? piece type)) 25 "black")
                           (overlay (square 40 "outline" "black")
                                    (square 40 "solid" c1)))
                  (chess-row c2 c1 (list-tail ls 1))))])])) 
        
(chess-row "brown" "beige" (list (Some (Piece 'Knight 'Black)) 'None))
(chess-row "brown" "beige" (list 'None (Some (Piece 'King 'Black))))

(: board->image : Board -> Image)
;;draw the image of a board
(define (board->image b)
  (above (chess-row "brown" "beige" (sublist b 56 63))
         (chess-row "beige" "brown" (sublist b 48 55))
         (chess-row "brown" "beige" (sublist b 40 47))
         (chess-row "beige" "brown" (sublist b 32 39))
         (chess-row "brown" "beige" (sublist b 24 31))
         (chess-row "beige" "brown" (sublist b 16 23))
         (chess-row "brown" "beige" (sublist b 8 15))
         (chess-row "beige" "brown" (sublist b 0 7)))) 
(board->image starting-board) 




         