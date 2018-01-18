#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(require "../project1/optional.rkt")
(require "../project1/loc.rkt")

;; ==== ==== ==== ====
;; external interface

(provide PieceType
         Player
         (struct-out Piece)
         Square
         Board
         (struct-out Move)
         PromoteTo
         (struct-out ChessGame)
         index-of
         starting-board      
         new-game  
         board-ref     
         board-update   
         in-check?      
         legal-move?     
         moves-piece   
         moves-player   
         checkmate?    
         stalemate?     
         apply-move    
         strings->board
         turn?
         test1
         test5
         test6
         test7
         other-player
         loc-to-index
         index-to-loc)

;; ==== ==== ==== ====
;; data definitions

(define-type PieceType
  (U 'Pawn 'Bishop 'Knight 'Rook 'King 'Queen))

(define-type Player
  (U 'Black 'White))

(define-struct Piece
  ([type  : PieceType]
   [color : Player]))

(define-type Square
  (Optional Piece))

(define-type Board
  (Listof Square))

(define-type PromoteTo
  (U 'Queen 'Rook 'Bishop 'Knight))

(define-struct Move
  ([src        : Loc]
   [dst        : Loc]
   [moved      : Piece]
   [captured   : (Optional Piece)]
   [promote-to : (Optional PromoteTo)]))

(define-struct ChessGame
  ([board : Board]
   [history : (Listof Move)]))

(define-struct Direct
  ([x : Integer]
   [y : Integer]))
      

(: piece : (Listof PieceType))
(define piece (list 'Rook 'Knight 'Bishop 'Queen 'King 'Pawn))



(define-type Position
  (Optional Loc))

(: file : (Listof File))
(define file (list 'A 'B 'C 'D 'E 'F 'G 'H))

(: rank : (Listof Rank))
(define rank '(1 2 3 4 5 6 7 8))

(: black : (Listof Char))
(define black '(#\R #\N #\B #\Q #\K #\P))

(: white : (Listof Char))
(define white '(#\r #\n  #\b #\q #\k #\p))

(: index-of : All (A) (A A -> Boolean) (Listof A)  A -> Integer)
;;return the index of the first occurence of the given element in the list
;;return -1 if not in the list
;;need to enter the function for checking the equality of two type A elements
(define (index-of fx xs x)
  (match xs
    ['() -1]
    [(cons first rest) (if (fx first x) 0
                           (local {(define a (index-of fx rest x))}
                             (if (= -1 a) -1
                                 (add1 a))))]))
(check-expect (index-of = '(2 3 4) 3) 1)
(check-expect (index-of symbol=? piece 'Pawn) 5)
(check-expect (index-of symbol=? piece 'Paw) -1)


(: strings->board : ((Listof String) -> Board))
;;Create a board based on the given list of strings
(define (strings->board ss)
 (build-list 64 (lambda ([x : Integer])
                  (local {(define y (string-ref
                                     (list-ref ss (- 7 (quotient x 8)))
                                     (remainder x 8)))
                          (define (char? [x : Char] [y : Char])
                            (char=? x y))}
                  (cond [(< -1 (index-of char? white y))
                         (Some (Piece  (list-ref piece (index-of char? white y))  'White))]
                        [(< -1 (index-of char? black y))
                         (Some (Piece   (list-ref piece (index-of char? black y))  'Black))]
                        [else 'None])))))

(define test1
  (ChessGame
   (strings->board (list
                     "-----Q--"
                     "--P-r---"
                     "-q---rR-"
                     "---kR-p-"
                     "-b--n---"
                     "K---R-rP"
                     "-p------"
                     "---N----"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'Black) 'None 'None))))

(define test2
  (ChessGame
   (strings->board '("B----Q--"
                     "--P-r---"
                     "-q-k-rR-"
                     "r-----p-"
                     "R---n---"
                     "K---R-rP"
                     "-rN-----"
                     "---Q-b--"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'White) 'None 'None))))

(define test3
  (ChessGame
   (strings->board '("-----Q--"
                     "--P-r---"
                     "-q---rR-"
                     "---kR-p-"
                     "-b--n---"
                     "K---R-rP"
                     "-P------"
                     "---N----"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'Black) 'None 'None))))

(define test4
  (ChessGame
   (strings->board '("B----Q--"
                     "--P-r---"
                     "-q-k-rR-"
                     "r-----p-"
                     "R---n---"
                     "K---R--P"
                     "-rN-----"
                     "---Q-b--"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'White) 'None 'None))))

(define test5
  (ChessGame
   (strings->board '("-----Q--"
                     "--P-r---"
                     "-q---rR-"
                     "-R-kR-p-"
                     "-b--n---"
                     "K--RR-rP"
                     "-P------"
                     "---N----"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'Black) 'None 'None))))

(define test6
  (ChessGame
   (strings->board '("K-------"
                     "-r-r----"
                     "-r------"
                     "--------"
                     "--------"
                     "--------"
                     "--------"
                     "--------"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'White) 'None 'None))))
(define test7
  (ChessGame
   (strings->board '("K-r-----"
                     "-r-r----"
                     "-r------"
                     "--------"
                     "--------"
                     "--------"
                     "--------"
                     "--------"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'White) 'None 'None))))
(define test8
  (ChessGame
   (strings->board '("R---K-R-"
                     "-r------"
                     "-r------"
                     "--------"
                     "--------"
                     "--------"
                     "--------"
                     "----k---"))
   (list (Move (Loc 'A 1) (Loc 'A 2) (Piece 'Rook 'White) 'None 'None))))
(define test9
  (ChessGame
   (strings->board '("R---K--R"
                     "-r------"
                     "--------"
                     "-PppP---"
                     "--------"
                     "--------"
                     "--------"
                     "----k---"))
   (list (Move (Loc 'E 7) (Loc 'E 5) (Piece 'Pawn 'Black) 'None 'None))))

(define test10
  (ChessGame
   (strings->board '("--KR--R-"
                     "-r------"
                     "-r------"
                     "--------"
                     "--------"
                     "--------"
                     "--------"
                     "----k---"))
   (list
    (Move (Loc 'E 8) (Loc 'C 8) (Piece 'King 'Black) 'None 'None)
    (Move (Loc 'A 1) (Loc 'A 2) (Piece 'Rook 'White) 'None 'None))))

(define test11
  (ChessGame
   (strings->board '("R---K--R"
                     "-r------"
                     "----p---"
                     "-Pp-----"
                     "--------"
                     "--------"
                     "--------"
                     "----k---"))
   (list
    (Move (Loc 'D 5) (Loc 'E 6) (Piece 'Pawn 'White) (Some (Piece 'Pawn 'Black)) 'None)
    (Move (Loc 'E 7) (Loc 'E 5) (Piece 'Pawn 'Black) 'None 'None))))

(define test12
  (ChessGame
   (strings->board '("R---K--R"
                     "-rp-----"
                     "----p---"
                     "-Pp-----"
                     "--------"
                     "--------"
                     "--------"
                     "----k---"))
   (list   
    (Move (Loc 'E 7) (Loc 'E 5) (Piece 'Pawn 'Black) 'None 'None))))

(define test13
  (ChessGame
   (strings->board '("R-q-K--R"
                     "-r------"
                     "----p---"
                     "-Pp-----"
                     "--------"
                     "--------"
                     "--------"
                     "----k---"))
   (list
    (Move (Loc 'C 7) (Loc 'C 8) (Piece 'Pawn 'White) 'None (Some 'Queen))
    (Move (Loc 'E 7) (Loc 'E 5) (Piece 'Pawn 'Black) 'None 'None))))


(: starting-board : Board)
;;This value represents the standard starting layout of a chess board, with white at bottom (ranks 1 and 2).
(define starting-board
  (strings->board (list "RNBQKBNR"
                        "PPPPPPPP"
                        "--------"
                        "--------"
                        "--------"
                        "--------"
                        "pppppppp"
                        "rnbqkbnr")))

(: new-game : ChessGame)
;;This value contains a starting board and empty move history.
(define new-game (ChessGame starting-board '()))

(: player? : Player -> Integer)
;;return 1 for 'White, -1 for 'Black
(define (player? player)
  (if (symbol=? player 'White) 1 -1))
(check-expect (player? 'White) 1)
(check-expect (player? 'Black) -1)

(: move->str (Move -> String))
;; build a string version of the move, for purposes of comparison
;; note: there is a bijection between moves and strings (and must be)
(define (move->str m)
  (match m
    [(Move src dst moved captured promote-to)
     (pipes (list "Move"
                  (loc->str src)
                  (loc->str dst)
                  (piece->str moved)
                  (get-opt (opt-map piece->str captured) "None")
                  (get-opt (opt-map symbol->string promote-to) "None")))]))

(: loc->str (Loc -> String))
;; return string representation of location
(define (loc->str loc)
  (match loc
    [(Loc f r)
     (string-append (symbol->string f) (number->string r))]))

(: piece->str (Piece -> String))
;; return string representation of piece
(define (piece->str p)
  (match p
    [(Piece t pl)
     (string-append "Piece:"
                    (symbol->string t)
                    ","
                    (symbol->string pl))]))

(: pipes ((Listof String) -> String))
;; connect strings with | character in between
;; ex: (pipes (list "a" "bb" "ccc")) ==> "a|bb|ccc"
(define (pipes ss)
  (match ss
    ['() ""]
    [(list s) s]
    [(cons s r) (string-append s "|" (pipes r))]))

(: move<? (Move Move -> Boolean))
;; move comparison for the purposes of sorting
(define (move<? m1 m2)
  (string<? (move->str m1) (move->str m2)))

(: loc<? (Loc Loc -> Boolean))
;; move comparison for the purposes of sorting
(define (loc<? loc1 loc2)
  (string<? (loc->str loc1) (loc->str loc2)))

(: moves->strings : (Listof Move) -> (Listof String))
;;for a list of moves and return the sorted list of strings built from this list
(define (moves->strings ms)
  (map move->str (sort ms move<?)))
(check-expect (moves->strings (list (Move (Loc 'A 2)
                                          (Loc 'B 2)
                                          (Piece 'Pawn 'White)
                                          'None 'None)
                                    (Move (Loc 'C 2)
                                          (Loc 'D 5)
                                          (Piece 'Pawn 'White)
                                          'None 'None)))
                              (list "Move|A2|B2|Piece:Pawn,White|None|None"
                                    "Move|C2|D5|Piece:Pawn,White|None|None"))

(: locs->strings : (Listof Loc) -> (Listof String))
;;for a list of locs and return the sorted list of strings built from this list
(define (locs->strings ls)
  (map loc->str (sort ls loc<?)))
(check-expect (locs->strings (list (Loc 'A 2) (Loc 'B 2))) (list "A2" "B2"))
(check-expect (locs->strings (list (Loc 'B 5) (Loc 'C 1))) (list "B5" "C1"))
     
(: loc-to-index : Loc -> Integer)
;;return the index in board corresponding to the loc
(define (loc-to-index loc)
  (match loc
    [(Loc f r) (+ (* (- r 1) 8) (index-of symbol=? file f))]))
(check-expect (loc-to-index (Loc 'A 2)) 8)
(check-expect (loc-to-index (Loc 'B 2)) 9)
(check-expect (loc-to-index (Loc 'B 3)) 17)


(: index-to-loc : Integer -> Loc)
;;return the loc corresponding to the index
(define (index-to-loc n)
  (Loc (list-ref file (remainder n 8))
       (list-ref rank (quotient n 8))))
(check-expect (index-to-loc 8) (Loc 'A 2))
(check-expect (index-to-loc 9) (Loc 'B 2))
(check-expect (index-to-loc 17) (Loc 'B 3))


(: loc=? : Loc Loc -> Boolean) 
;;check if two moves are the same
(define (loc=? l1 l2)
  (match* (l1 l2)
    [((Loc f1 r1) (Loc f2 r2))
     (and (symbol=? f1 f2)
          (= r1 r2))]))
(check-expect (loc=? (Loc 'A 1) (Loc 'A 1)) #t)
(check-expect (loc=? (Loc 'A 1) (Loc 'B 1)) #f)

(: piece=? : Piece Piece -> Boolean)
;;check if two pieces are the same
(define (piece=? p1 p2)
  (match* (p1 p2)
    [((Piece type1 player1) (Piece type2 player2))
     (and (symbol=? type1 type2)
          (symbol=? player1 player2))]))
(check-expect (piece=? (Piece 'Knight 'Black)(Piece 'Pawn 'Black)) #f)
(check-expect (piece=? (Piece 'Pawn 'Black) (Piece 'Pawn 'Black)) #t)

(: some=? : All (A) (Optional A) (Optional A) (A A -> Boolean) -> Boolean)
;;check if two optional of same type are the same
(define (some=? x1 x2 fx)
  (match* (x1 x2)
    [('None 'None) #t]
    [('None _) #f]
    [(_ 'None) #f]
    [((Some a1) (Some a2)) (fx a1 a2)]))
(check-expect (some=? (Some 2) (Some 4) =) #f)
(check-expect (some=? (Some 2) 'None =) #f)
(check-expect (some=? 'None 'None =) #t)


(: move=? : Move Move -> Boolean)
;;check if two moves are the same
(define (move=? m1 m2)
  (match* (m1 m2)
    [((Move src1 dst1 moved1 captured1 promote-to1)
     (Move src2 dst2 moved2 captured2 promote-to2))
     (and (loc=? src1 src2)
          (loc=? dst1 dst2)
          (some=? captured1 captured2 piece=?)
          (some=? promote-to1 promote-to2 symbol=?))]))

(: board-ref : Board Loc -> Square)
;;returns the contents of the specified square in the board.
(define (board-ref b loc)
     (list-ref b (loc-to-index loc)))
(check-expect (board-ref starting-board (Loc 'B 2)) (Some (Piece 'Pawn 'White)))
(check-expect (board-ref (ChessGame-board test1) (Loc 'A 3)) (Some (Piece 'King 'Black)))
(check-expect (board-ref (ChessGame-board test1) (Loc 'D 7)) 'None)


(: board-update : Board Loc Square -> Board)
;;returns an updated board where the contents of the specified square are replaced with the given value.
(define (board-update b loc s)
  (match loc
    [(Loc f r)
     (local {(define l (loc-to-index loc))
             (: fx : (Listof Square) Integer -> (Listof Square))
             (define (fx x y)
                         (match x
                           [(cons first rest) (if (= y 0)
                                                  (cons s rest)
                                                  (cons first (fx rest (- y 1))))]))}
       (fx b l))]))
(check-expect (board-update (ChessGame-board test1) (Loc 'B 2) (Some (Piece 'Pawn 'Black))) (ChessGame-board test3))
(check-expect (board-update (ChessGame-board test2) (Loc 'G 3) 'None) (ChessGame-board test4))



(: direct : Loc Direct -> Position)
;;take an loc and (Direct x y)
;;return the position of loc moved right by x up by y, 'None if not exist
(define (direct loc p)
  (match* (loc p)
    [((Loc f r) (Direct x y))
     (local {(define f1 (+ x (index-of symbol=? file f)))
             (define r1  (+ y r))}
       (if (or (not (<= 1 r1 8)) (not (<= 0 f1 7)))
           'None
           (Some (Loc (list-ref file f1)
                      (list-ref rank (sub1 r1))))))]))
(check-expect (direct (Loc 'B 5) (Direct 1 1)) (Some (Loc 'C 6)))
(check-expect (direct (Loc 'B 8) (Direct 1 1)) 'None)
(check-expect (direct (Loc 'B 7) (Direct 1 1)) (Some (Loc 'C 8)))



(: same-color? : Board Player Loc (Listof Loc) (Listof Loc)-> (Listof Loc))
;;return '() if the piece has same color
;;return default1 if not same color or
;;return default2 value if there's no piece at loc
(define (same-color? b player loc default1 default2)
  (match (list-ref b (loc-to-index loc))
    ['None default2]
    [(Some (Piece _ player1))
     (if (symbol=? player player1)
         '()
         default1)]))
(check-expect (same-color? (ChessGame-board test2) 'White (Loc 'A 8) (list (Loc 'A 8)) '()) (list (Loc 'A 8))) 
(check-expect (same-color? (ChessGame-board test2) 'White (Loc 'A 7) '() (list (Loc 'A 7))) (list (Loc 'A 7))) 
(check-expect (same-color? (ChessGame-board test2) 'White (Loc 'A 5) (list (Loc 'A 8)) '()) '()) 


(: direct-line : Board Player Loc Direct -> (Listof Loc))
;;return the line (list of loc) in the given direction of the input loc
;;this line ends before a piece with same color as given player
(define (direct-line b player loc d)
  (match d
    [(Direct x y)
     (local {(define n-loc (direct loc d))}
       (match n-loc
         ['None '()]
         [(Some loc1) (match (list-ref b (loc-to-index loc1))
                        ['None (append (list loc1) (direct-line b player loc1 d))]
                        [_ (same-color? b player loc1 (list loc1) (list loc1))])]))]))
(check-expect (direct-line (ChessGame-board test1) 'White (Loc 'E 7) (Direct 0 -1)) (list (Loc 'E 6) (Loc 'E 5)))
(check-expect (direct-line (ChessGame-board test1) 'White (Loc 'B 4) (Direct -1 -1)) (list (Loc 'A 3)))
(check-expect (direct-line (ChessGame-board test1) 'White (Loc 'B 1) (Direct -1 -1)) '())


(: locs-rook : Board Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a rook of a given player
(define (locs-rook b player loc)
  (append (direct-line b player loc (Direct 1 0))
          (direct-line b player loc (Direct 0 1))
          (direct-line b player loc (Direct -1 0))
          (direct-line b player loc (Direct 0 -1))))
(check-expect (locs->strings (locs-rook (ChessGame-board test1) 'White (Loc 'E 7)))
              (sort (list "E8" "D7" "C7" "F7" "G7" "H7" "E6" "E5") string<?))
(check-expect (locs->strings (locs-rook (ChessGame-board test1) 'Black (Loc 'E 5)))
              (sort (list "D5" "F5" "G5" "E6" "E7" "E4" ) string<?))

(: locs-bishop : Board Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a bishop of given player
(define (locs-bishop b player loc)
  (append (direct-line b player loc (Direct 1 -1))
          (direct-line b player loc (Direct 1 1))
          (direct-line b player loc (Direct -1 1))
          (direct-line b player loc (Direct -1 -1))))
(check-expect (locs->strings (locs-bishop (ChessGame-board test2) 'Black (Loc 'A 8)))
              (sort (list "B7" "C6" "D5" "E4") string<?))
(check-expect (locs->strings (locs-bishop (ChessGame-board test2) 'White (Loc 'F 1)))
              (sort (list "G2" "H3" "E2" "D3" "C4" "B5" "A6") string<?))


(: locs-queen : Board Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a queen of given player
(define (locs-queen b player loc)
  (append (locs-bishop b player loc) (locs-rook b player loc)))
(check-expect (locs->strings (locs-queen (ChessGame-board test2) 'White (Loc 'B 6)))
              (sort (list "A6" "A7" "B3" "B4" "B5" "B7" "B8" "C5" "C6" "C7" "D4" "E3") string<?))
(check-expect (locs->strings (locs-queen (ChessGame-board test2) 'Black (Loc 'D 1)))
              (sort (list "A1" "B1" "C1" "D2" "D3" "D4" "D5" "D6" "E1" "E2" "F1" "F3" "G4" "H5") string<?))


(: append-direct-loc : Board Player Loc -> (Direct (Listof Loc)-> (Listof Loc)))
;;given a player and loc
;;return a function that takes in a direct and a list of loc and append to the list
;;'() if the new loc got from direct has the same color piece or the new loc doesn't exist
;;a singleton list of the new loc if the new loc is empty or has different color piece
(define (append-direct-loc b player  loc)
  (lambda ([x : Direct] [y : (Listof Loc)])
    (match (direct loc x)
      ['None y]
      [(Some new-loc)
       (local {(define the-square (board-ref b new-loc))}
         (match the-square
           ['None (append (list new-loc) y)]
           [(Some (Piece _ pl))
            (if (symbol=? player pl) y (append (list new-loc) y))]))])))
(check-expect ((append-direct-loc (ChessGame-board test2) 'White  (Loc 'D 6)) (Direct 2 0) '())
              '())

         
(: locs-knight : Board Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a knight of given player
(define (locs-knight b player loc) 
                (foldl  (append-direct-loc b player loc) 
               '()
               (list (Direct 1 2) (Direct -1 2) (Direct 1 -2) (Direct -1 -2)
                     (Direct 2 1) (Direct -2 1) (Direct 2 -1) (Direct -2 -1))))
(check-expect (locs->strings (locs-knight (ChessGame-board test2) 'White (Loc 'E 4)))
              (sort (list "C3" "C5" "D2" "F2") string<?))
(check-expect (locs->strings (locs-knight (ChessGame-board test2) 'Black (Loc 'C 2)))
              (sort (list "A1" "B4" "D4" "E1") string<?))

(: locs-king-capture : Board Player Loc -> (Listof Loc))
;;return the list of locations that can be captured by a king of given player
(define (locs-king-capture b player loc)
  (foldl  (append-direct-loc b player loc) 
               '()
               (list (Direct 1 1) (Direct -1 1) (Direct 1 -1) (Direct -1 -1)
                     (Direct 1 0) (Direct -1 0) (Direct 0 -1) (Direct 0 1))))
(check-expect (locs->strings (locs-king-capture (ChessGame-board test2) 'Black (Loc 'A 3)))
              (sort (list "A2" "B2" "B3" "B4") string<?))
(check-expect (locs->strings (locs-king-capture (ChessGame-board test2) 'White (Loc 'D 6)))
              (sort (list "C5" "C6" "C7" "D5" "D7" "E5" "E6") string<?))

(: locs-pawn-capture : Board Player Loc -> (Listof Loc))
;;return the list of locations that can be captured by a pawn of given player
(define (locs-pawn-capture b player loc)
  (local {(define k (player? player))
          (define loc1 (direct loc (Direct 1 (* k 1))))
          (define loc2 (direct loc (Direct -1 (* k 1))))
          (define (fx [x : (Optional Loc)])
            (match x
              [(Some loc) (same-color? b player loc (list loc) '())]
              [_ '()]))}
    (append (fx loc1) (fx loc2))))
(check-expect (locs->strings (locs-pawn-capture starting-board 'White (Loc 'A 2)))
              '())
(check-expect (locs->strings (locs-pawn-capture (ChessGame-board test1) 'Black (Loc 'C 7)))
              (list "B6"))


(: en-passant : ChessGame Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a pawn through en-passant (if possible)
(define (en-passant game p l)
  (match game
    [(ChessGame b h)
     (match h
       [(cons (Move (Loc f1 r1) (Loc f2 r2) (Piece 'Pawn c) _ _) _)
        (if
         (and (symbol=? c (other-player p))
              (= (abs (- r1 r2)) 2)
              (= (abs (- (loc-to-index l) (loc-to-index (Loc f2 r2)))) 1))                              
         (cond
           [(and (symbol=? p 'White) (= 5 (Loc-rank l)))
            (match (board-ref b (Loc f2 (cast (add1 r2) Rank)))
              ['None (list (Loc f2 (cast (add1 r2) Rank)))]
              [_ '()])]
           [(and (symbol=? p 'Black) (= 4 (Loc-rank l))) 
            (match (board-ref b (Loc f2 (cast (sub1 r2) Rank)))
              ['None (list (Loc f2 (cast (sub1 r2) Rank)))]
              [_ '()])]
           [else '()])
         '())]
       [_ '()])]))
(check-expect (en-passant test9 'White (Loc 'D 5)) (list (Loc 'E 6)))
(check-expect (en-passant test9 'White (Loc 'C 5)) '())
              

(: locs-pawn-move : ChessGame Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a pawn of given player(including capture)
(define (locs-pawn-move game player loc)
  (match game
    [(ChessGame b h)
     (local {(define k (player? player))
             (define two-steps? (+ 2 (* (- 1 k) 5/2)))
             (define loc1 (direct loc (Direct 0 (* k 1))))
             (define (hx [x : (Optional Loc)])
               (match x
                 ['None '()]
                 [(Some loc) (same-color? b player loc '() (list loc))]))}
       (match loc
         [(Loc _ r)
          (append
           (if (= two-steps? r)
               (append (locs-pawn-capture b player loc)
                       (hx loc1)
                       (hx (direct loc (Direct 0 (* k 2)))))
               (append (locs-pawn-capture b player loc) (hx loc1)))
           (en-passant game player loc))]))]))  
(check-expect (locs->strings (locs-pawn-move (ChessGame starting-board '()) 'White (Loc 'A 2)))
              (list "A3" "A4"))
(check-expect (locs->strings (locs-pawn-move test1 'Black (Loc 'C 7)))
              (sort (list "B6" "C6" "C5") string<?))

  
(: loc-to-move : Piece Board Loc -> (Loc -> (Listof Move)))
;;take in a piece, src loc and Board,
;;return a function that will take in a dst loc return a list of moves 
;;handle the case of promotion, i.e. will return a list of four promotion moves if pawn touches endline
(define (loc-to-move p b src)
  (lambda ([x : Loc])
    (match p
      [(Piece 'Pawn color)
       (if
        (or (and (symbol=? color 'White) (= (Loc-rank x) 8))
            (and (symbol=? color 'Black) (= (Loc-rank x) 1)))            
        (list (Move src x p (list-ref b (loc-to-index x)) (Some 'Queen))
              (Move src x p (list-ref b (loc-to-index x)) (Some 'Knight))
              (Move src x p (list-ref b (loc-to-index x)) (Some 'Bishop))
              (Move src x p (list-ref b (loc-to-index x)) (Some 'Rook)))
        (match (board-ref b x)
          ['None (if (symbol=? (Loc-file src) (Loc-file x))
                     (list (Move src x p 'None 'None))
                     (list (Move src x p (Some (Piece 'Pawn (other-player color))) 'None)))]
          [eaten (list (Move src x p eaten 'None))]))]        
      [_ (list (Move src x p (board-ref b  x) 'None))])))

(check-expect ((loc-to-move (Piece 'King 'White) starting-board (Loc 'A 3)) (Loc 'A 4))
              (list (Move (Loc 'A 3) (Loc 'A 4) (Piece 'King 'White) 'None 'None)))
(check-expect ((loc-to-move (Piece 'Pawn 'White) starting-board (Loc 'A 7)) (Loc 'B 8))
              (list (Move (Loc 'A 7) (Loc 'B 8) (Piece 'Pawn 'White) (Some (Piece 'Knight 'Black)) (Some 'Queen))
                    (Move (Loc 'A 7) (Loc 'B 8) (Piece 'Pawn 'White) (Some (Piece 'Knight 'Black)) (Some 'Knight))
                    (Move (Loc 'A 7) (Loc 'B 8) (Piece 'Pawn 'White) (Some (Piece 'Knight 'Black)) (Some 'Bishop))
                    (Move (Loc 'A 7) (Loc 'B 8) (Piece 'Pawn 'White) (Some (Piece 'Knight 'Black)) (Some 'Rook))))
(check-expect ((loc-to-move (Piece 'Pawn 'White) (ChessGame-board test9) (Loc 'D 5)) (Loc 'E 6))
              (list (Move (Loc 'D 5) (Loc 'E 6) (Piece 'Pawn 'White) (Some (Piece 'Pawn 'Black)) 'None)))  


   
(: locs-piece-capture : ChessGame Loc -> (Listof Loc))
;;Given a game and a particular piece (identified by its location),
;;give a list of locs where another piece of different color can be legally captured by this piece
;;If none, return the empty list.
(define (locs-piece-capture game loc)
  (match game 
    [(ChessGame b _)
    (match (get-opt (list-ref b (loc-to-index loc)) 'None)
      [(Piece 'King player) (locs-king-capture  b player loc)]
      [(Piece 'Knight player) (locs-knight b player loc)]
      [(Piece 'Pawn player) (locs-pawn-capture b player loc)]
      [(Piece 'Queen player) (locs-queen b player loc)]
      [(Piece 'Rook player) (locs-rook b player loc)]
      [(Piece 'Bishop player) (locs-bishop b player loc)]
      [_ '()])]))
(check-expect (locs->strings (locs-piece-capture test1 (Loc 'C 7)))
              (list "B6"))
(check-expect (locs->strings (locs-piece-capture  test2 (Loc 'A 3)))
              (sort (list "A2" "B2" "B3" "B4") string<?))
(check-expect (locs->strings (locs-piece-capture test2  (Loc 'D 1)))
              (sort (list "A1" "B1" "C1" "D2" "D3" "D4" "D5" "D6" "E1" "E2" "F1" "F3" "G4" "H5") string<?))


(: other-player : Player -> Player)
;;return the other player
(define (other-player player)
  (match player
    ['Black 'White]
    ['White 'Black]))
(check-expect (other-player 'White) 'Black)
(check-expect (other-player 'Black) 'White)


(: same-square? : Square Square -> Boolean)
;;check if two Squares are the same
(define (same-square? s1 s2)
  (local {(define p1 (get-opt s2 (Piece 'Rook 'White)))
          (define p2 (get-opt s1 (Piece 'Rook 'White)))}
    (match* (p1 p2)
      [((Piece type1 player1) (Piece type2 player2))
       (and (symbol=? type1 type2)
            (symbol=? player1 player2))])))


(: same-loc? : Loc Loc -> Boolean)
;;check if two Squares are the same
(define (same-loc? l1 l2)
    (match* (l1 l2)
      [((Loc file1 rank1) (Loc file2 rank2))
       (and (symbol=? file1 file2)
            (= rank1 rank2))]))

(: turn? : (Listof Move) -> Player)
;;determine whose turn it is now
(define (turn? ms)
  (match ms
    ['() 'White]
    [(cons (Move _ _ (Piece _ player) _ _) rest)
     (other-player player)]))
(check-expect (turn? '()) 'White)
(check-expect (turn? (ChessGame-history test1)) 'White)


(: in-check? : ChessGame -> Boolean)
;;This function returns true, if and only if the player whose turn it is is in check
(define (in-check? game)
  (match game
    [(ChessGame b history)
     (local {(define player (turn?  history))
             (define previous-player  (other-player player))
             (define king-loc
               (index-to-loc (index-of same-square? b (Some (Piece 'King player)))))}
       (ormap (lambda ([x : Integer])
                (match (list-ref b x)
                  ['None #f]
                  [(Some (Piece t p))
                   (if (symbol=? p previous-player)
                       (not (= -1 (index-of same-loc? (locs-piece-capture game (index-to-loc x)) king-loc)))
                       #f)]))
              (build-list 64 (lambda ([x : Integer]) x))))]))
(check-expect (in-check? test1) #t)                           
(check-expect (in-check? test2) #f)
(check-expect (in-check? test6) #f)



(: castling : ChessGame Player Loc -> (Listof Loc))
;;return locs king can castle to
;;not take into account the possibility of castling into check (will check this in locs-king-move)
(define (castling g p l)
  (match g
    [(ChessGame b h)
      (local {(define index (loc-to-index l))
              (define l-rook (get-opt (board-ref b (index-to-loc (abs (- index 4))))
                                      (Piece 'Knight 'White)))
              (define r-rook (get-opt (board-ref b (index-to-loc (+ index 3)))
                                      (Piece 'Knight 'White)))
              (define k-moved?
               (ormap (lambda ([x : Move])
                  (match x
                    [(Move _ _ moved _ _)
                     (and (symbol=? (Piece-type moved) 'King)
                          (symbol=? (Piece-color moved) p))]))
                h))
              (define k-move-left?
               (and
                (symbol=? (Piece-type l-rook) 'Rook)
                (symbol=? (Piece-color l-rook) p)
                (not (ormap (lambda ([x : Move])
                              (match x
                                [(Move src _ moved _ _)
                                 (and (symbol=? (Piece-type moved) 'Rook)
                                      (loc=? src (index-to-loc (abs (- index 4)))))])) h))))
             (define k-move-right?
               (and
                (symbol=? (Piece-type r-rook) 'Rook)
                (symbol=? (Piece-color r-rook) p)
                (not (ormap (lambda ([x : Move])
                              (match x
                                [(Move src _ moved _ _)
                                 (and (symbol=? (Piece-type moved) 'Rook)
                                      (loc=? src (index-to-loc (+ index 3))))])) h))))}
        (if k-moved?
            '()
            (filter
             (lambda ([x : Loc])
               (match* ((board-ref b x)
                        (board-ref b (index-to-loc (exact-ceiling
                                                    (/ (+
                                                        (loc-to-index x)
                                                        (loc-to-index l))
                                                       2)))))
                 [('None 'None) #t]
                 [(_ _) #f]))
             (if k-move-right?
                 (if k-move-left?
                     (list (index-to-loc (+ 2 (loc-to-index l)))
                           (index-to-loc (abs (- 2 (loc-to-index l)))))
                     (list (index-to-loc (+ 2 (loc-to-index l)))))
                 (if k-move-left?
                     (list (index-to-loc (abs (- 2 (loc-to-index l)))))
                     '())))))]))             
(check-expect (castling test8 'White (Loc 'E 1)) '())  
(check-expect (castling test8 'Black (Loc 'E 8)) (list (Loc 'C 8)))
                       


(: locs-king-move : ChessGame Player Loc -> (Listof Loc))
;;return the list of locations that can be legally moved to by a king of given player
;;player should not move king into check
(define (locs-king-move g player loc)
  (match g
    [(ChessGame b h)
     (local {(define fakemove (list (Move
                                  (Loc 'A 1) (Loc 'A 1)
                                  (Piece 'Rook (other-player player))
                                  'None 'None)))
          (: fx : Board -> Boolean)
          (define (fx x)
            (in-check? (ChessGame x fakemove)))
          (define b1 (board-update b loc 'None))}
    (append
     (filter (lambda ([x : Loc])
              (not (fx (board-update b1 x
                                     (Some (Piece 'King player))))))
            (locs-king-capture b player loc))
     (filter (lambda ([x : Loc])
               (not (or 
                     (fx (board-update b1 x
                                       (Some (Piece 'King player))))
                     (fx (board-update b1
                                       (index-to-loc
                                       (exact-ceiling (/ (+ (loc-to-index x)
                                              (loc-to-index loc))
                                           2)))
                                       (Some (Piece 'King player)))))))
      (castling g player loc))))]))



(: moves-piece : ChessGame Loc -> (Listof Move))
;;give a list of moves that can legally be made with that piece.
;;i think testing legal-move? can 
(define (moves-piece game loc)
  (match game
    [(ChessGame b history)                               
     (local {(define the-piece (get-opt (list-ref b (loc-to-index loc)) 'None))}
       (match the-piece
         ['None '()]
         [(Piece m n)
          (if (symbol=? (turn? history) n)
          (local
            {(define player (turn? history))
             (define fakehistory (list (Move
                                        (Loc 'A 1) (Loc 'A 1)
                                        (Piece 'Rook (other-player player))
                                        'None 'None)))
             (: fx : Board -> Boolean)
             (define (fx x)
               (in-check? (ChessGame x fakehistory)))
             (: gx : Piece -> (Listof Loc))
             (define (gx [x : Piece])
               (match x
                 [(Piece 'King player)  (locs-king-move game player loc)]
                 [(Piece 'Knight player)  (locs-knight b player loc)]
                 [(Piece 'Pawn player)  (locs-pawn-move game player loc)]
                 [(Piece 'Queen player)  (locs-queen b player loc)]
                 [(Piece 'Rook player)  (locs-rook b player loc)]
                 [(Piece 'Bishop player)  (locs-bishop b player loc)]
                 [_ '()]))
             (define locs (gx (Piece m n)))
             (define b1 (board-update b loc 'None))}
            (foldl (lambda ([x : Loc] [y : (Listof Move)])
                     (append ((loc-to-move (Piece m n) b loc) x)
                             y))
                 '()
                 (filter (lambda ([x : Loc])
                           (not (fx (board-update b1 x
                                                  (Some (Piece m n))))))
                         locs)))
          '())]))]))

(: legal-move? : ChessGame Move -> Boolean)
;;Return true if, and only if, the proposed move, is a legal move, according to the rules of chess
(define (legal-move? game m)
  (if (= -1 (index-of move=? (moves-piece game (Move-src m)) m))
      #f
      #t))

(check-expect (legal-move? test1 (Move (Loc 'D 5) (Loc 'D 6) (Piece 'King 'White) 'None 'None)) #f)
(check-expect (legal-move? test1 (Move (Loc 'D 5) (Loc 'E 5) (Piece 'King 'White) (Some (Piece 'Rook 'Black)) 'None)) #t)
(check-expect (legal-move? test1 (Move (Loc 'A 1) (Loc 'A 2) (Piece 'King 'Black) 'None 'None)) #f)
(check-expect (legal-move? test1 (Move (Loc 'B 2) (Loc 'B 3) (Piece 'Pawn 'White) 'None 'None)) #f)
(check-expect (legal-move? test2 (Move (Loc 'C 7) (Loc 'C 6) (Piece 'Pawn 'Black) 'None 'None)) #t)
(check-expect (legal-move? test2 (Move (Loc 'C 7) (Loc 'C 5) (Piece 'Pawn 'Black) 'None 'None)) #t)
(check-expect (legal-move? test2 (Move (Loc 'C 7) (Loc 'B 6) (Piece 'Pawn 'Black) (Some (Piece 'Queen 'White)) 'None)) #t)
(check-expect (legal-move? test2 (Move (Loc 'G 6) (Loc 'F 6) (Piece 'Rook 'Black) (Some (Piece 'Rook 'White)) 'None)) #t)
(check-expect (legal-move? test2 (Move (Loc 'G 6) (Loc 'G 5) (Piece 'Rook 'Black) (Some (Piece 'Pawn 'White)) 'None)) #t)
(check-expect (legal-move? test2 (Move (Loc 'A 8) (Loc 'E 4) (Piece 'Bishop 'Black) (Some (Piece 'Knight 'White)) 'None)) #t)
(check-expect (legal-move? test2 (Move (Loc 'C 2) (Loc 'E 3) (Piece 'Knight 'Black) (Some (Piece 'Rook 'Black)) 'None)) #f)
(check-expect (legal-move? test2 (Move (Loc 'C 2) (Loc 'A 1) (Piece 'Knight 'Black) 'None 'None)) #t)



(: apply-move : ChessGame Move -> ChessGame)
;;Make the specified move for the player whose turn it is, modifying the board accordingly.
;;will handle special conditions like: en-passant, promotion, castling
(define (apply-move game m)
  (match game
    [(ChessGame b history)
     (if (not (legal-move? game m))
         game
         (match m
           [(Move src dst p captured promote)
            (local
              {(: fx : (Optional PromoteTo) -> ChessGame)
               (define (fx x)
                 (match x
                   ['None (ChessGame (board-update (board-update b dst (Some p)) src 'None)
                                     (cons m history))]
                   [(Some promote)
                    (ChessGame
                     (board-update (board-update b dst (Some (Piece promote (Piece-color p)))) src 'None)
                     (cons m history))]))}
              (match p
                [(Piece 'King color)
                 (cond
                   [(= 2 (- (loc-to-index src) (loc-to-index dst)))
                    (ChessGame
                     (board-update
                      (board-update (board-update (board-update b dst (Some p)) src 'None)
                                    (get-opt (direct dst (Direct 1 0)) (Loc 'A 1))
                                    (Some (Piece 'Rook color)))
                      (get-opt (direct src (Direct -4 0)) (Loc 'A 1)) 'None) (cons m history))]
                   [(= -2 (- (loc-to-index src) (loc-to-index dst)))
                    (ChessGame
                     (board-update
                      (board-update (board-update (board-update b dst (Some p)) src 'None)
                                    (get-opt (direct dst (Direct -1 0)) (Loc 'A 1)) (Some (Piece 'Rook color)))
                      (get-opt (direct src (Direct 3 0)) (Loc 'A 1)) 'None)                                  
                     (cons m history))]
                   [else (ChessGame (board-update (board-update b dst (Some p)) src 'None)
                                    (cons m history))])]
                [(Piece 'Pawn color)
                 (match (board-ref b dst)
                   ['None (if (symbol=? (Loc-file src) (Loc-file dst))
                              (fx promote)
                              (if (symbol=? color 'White)
                                  (ChessGame (board-update
                                              (board-update (board-update b dst (Some p)) src 'None)
                                              (Loc (Loc-file dst)
                                                   (cast (sub1 (Loc-rank dst)) Rank))
                                              'None)
                                    (cons m history))
                                  (ChessGame (board-update
                                              (board-update (board-update b dst (Some p)) src 'None)
                                              (Loc (Loc-file dst)
                                                   (cast (add1 (Loc-rank dst)) Rank))
                                              'None)
                                    (cons m history))))]
                   [_ (fx promote)])]
                [_ (ChessGame (board-update (board-update b dst (Some p)) src 'None)
                              (cons m history))]))]))]))
(check-expect (apply-move test8 (Move (Loc 'E 8) (Loc 'C 8) (Piece 'King 'Black) 'None 'None)) test10)
(check-expect (apply-move test8 (Move (Loc 'E 7) (Loc 'C 8) (Piece 'King 'Black) 'None 'None)) test8)
(check-expect (apply-move test9 (Move (Loc 'D 5) (Loc 'E 6) (Piece 'Pawn 'White) (Some (Piece 'Pawn 'Black)) 'None)) test11)
(check-expect (apply-move test12 (Move (Loc 'C 7) (Loc 'C 8) (Piece 'Pawn 'White) 'None (Some 'Queen))) test13)
                                        
            
(: moves-player : ChessGame -> (Listof Move))
;;For the player whose turn it currently is, determine a list of all legal moves that player could make. 
(define (moves-player game)
  (match game
    [(ChessGame b history)
     (local {(define player (turn? history))}
            (filter
             (lambda ([x : Move]) (legal-move? game x))
             (foldl (lambda ([x : Integer] [y : (Listof Move)])
                          (local {(define loc (index-to-loc x))}
                          (match (board-ref b loc)
                            ['None y]
                            [(Some (Piece t pl))
                             (if
                              (symbol=? player pl)
                              (append (moves-piece game loc) y)
                              y)])))
                  '()
                 (build-list 64 (lambda ([x : Integer]) x)))))]))
(check-expect (moves->strings (moves-player (ChessGame starting-board '())))
              '("Move|A2|A3|Piece:Pawn,White|None|None" "Move|A2|A4|Piece:Pawn,White|None|None"
              "Move|B1|A3|Piece:Knight,White|None|None" "Move|B1|C3|Piece:Knight,White|None|None"
              "Move|B2|B3|Piece:Pawn,White|None|None" "Move|B2|B4|Piece:Pawn,White|None|None"
              "Move|C2|C3|Piece:Pawn,White|None|None" "Move|C2|C4|Piece:Pawn,White|None|None"
              "Move|D2|D3|Piece:Pawn,White|None|None" "Move|D2|D4|Piece:Pawn,White|None|None"
              "Move|E2|E3|Piece:Pawn,White|None|None" "Move|E2|E4|Piece:Pawn,White|None|None"
              "Move|F2|F3|Piece:Pawn,White|None|None" "Move|F2|F4|Piece:Pawn,White|None|None"
              "Move|G1|F3|Piece:Knight,White|None|None" "Move|G1|H3|Piece:Knight,White|None|None"
              "Move|G2|G3|Piece:Pawn,White|None|None" "Move|G2|G4|Piece:Pawn,White|None|None"
              "Move|H2|H3|Piece:Pawn,White|None|None" "Move|H2|H4|Piece:Pawn,White|None|None"))
(check-expect (moves->strings (moves-player test1))
               '("Move|D5|C4|Piece:King,White|None|None"
                 "Move|D5|C6|Piece:King,White|None|None"
                 "Move|D5|D4|Piece:King,White|None|None"
                 "Move|D5|E5|Piece:King,White|Piece:Rook,Black|None"
                 "Move|E7|E5|Piece:Rook,White|Piece:Rook,Black|None"))





(: stalemate? : ChessGame -> Boolean)
;;Return true if and only if the player whose turn it currently is, is not in check but yet has no available moves.
(define (stalemate? game)
  (match (moves-player game)
    ['() (not (in-check? game))]
    [_ #f]))
(check-expect (stalemate? test6) #t)
(check-expect (stalemate? test5) #f)


(: checkmate? : ChessGame -> Boolean)
;;Return true if and only if the player whose turn it currently is, is checkmated.
(define (checkmate? game)
  (match (moves-player game)
    ['() (in-check? game)]
    [_ #f]))
(check-expect (checkmate? test7) #t)
(check-expect (checkmate? test6) #f)


                             
                             
                                   
                           

                                                                     


    

  


         





                          
 
(test)
