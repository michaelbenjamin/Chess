data Piece = Piece Color Type | Nothing
data Color = White | Black
instance Eq Color where
    White == White = True
    Black == Black = True
    _ ==  _        = False
data Type = King | Queen | Rook | Bishop | Knight | Pawn

type Position = (Int,Int)
data Square = Square Position Piece
    
startBoard :: [Square]
startBoard = undefined


kingMove (x,y) =   [(x2,y2) | (x2,y2)  <- [(x+1,y),(x+1,y+1),(x+1,y-1),(x,y+1),
                    (x,y-1),(x-1,y),(x-1,y+1),(x-1,y)], 
                     x2 >= 1 && x2 <= 8 && y2 >= 1 && y2 <= 8]
bishopMove (x,y) = moveNE (x,y) ++ moveNW (x,y) ++ moveSE (x,y) ++ moveSW (x,y)
knightMove (x,y) = [(x2,y2) | (x2,y2) <- [(x+2,y+1),(x+2,y-1),(x+1,y+2),
                    (x-1,y+2),(x-2,y+1),(x-2,y-1),(x-1,y-2),(x+1,y+2)], 
                     x2 >= 1 && x2 <= 8 && y2 >= 1 && y2 <= 8]
rookMove (x,y)  = moveN (x,y) ++ moveS (x,y) ++ moveE (x,y) ++ moveW (x,y)
queenMove (x,y) = bishopMove (x,y) ++ rookMove (x,y)
pawnMove (x,y) = undefined


moveN (x,y) = [(x,y+z) | z <- [1..8], (y+z) <= 8]
moveS (x,y) = [(x,y-z) | z <- [1..8], y-z >= 1]
moveE (x,y) = [(x+z,y) | z <- [1..8], x+z <= 8]
moveW (x,y) = [(x-z,y) | z <- [1..8], x-z >= 1]

moveNE (x,y) = [(x+z,y+z) | z <- [1..8], x+z <= 8]
moveNW (x,y) = [(x+z,y-z) | z <- [1..8], x+z <= 8 && y-z >= 1]
moveSE (x,y) = [(x-z,y+z) | z <- [1..8], x-z >= 1 && y+z <= 8]
moveSW (x,y) = [(x-z,y-z) | z <- [1..8], x-z >= 1]

checkIfCanMove :: [Position] -> Color -> [Square] -> [Position]
checkIfCanMove [] _ _ = []
checkIfCanMove (x:xs) White pos = case checkIfPiece x pos of 
                                    Main.Nothing  -> x:checkIfCanMove xs White pos  
                                    Piece White _ -> []
                                    otherwise     -> [x]
checkIfCanMove (x:xs) Black pos = case checkIfPiece x pos of 
                                    Main.Nothing -> x:checkIfCanMove xs Black pos  
                                    Piece White _ -> [x]
                                    otherwise     -> []  
--This is a look up in O(n) and can be done in O(1)
checkIfPiece :: Position -> [Square] -> Piece
checkIfPiece _ [] = error "the square doesnt exist"
checkIfPiece coord (Square coord2 p:xs) = case coord2 == coord of
                                                True -> p
                                                _    -> checkIfPiece coord xs

--All pieces for a specific player 
getAllPiecesSq :: Color -> [Square] -> [Square]
getAllPiecesSq c pos = filter (isPieceColor c) pos

isPieceColor :: Color -> Square -> Bool
isPieceColor _ (Square _ Main.Nothing) = False
isPieceColor c (Square _ (Piece c2 _)) = c == c2
                                              






