import Data.List

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher inlist c d n =  (goMinmax (head inlist) c d n inlist):inlist

goMinmax :: String -> Char -> Int -> Int -> [String]-> String
goMinmax inlist c d n past  = getNext (reverse pawnList) past
 where { pawnList = sort (zip (minimaxTrigger inlist c d n) (search_pawns inlist c n))} 

getNext :: [(Int, String)] ->[String] -> String
getNext pawnList past
 | null pawnList = []
 | not (elem headStr past) = headStr
 | otherwise = getNext (tail pawnList) past
 where headStr = (snd (head pawnList))

minimax :: String-> Char -> Int -> Int -> Int -> Int
minimax inlist c d n curr
 | (curr == d ) = currentBoard
 | (currentBoard == 10 || currentBoard == -10 ) = currentBoard
 | (mod curr 2) == 0 = maximum ( minimaxAll (search_pawns inlist c n) c d n (curr + 1) )
 | otherwise = minimum ( minimaxAll (search_pawns inlist op n) c d n (curr + 1) )
 where {currentBoard = (board_evaluator_basic inlist c n);
           op = getOpp c;}

minimaxTrigger :: String -> Char -> Int -> Int ->[Int]
minimaxTrigger inlist c d n
 | d == 0 = []
 | otherwise = (minimaxAll (search_pawns inlist c n) c d n 1)

minimaxAll :: [String] -> Char -> Int -> Int -> Int -> [Int]
minimaxAll inlist c d n curr
 | null inlist = []
 | otherwise = (minimax (head inlist) c d n curr ):(minimaxAll (tail inlist) c d n curr)


dumbLeft :: [String] -> Int-> [String]
dumbLeft inlist n
 | null inlist = []
 | otherwise =  (concat (transpose_right(parser (head inlist) n 0 ) n 1 )) : (dumbLeft (tail inlist) n)


dumbRight :: [String] -> Int -> [String]
dumbRight inlist n 
 | null inlist = []
 | otherwise = (concat (transpose_left( parser (head inlist) n 0 ) n 1 )) : (dumbRight (tail inlist) n )


generateMove :: [String]->String->Char->[String]
generateMove inlist past c
 | null inlist = []
 | not (elem '*' (head inlist)) = (generateMove (tail inlist) (past ++ (head inlist))  c)
 | otherwise = generateTemplate past (tail inlist) (head inlist) strSet 
 where { op = getOpp c;
              strSet = [(  "-*", (c:"-") ), ( "*-", ('-':(c:"")) ), ('*':c:('-':""), '-':c:(c:"") ),
                        ( '-':c:('*':""), c:c:('-':"") ), ('*':c:(op:""),'-':c:(c:"") ), ( op:c:('*':""), c:c:('-':"") )];}

generateTemplate :: String-> [String] -> String -> [(String, String)] -> [String]
generateTemplate past next curr strSet
 | null strSet = []
 | null generateCurr = (generateTemplate past next curr (tail strSet) )
 | otherwise =  (past ++ generateCurr ++ nextCons):(generateTemplate past next curr (tail strSet) ) 
 where {currStr = (head strSet);
	generateCurr = concat (generateNew curr 0 (fst currStr) (snd currStr) );
        nextCons = (concat next); }

search_pawns :: String -> Char -> Int -> [String]
search_pawns inlist c n 
 | null inlist = []
 |otherwise = search_pawns_helper "" (head inlist) (tail inlist) c n []

search_pawns_helper :: String->Char->String->Char-> Int -> [String]->[String]
search_pawns_helper past current next c n set
 | ((next == []) && (current == c) )  = (filter (not . null) (generate_new (past ++ ('*':[])) c n)) ++ set
 | ((next == []) && (set /= [] ) )     = set
 | current /= c	= search_pawns_helper (past ++ (current:"")) (head next) (tail next) c n set
 | otherwise 	= search_pawns_helper (past ++ (current:"")) (head next) (tail next) c n
			((filter (not . null) (generate_new (past ++ ('*': next)) c n)) ++ set)


generate_new :: String->Char -> Int ->[String]
generate_new inlist c n  = (generateMove (parser inlist n 0) [] c)++
	                 (dumbLeft (filter (not . null) (generateMove (transpose_left (parser inlist n 0) n 1) [] c)) n)++
		         (dumbRight (filter (not . null) (generateMove (transpose_right (parser inlist n 0) n 1) [] c)) n)


--generateNew 
--referenced from pegpuzzle program
generateNew currState pos oldSegment newSegment
   | pos + (length oldSegment) > length currState   = []
   | segmentEqual currState pos oldSegment           =
        (replaceSegment currState pos newSegment):
        (generateNew currState (pos + 1) oldSegment newSegment)
   | otherwise                                       =
        (generateNew currState (pos + 1) oldSegment newSegment)

--segmentEqual
--referenced from pegpuzzle program
segmentEqual currState pos oldSegment = 
   (oldSegment == take (length oldSegment) (drop pos currState))

--replaceSegment
--referenced from pegpuzzle program
replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise = 
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)

--getOpp
--returns the opponent's pawn color
getOpp :: Char -> Char
getOpp c
 | c == 'W' = 'B'
 | otherwise = 'W'	


parser :: String -> Int -> Int -> [String]
parser inlist n i
 | i < n - 1 = (parser_take inlist (n + i)) : (parser (drop (n + i) inlist) n (i+1) )
 | i > 2*n - 2 = []
 | otherwise = (parser_take inlist (3*n - 2 - i))
		:(parser (drop (3*n -2 - i) inlist) n (i+1))  


parser_take :: String -> Int -> String
parser_take (x:xs) n
 | n == 0 = []
 | null xs = x:[]
 | otherwise = x : (parser_take xs (n - 1))


drop_last :: [a] -> [a]
drop_last [] = []
drop_last (x:xs)
 | null xs = []
 | otherwise = x : (drop_last xs)

transpose_left :: [String] -> Int -> Int -> [String]
transpose_left inlist n i
 | i < n = (transpose_ind inlist 0 (n + i - 2) ) 
	:(transpose_left (transpose_drop inlist 0 (n + i - 2)) n (i+1))
 | i <= 2*n - 1 = (transpose_ind inlist (i - n) (2*n - 2)) 
	:(transpose_left (transpose_drop inlist (i - n) (2*n - 2)) n (i+1))
 | otherwise = []


transpose_ind :: [String] -> Int -> Int -> String
transpose_ind inlist start end 
 | null inlist = []
 | start /= 0 = (transpose_ind (tail inlist) (start - 1) end)
 | end >= 0   = (last (head inlist)) : (transpose_ind (tail inlist) start (end - 1))
 | otherwise = []

transpose_drop :: [String] -> Int -> Int -> [String]
transpose_drop inlist start end
 | null inlist = inlist
 | start /= 0 = (head inlist):(transpose_drop (tail inlist) (start - 1) end) 
 | end >= 0   = (drop_last (head inlist))
		:(transpose_drop (tail inlist) start (end - 1))
 | otherwise = inlist


transpose_right :: [String] -> Int -> Int -> [String]
transpose_right inlist n i
 | i < n = (reverse (transpose_ind_r inlist 0 (n + i - 2) ))
	:(transpose_right (transpose_drop_r inlist 0 (n + i - 2)) n (i+1))
 | i <= 2*n - 1 = (reverse (transpose_ind_r inlist (i - n) (2*n - 2)))
	:(transpose_right (transpose_drop_r inlist (i - n) (2*n - 2)) n (i+1))
 | otherwise = []

transpose_ind_r :: [String] -> Int -> Int -> String
transpose_ind_r inlist start end 
 | null inlist = []
 | start /= 0 = (transpose_ind_r (tail inlist) (start - 1) end)
 | end >= 0   = (head (head inlist)) : (transpose_ind_r (tail inlist) start (end - 1))
 | otherwise = []

transpose_drop_r :: [String] -> Int -> Int -> [String]
transpose_drop_r inlist start end
 | null inlist = inlist
 | start /= 0 = (head inlist):(transpose_drop_r (tail inlist) (start - 1) end) 
 | end >= 0   = (tail (head inlist))
		:(transpose_drop_r (tail inlist) start (end - 1))
 | otherwise = inlist

--input: takes a string representing a board (instring) and a char representing the pawn colour to count (pawn_to_count)
--output: returns the count of the given pawn colour in the board
pawn_counter :: String -> Char -> Int
pawn_counter instring pawn_to_count = pawn_counter_helper instring pawn_to_count 0

pawn_counter_helper :: String -> Char -> Int -> Int
pawn_counter_helper instring pawn_to_count count
 	|null instring = count
 	|(head instring) == pawn_to_count = pawn_counter_helper (tail instring) pawn_to_count (count + 1)
 	|otherwise = pawn_counter_helper (tail instring) pawn_to_count count

--input: takes a string representing a board (instring), a char representing the pawn colour of the current player (whos_turn) and
--an int representing the size of one edge of the board (n). n is used to calculate the number of pieces in the original board
--output: -10 if the current player has less than half of its original number of pawns (lose), +10 if the opponent has less than half (win), otherwise
--return the number of the current player's pieces minus the opponent's
board_evaluator_basic :: String -> Char -> Int -> Int
board_evaluator_basic instring whos_turn n
  |(not (finalStaticMoveEv instring whos_turn n)) = -10
  |(not (finalStaticMoveEv instring (getOpp whos_turn) n)) = 10
	|(fromIntegral (pawn_counter instring whos_turn)) < (fromIntegral ((2*n) - 1) / 2) = -10
	|(fromIntegral (pawn_counter instring (getOpp whos_turn))) < (fromIntegral ((2*n) - 1) / 2) = 10
	|otherwise = (pawn_counter instring whos_turn) - (pawn_counter instring (getOpp whos_turn))


staticMoveEv :: String -> Char -> Int -> Bool
staticMoveEv inlist c n
 | null inlist = False 
 | (not curr) = staticMoveEv (tail inlist) c n
 | otherwise = True
 where  curr = (segEqualAll inlist c n)

segEqualAll :: String -> Char -> Int -> Bool
segEqualAll inlist c n = 
 ((segmentEqual inlist 0 (c:"-"))||
  (segmentEqual inlist 0 ('-':c:""))||
  (segmentEqual inlist 0 (c:c:"-"))||
  (segmentEqual inlist 0 ('-':c:c:""))||
  (segmentEqual inlist 0 ( (getOpp c):c:c:""))||
  (segmentEqual inlist 0 ( c:c:(getOpp c):"")))

check_left :: String -> Char -> Int -> Bool
check_left inlist c n = check_helper inlist c n (transpose_left(parser inlist n 0) n 1)

check_helper :: String -> Char -> Int -> [String] -> Bool
check_helper inlist c n transpose_list
  |(null transpose_list) = False
  |staticMoveEv (head transpose_list) c n = True
  |otherwise = check_helper inlist c n (tail transpose_list)

check_right :: String -> Char -> Int -> Bool
check_right inlist c n = check_helper inlist c n (transpose_right(parser inlist n 0) n 1)


check_horizontal :: String -> Char -> Int -> Bool
check_horizontal inlist c n = check_helper inlist c n (parser inlist n 0)

finalStaticMoveEv :: String -> Char -> Int -> Bool
finalStaticMoveEv inlist c n = (check_horizontal inlist c n) || (check_left inlist c n) || (check_right inlist c n)


crusherTillWin :: String -> Char -> Int -> Int -> [String] -> [String]
crusherTillWin inlist c d n past
 | null curr = past  
 | (currBoard == 10 || currBoard == -10 ) = curr:past
 | otherwise = crusherTillWin curr (getOpp c) d n (curr:past)
 where{ curr = (goMinmax inlist c d n past);
             currBoard = (board_evaluator_basic curr c n); }
          
