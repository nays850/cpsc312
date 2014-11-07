import Data.List

crusherTillWin :: [String] -> Char -> Int -> Int -> [String]
crusherTillWin inlist c d n
 | null curr = inlist
 | (currBoard == 100 || currBoard == -100 ) = curr:inlist
 | otherwise = crusherTillWinOpp (curr:inlist) (getOpp_m9a0b c) (d) n
 where{ curr = (head (crusher_m9a0b inlist c d n)); 
             currBoard = (borad_evaluator_m9a0b curr c n); }
          

crusherTillWinOpp :: [String] -> Char -> Int -> Int -> [String]
crusherTillWinOpp inlist c d n
 | null curr = inlist
 | (currBoard == 100 || currBoard == -100 ) = curr:inlist
 | otherwise = crusherTillWin (curr:inlist) (getOpp_m9a0b c) (d) n
 where{ curr = (head (crusher_m9a0b inlist c d n)); 
             currBoard = (borad_evaluator_m9a0b curr c n); }
          

crusher_m9a0b :: [String] -> Char -> Int -> Int -> [String]
crusher_m9a0b inlist c d n 
 | ( curr == 100 || curr == -100 ) = inlist
 | c == 'B'= (goMinmax_m9a0b (head inlist) c d n inlist):inlist
 | otherwise = (reverse  (goMinmax_m9a0b (reverse (head inlist)) c d n inlist)):inlist
 where curr = (borad_evaluator_m9a0b (head inlist) c n)

goMinmax_m9a0b :: String -> Char -> Int -> Int -> [String]-> String
goMinmax_m9a0b inlist c d n past  = getNext_m9a0b (reverse pawnList) past
 where { pawnList = sort (zip (minimaxTrigger_m9a0b inlist c d n) (search_pawns_m9a0b inlist c n))} 

getNext_m9a0b :: [(Int, String)] ->[String] -> String
getNext_m9a0b pawnList past
 | null pawnList = []
 | not (elem headStr past) = headStr
 | otherwise = getNext_m9a0b (tail pawnList) past
 where headStr = (snd (head pawnList))

minimax_m9a0b :: String-> Char -> Int -> Int -> Int -> Int
minimax_m9a0b inlist c d n curr
 | (curr == d ) = currentBoard
 | (currentBoard == 100 || currentBoard == -100 ) = currentBoard
 | (mod curr 2) == 0 = maximum ( minimaxAll_m9a0b (search_pawns_m9a0b inlist c n) c d n (curr + 1) )
 | otherwise = minimum ( minimaxAll_m9a0b (search_pawns_m9a0b inlist op n) c d n (curr + 1) )
 where {currentBoard = (borad_evaluator_m9a0b inlist c n);
           op = getOpp_m9a0b c;}

minimaxTrigger_m9a0b :: String -> Char -> Int -> Int ->[Int]
minimaxTrigger_m9a0b inlist c d n
 | d == 0 = []
 | otherwise = (minimaxAll_m9a0b (search_pawns_m9a0b inlist c n) c d n 1)

minimaxAll_m9a0b :: [String] -> Char -> Int -> Int -> Int -> [Int]
minimaxAll_m9a0b inlist c d n curr
 | null inlist = []
 | otherwise = (minimax_m9a0b (head inlist) c d n curr ):(minimaxAll_m9a0b (tail inlist) c d n curr)


dumLeft_m9a0b :: [String] -> Int-> [String]
dumLeft_m9a0b inlist n
 | null inlist = []
 | otherwise =  (concat (transpose_right_m9a0b(parser_m9a0b (head inlist) n 0 ) n 1 )) : (dumLeft_m9a0b (tail inlist) n)


dumbRight_m9a0b :: [String] -> Int -> [String]
dumbRight_m9a0b inlist n 
 | null inlist = []
 | otherwise = (concat (transpose_left_m9a0b( parser_m9a0b (head inlist) n 0 ) n 1 )) : (dumbRight_m9a0b (tail inlist) n )


generateMove_m9a0b :: [String]->String->Char->[String]
generateMove_m9a0b inlist past c
 | null inlist = []
 | not (elem '*' (head inlist)) = (generateMove_m9a0b (tail inlist) (past ++ (head inlist))  c)
 | otherwise = generateTemplate_m9a0b past (tail inlist) (head inlist) strSet 
 where { op = getOpp_m9a0b c;
              strSet = [(  "-*", (c:"-") ), ( "*-", ('-':(c:"")) ), ('*':c:('-':""), '-':c:(c:"") ),
                        ( '-':c:('*':""), c:c:('-':"") ), ('*':c:(op:""),'-':c:(c:"") ), ( op:c:('*':""), c:c:('-':"") )];}

generateTemplate_m9a0b :: String-> [String] -> String -> [(String, String)] -> [String]
generateTemplate_m9a0b past next curr strSet
 | null strSet = []
 | null generateCurr = (generateTemplate_m9a0b past next curr (tail strSet) )
 | otherwise =  (past ++ generateCurr ++ nextCons):(generateTemplate_m9a0b past next curr (tail strSet) ) 
 where {currStr = (head strSet);
	generateCurr = concat (generateNew_m9a0b curr 0 (fst currStr) (snd currStr) );
        nextCons = (concat next); }

search_pawns_m9a0b :: String -> Char -> Int -> [String]
search_pawns_m9a0b inlist c n 
 | null inlist = []
 |otherwise = search_pawns_helper_m9a0b "" (head inlist) (tail inlist) c n []

search_pawns_helper_m9a0b :: String->Char->String->Char-> Int -> [String]->[String]
search_pawns_helper_m9a0b past current next c n set
 | ((next == []) && (current == c) )  = (filter (not . null) (generate_new_m9a0b (past ++ ('*':[])) c n)) ++ set
 | ((next == []) && (set /= [] ) )     = set
 | current /= c	= search_pawns_helper_m9a0b (past ++ (current:"")) (head next) (tail next) c n set
 | otherwise 	= search_pawns_helper_m9a0b (past ++ (current:"")) (head next) (tail next) c n
			((filter (not . null) (generate_new_m9a0b (past ++ ('*': next)) c n)) ++ set)


generate_new_m9a0b :: String->Char -> Int ->[String]
generate_new_m9a0b inlist c n  = (generateMove_m9a0b (parser_m9a0b inlist n 0) [] c)++
	                 (dumLeft_m9a0b (filter (not . null) (generateMove_m9a0b (transpose_left_m9a0b (parser_m9a0b inlist n 0) n 1) [] c)) n)++
		         (dumbRight_m9a0b (filter (not . null) (generateMove_m9a0b (transpose_right_m9a0b (parser_m9a0b inlist n 0) n 1) [] c)) n)


--generateNew_m9a0b 
--referenced from pegpuzzle program
generateNew_m9a0b currState pos oldSegment newSegment
   | pos + (length oldSegment) > length currState   = []
   | segmentEqual_m9a0b currState pos oldSegment           =
        (replaceSegment_m9a0b currState pos newSegment):
        (generateNew_m9a0b currState (pos + 1) oldSegment newSegment)
   | otherwise                                       =
        (generateNew_m9a0b currState (pos + 1) oldSegment newSegment)

--segmentEqual_m9a0b
--referenced from pegpuzzle program
segmentEqual_m9a0b currState pos oldSegment = 
   (oldSegment == take (length oldSegment) (drop pos currState))

--replaceSegment_m9a0b
--referenced from pegpuzzle program
replaceSegment_m9a0b oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise = 
        (head oldList):
        (replaceSegment_m9a0b (tail oldList) (pos - 1) segment)

--getOpp_m9a0b
--returns the opponent's pawn color
getOpp_m9a0b :: Char -> Char
getOpp_m9a0b c
 | c == 'W' = 'B'
 | otherwise = 'W'	


parser_m9a0b :: String -> Int -> Int -> [String]
parser_m9a0b inlist n i
 | i < n - 1 = (parser_take_m9a0b inlist (n + i)) : (parser_m9a0b (drop (n + i) inlist) n (i+1) )
 | i > 2*n - 2 = []
 | otherwise = (parser_take_m9a0b inlist (3*n - 2 - i))
		:(parser_m9a0b (drop (3*n -2 - i) inlist) n (i+1))  


parser_take_m9a0b :: String -> Int -> String
parser_take_m9a0b (x:xs) n
 | n == 0 = []
 | null xs = x:[]
 | otherwise = x : (parser_take_m9a0b xs (n - 1))


drop_last_m9a0b :: [a] -> [a]
drop_last_m9a0b [] = []
drop_last_m9a0b (x:xs)
 | null xs = []
 | otherwise = x : (drop_last_m9a0b xs)

transpose_left_m9a0b :: [String] -> Int -> Int -> [String]
transpose_left_m9a0b inlist n i
 | i < n = (transpose_ind_m9a0b inlist 0 (n + i - 2) ) 
	:(transpose_left_m9a0b (transpose_drop_m9a0b inlist 0 (n + i - 2)) n (i+1))
 | i <= 2*n - 1 = (transpose_ind_m9a0b inlist (i - n) (2*n - 2)) 
	:(transpose_left_m9a0b (transpose_drop_m9a0b inlist (i - n) (2*n - 2)) n (i+1))
 | otherwise = []


transpose_ind_m9a0b :: [String] -> Int -> Int -> String
transpose_ind_m9a0b inlist start end 
 | null inlist = []
 | start /= 0 = (transpose_ind_m9a0b (tail inlist) (start - 1) end)
 | end >= 0   = (last (head inlist)) : (transpose_ind_m9a0b (tail inlist) start (end - 1))
 | otherwise = []

transpose_drop_m9a0b :: [String] -> Int -> Int -> [String]
transpose_drop_m9a0b inlist start end
 | null inlist = inlist
 | start /= 0 = (head inlist):(transpose_drop_m9a0b (tail inlist) (start - 1) end) 
 | end >= 0   = (drop_last_m9a0b (head inlist))
		:(transpose_drop_m9a0b (tail inlist) start (end - 1))
 | otherwise = inlist


transpose_right_m9a0b :: [String] -> Int -> Int -> [String]
transpose_right_m9a0b inlist n i
 | i < n = (reverse (transpose_ind_r_m9a0b inlist 0 (n + i - 2) ))
	:(transpose_right_m9a0b (transpose_drop_r_m9a0b inlist 0 (n + i - 2)) n (i+1))
 | i <= 2*n - 1 = (reverse (transpose_ind_r_m9a0b inlist (i - n) (2*n - 2)))
	:(transpose_right_m9a0b (transpose_drop_r_m9a0b inlist (i - n) (2*n - 2)) n (i+1))
 | otherwise = []

transpose_ind_r_m9a0b :: [String] -> Int -> Int -> String
transpose_ind_r_m9a0b inlist start end 
 | null inlist = []
 | start /= 0 = (transpose_ind_r_m9a0b (tail inlist) (start - 1) end)
 | end >= 0   = (head (head inlist)) : (transpose_ind_r_m9a0b (tail inlist) start (end - 1))
 | otherwise = []

transpose_drop_r_m9a0b :: [String] -> Int -> Int -> [String]
transpose_drop_r_m9a0b inlist start end
 | null inlist = inlist
 | start /= 0 = (head inlist):(transpose_drop_r_m9a0b (tail inlist) (start - 1) end) 
 | end >= 0   = (tail (head inlist))
		:(transpose_drop_r_m9a0b (tail inlist) start (end - 1))
 | otherwise = inlist

--input: takes a string representing a board (instring) and a char representing the pawn colour to count (pawn_to_count)
--output: returns the count of the given pawn colour in the board
pawn_counter_m9a0b :: String -> Char -> Int
pawn_counter_m9a0b instring pawn_to_count = pawn_counter_helper_m9a0b instring pawn_to_count 0

pawn_counter_helper_m9a0b :: String -> Char -> Int -> Int
pawn_counter_helper_m9a0b instring pawn_to_count count
 	|null instring = count
 	|(head instring) == pawn_to_count = pawn_counter_helper_m9a0b (tail instring) pawn_to_count (count + 1)
 	|otherwise = pawn_counter_helper_m9a0b (tail instring) pawn_to_count count

--input: takes a string representing a board (instring), a char representing the pawn colour of the current player (whos_turn) and
--an int representing the size of one edge of the board (n). n is used to calculate the number of pieces in the original board
--output: -10 if the current player has less than half of its original number of pawns (lose), +10 if the opponent has less than half (win), otherwise
--return the number of the current player's pieces minus the opponent's
borad_evaluator_m9a0b :: String -> Char -> Int -> Int
borad_evaluator_m9a0b instring whos_turn n
  |(not (finalStaticMoveEv_m9a0b instring whos_turn n)) = -100
  |(not (finalStaticMoveEv_m9a0b instring op n)) = 100
	|(fromIntegral myPawn) < (fromIntegral ((2*n) - 1) / 2) = -100
	|(fromIntegral notMyPawn) < (fromIntegral ((2*n) - 1) / 2) = 100
	|otherwise = 3*(myPawn - notMyPawn)
 		      - 3*(hrNext_m9a0b instring whos_turn n) 
		     + 3*(hrNext_m9a0b instring op n)
		     - 0*(avoidCenter_m9a0b instring whos_turn n)
 where { op = getOpp_m9a0b whos_turn;
	myPawn = (pawn_counter_m9a0b instring whos_turn);
	notMyPawn = (pawn_counter_m9a0b instring op); }

avoidCenter_m9a0b :: String -> Char -> Int -> Int
avoidCenter_m9a0b inlist c n
 | c == 'W' = pawn_counter_m9a0b (head (drop (n-1) curr)) c
 | otherwise = pawn_counter_m9a0b (head (drop (n-1) (reverse curr))) c
 where curr = (parser_m9a0b inlist n 0)

staticMoveEv_m9a0b :: String -> Char -> Int -> Bool
staticMoveEv_m9a0b inlist c n
 | null inlist = False 
 | (not curr) = staticMoveEv_m9a0b (tail inlist) c n
 | otherwise = True
 where  curr = (segEqualAll_m9a0b inlist c n)

segEqualAll_m9a0b :: String -> Char -> Int -> Bool
segEqualAll_m9a0b inlist c n = 
 ((segmentEqual_m9a0b inlist 0 (c:"-"))||
  (segmentEqual_m9a0b inlist 0 ('-':c:""))||
  (segmentEqual_m9a0b inlist 0 (c:c:"-"))||
  (segmentEqual_m9a0b inlist 0 ('-':c:c:""))||
  (segmentEqual_m9a0b inlist 0 ( (getOpp_m9a0b c):c:c:""))||
  (segmentEqual_m9a0b inlist 0 ( c:c:(getOpp_m9a0b c):"")))

check_helper_m9a0b :: Char -> Int -> [String] -> Bool
check_helper_m9a0b c n transpose_list
  |(null transpose_list) = False
  |staticMoveEv_m9a0b (head transpose_list) c n = True
  |otherwise = check_helper_m9a0b c n (tail transpose_list)

checkMove_m9a0b :: [String] -> Char -> Int -> Bool
checkMove_m9a0b inlist c n = check_helper_m9a0b c n inlist 

finalStaticMoveEv_m9a0b :: String -> Char -> Int -> Bool
finalStaticMoveEv_m9a0b inlist c n  = (checkMove_m9a0b parsedList c n) ||
    	       (checkMove_m9a0b (transpose_left_m9a0b parsedList n 1) c n) ||
    	       (checkMove_m9a0b (transpose_right_m9a0b parsedList n 1) c n) 
 where parsedList = (parser_m9a0b inlist n 0)	                 

hrEv_m9a0b :: String -> Char -> Int -> Int -> Int
hrEv_m9a0b inlist c n i
 | null inlist =  i
 | (not curr) = hrEv_m9a0b (tail inlist) c n i
 | otherwise = hrEv_m9a0b (tail inlist) c n (i+1)
 where  curr =  ((segmentEqual_m9a0b inlist 0 ( (getOpp_m9a0b c):(getOpp_m9a0b c):c:""))||
 	         (segmentEqual_m9a0b inlist 0 ( c:(getOpp_m9a0b c):(getOpp_m9a0b c):"")))

checkHr_m9a0b :: [String] -> Char -> Int ->Int -> Int
checkHr_m9a0b inlist c n i
 | null inlist = i
 | otherwise = (checkHr_m9a0b (tail inlist) c n (hrEv_m9a0b (head inlist) c n i))

hrNext_m9a0b :: String -> Char -> Int -> Int
hrNext_m9a0b inlist c n = (checkHr_m9a0b (parser_m9a0b inlist n 0) c n 0)
                           + (checkHr_m9a0b (transpose_right_m9a0b (parser_m9a0b inlist n 0) n 1) c n 0)
                           + (checkHr_m9a0b (transpose_left_m9a0b (parser_m9a0b inlist n 0) n 1) c n 0)
