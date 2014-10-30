generateMove :: [String]->String->Char->[String]
generateMove inlist past c
 | null inlist = []
 | not (elem '*' (head inlist)) = (generateMove (tail inlist) (past ++ (head inlist))  c)
 | otherwise = (past ++ (concat (generateNew (head inlist) 0 "-*" (c:"-"))) ++ (concat (tail inlist))       ):
 	       (past ++ (concat (generateNew (head inlist) 0 "*-" ('-':(c:""))) ) ++ (concat (tail inlist))  ):
	       (past ++ (concat (generateNew (head inlist) 0 ('*':(c:"")++('-':""))  ('-':(c:"")++(c:""))) ++ (concat (tail inlist)))):
	       (past ++ (concat (generateNew (head inlist) 0 ('-':(c:"")++('*':""))  (c:(c:"")++('-':""))) ++ (concat (tail inlist)))):
	       (past ++ (concat (generateNew (head inlist) 0 ('*':(c:"")++(op:""))  ('-':(c:"")++(c:""))) ++ (concat (tail inlist)))):
	       (past ++ (concat (generateNew (head inlist) 0 (op:(c:"")++('*':""))  (c:(c:"")++('-':""))) ++ (concat (tail inlist)))):[]
  where op = getOpp c


search_pawns :: String -> Char -> Int -> [String]
search_pawns inlist c n = search_pawns_helper "" (head inlist) (tail inlist) c n []

search_pawns_helper :: String->Char->String->Char-> Int -> [String]->[String]
search_pawns_helper past current next c n set
 | null next = set
 | current /= c	= search_pawns_helper (past ++ (current:"")) (head next) (tail next) c n set
 | otherwise 	= search_pawns_helper (past ++ (current:"")) (head next) (tail next) c n
			( (rmLoss (generate_new (past ++ ('*': next)) c n) lengthStr) ++ set)
 where lengthStr = length (past++('*':next))

rmLoss :: [String] -> Int -> [String]
rmLoss inlist n
 | null inlist = []
 | length (head inlist) == n = (head inlist):(rmLoss (tail inlist) n)
 | otherwise = rmLoss (tail inlist) n

generate_new :: String->Char -> Int ->[String]
generate_new inlist c n  = generateMove (parser inlist n 0) [] c


--generateNew 
--referenced from pegpuzzle program
generateNew currState pos oldSegment newSegment
   | pos + (length oldSegment) > length currState    = []
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
 | c == 'B' = 'W'
 | otherwise = 'B'	


parser :: String -> Int -> Int -> [String]
parser inlist n i
 | i < n - 1 = (parser_take inlist (n + i)) : (parser (drop (n + i) inlist) n (i+1) )
 | i > 2*n - 2 = []
 | otherwise = (parser_take inlist (3*n - 2 - i))
		:(parser (drop (3*n -2 - i) inlist) n (i+1))  


parser_take :: String -> Int -> String
parser_take inlist n
 | n == 0 = []
 | otherwise = (head inlist) : (parser_take (tail inlist) (n - 1))



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
 | i < n = (transpose_ind_r inlist 0 (n + i - 2) ) 
	:(transpose_right (transpose_drop_r inlist 0 (n + i - 2)) n (i+1))
 | i <= 2*n - 1 = (transpose_ind_r inlist (i - n) (2*n - 2)) 
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
