module TreeGame where
import Data.Fixed
import Data.List.Split
import Control.Monad (forM_, liftM)

player = ["X","Y"]
colour = ['R','G']

-- Player X can remove Red or Blue; Player Y can remove Green or Blue
-- Levels, Trees, and Nodes are indexed starting from ONE (1)

type Move = [Int] -- (tree no., source node id, dest node id). Size must be THREE (3)
type State = [Tree]
type Tree = [AdjList]
type AdjList = [Node]
type Node = (Int,Char)

display :: State -> IO ()
display state = undefined

-- boolVals is a list of bools representing true/false = finished/unfinished for each tree
-- boolVals is constructed by extracting each tree from state and folding over the tree 
-- to check if each adj list is of size EXACTLY 1
isFinished :: State -> Bool
isFinished state = foldl (\acc x -> acc && x) True boolVals
	where boolVals = [ foldl (\acc x -> if ((length x) == 1) then (acc && True) else False) True (state !! i) | i <- [0..length state - 1] ]

getLevelNo :: IO Int
getLevelNo = do
	input <- getLine
	return (read input :: Int)

getInput :: IO Move
getInput = do
	input <- getLine
	let tempMove = splitOn "," input
	let move = map (\x -> (read x :: Int)) tempMove
	return move

-- Load all levels from file. Implement later
loadAllLevels :: String -> IO [State]
loadAllLevels filename = undefined
	--lns <- liftM lines . readFile $ filename
	--return lns
	--return $ unfoldr consume lns
	--where isEmptyLine = all (' '==)
	--      consume [] = Nothing
	--      consume ls = let (a,b) = break isEmptyLine ls
	--                   in return (loadLevel $ unlines a, drop 1 b)

loadState :: Int->State
loadState levelNo = levels !! levelNo
	where levels =	[
						[ [[(0,'N')]] ],

						[ [[(0,'N')]] , [[(0,'N')],[(1,'N'),(2,'R'),(3,'G'),(4,'B')],[(2,'N'),(5,'b')],[(3,'N')],[(4,'N'),(6,'G')],[(5,'N')],[(6,'N'),(7,'G'),(8,'R')]] ],

						[ [[(0,'N')]] , [[(0,'N')],[(1,'N'),(2,'R'),(3,'G'),(4,'B')],[(2,'N'),(5,'B')],[(3,'N')],[(4,'N'),(6,'G')],[(5,'N')],[(6,'N'),(7,'G'),(8,'R')]] , [[(0,'N')],[(1,'N'),(2,'R')],[(2,'N'),(3,'G')],[(3,'N')]] ]
					]


isValid :: Move->State->Int->Bool
isValid move state id =
	if length move /= 3
		then False
	else
		if length edge == 0
			then False
		else
			(snd (edge !! 0) == (colour !! id)) || (snd (edge !! 0) == 'B')

	where
		t = move !! 0
		s = move !! 1
		d = move !! 2
		srcAdjList = (state !! t) !! s
		edge = [tuple | tuple <- srcAdjList, fst(tuple)==d]



modify :: State->Move->State
modify state move = undefined



main = do
	--levels <- loadAllLevels "levels.txt"
	levelNo <- getLevelNo
	--let state = levels !! levelNo
	let state = loadState levelNo
	gameLoop state 0	-- call gameLoop with initial state and first player
	return ()

gameLoop:: State->Int->IO ()
gameLoop state id = do
	display state
	if isFinished state
		then putStrLn ("Player "++(player !! id)++" lost :(")
	else do
		move <- getInput
		if isValid move state id 	--check if given move is valid for curr player on curr state
			then gameLoop (modify state move) (mod' (id+1) 2)
						-- gameLoop with modified state and next player
		else
			do
				putStrLn ("Invalid Move! Try again...")
				gameLoop state id 	--if invalid move, then gameLoop with same state and same player




{- Basic Code Flow

	load state -- initial state
	display state

	while not isFinished
	{
		get input from Player1
		gameStep input
		{
			if isValid input -- print error message inside isValid fn.
				modify state
		}
		display state
	
		if isFinished
			break

		get input from Player2
		gameStep input
		{
			if isValid input
				modify state
		}
		display state
	}
-}


{- sampleTree 
		= [[[(0,'N')],[(1,'N'),(2,'R'),(3,'G'),(4,'B')],[(2,'N'),(5,'b')],[(3,'N')],[(4,'N'),(6,'G')],[(5,'N')],[(6,'N'),(7,'G'),(8,'R')]]]

			1
		  / | \
		 r  g  b
		/	|   \
	   2	3	4
	  /			\
	 b 		  	g
	/			\
   5 			6
	   		   / \	   
	   		  g  r
	   		 /    \
	   		7	   8
-}