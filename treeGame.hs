module TreeGame where
import Data.Fixed
import Data.List.Split
import Control.Monad (forM_, liftM)

player = ["X","Y"]
colour = ['R','G']

-- Player X can remove Red or Blue; Player Y can remove Green or Blue
-- Levels, Trees, and Nodes are indexed starting from ONE (1)

data Node = Node 
			{
				nodeId :: Int
				,adjList :: [(Int,Char)]
			}	deriving (Show)

type Move = [Int] -- (tree no., source node id, dest node id). Size must be THREE (3)
type State = [Tree]
type Tree = [Node]

display :: State -> IO ()
display state = print state

-- game is finished when each tree has only the root node left, with no edges
-- so each Tree of State should have EXACTLY one Node
isFinished :: State -> Bool
isFinished state = null nonEmptyTrees
	where
		nonEmptyTrees = filter (\tree -> (length tree) > 1) state

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
						[ 
							[
								Node {nodeId = 1, adjList = []}
							]
						],

						[ 	
							[
								Node {nodeId = 1, adjList = []}
							] , 
							
							[
								Node {nodeId=1, adjList=[(2,'G'),(3,'G'),(4,'G')]},
								Node {nodeId=2, adjList=[(5,'G')]},
								Node {nodeId=3, adjList=[]},
								Node {nodeId=4, adjList=[(6,'G')]},
								Node {nodeId=5, adjList=[]},
								Node {nodeId=6, adjList=[(7,'G'),(8,'G')]},
								Node {nodeId=7, adjList=[]},
								Node {nodeId=8, adjList=[]}
							] 
						],

						[ 	
							[
								Node {nodeId = 1, adjList = []}
							] , 
							
							[
								Node {nodeId=1, adjList=[(2,'R'),(3,'G'),(4,'B')]},
								Node {nodeId=2, adjList=[(5,'B')]},
								Node {nodeId=3, adjList=[]},
								Node {nodeId=4, adjList=[(6,'G')]},
								Node {nodeId=5, adjList=[]},
								Node {nodeId=6, adjList=[(7,'G'),(8,'R')]},
								Node {nodeId=7, adjList=[]},
								Node {nodeId=8, adjList=[]}
							] 
						],

						[
							[
								Node {nodeId = 1, adjList = []}
							],

							[
								Node {nodeId=1, adjList=[(2,'R'),(3,'G'),(4,'B')]},
								Node {nodeId=2, adjList=[(5,'B')]},
								Node {nodeId=3, adjList=[]},
								Node {nodeId=4, adjList=[(6,'G')]},
								Node {nodeId=5, adjList=[]},
								Node {nodeId=6, adjList=[(7,'G'),(8,'R')]},
								Node {nodeId=7, adjList=[]},
								Node {nodeId=8, adjList=[]}
							],

							[
								Node {nodeId=1, adjList=[(2,'R')]},
								Node {nodeId=2, adjList=[(3,'G')]},
								Node {nodeId=3, adjList=[]}
							]

						]
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
		tree = (state !! t)
		srcAdjList = adjList ((filter (\x -> (nodeId x)==s) tree) !! 0)
		edge = filter (\x -> (fst x)==d) srcAdjList


pruneSubTree :: Int->Tree->Tree
pruneSubTree s tree = 
	cleanList (map (\c -> if((nodeId c) == s) then Node{nodeId = -1,adjList = [(-1,'N')]} else c) tree')
	where
		children = getChildren tree s
		tree' = foldl (\acc c -> if c == -1 then acc else (pruneSubTree c acc)) tree children

getChildren :: Tree->Int->[Int]
getChildren tree s = 
	map(\t -> fst(t)) adjListOfs
	where
	adjListOfs 	= adjList (tempList !! 0)
	tempList = filter (\x -> (nodeId x) == s) tree

cleanList :: Tree->Tree
cleanList tree = 
	foldl (\acc x -> if ((nodeId x) == -1) then acc else [x]++acc) [] tree

modify :: State->Move->State
modify state move = 
	take t state ++ [(dfs tree move)] ++ drop (t + 1) state
	where
		t = move !! 0
		tree = (state !! t)

dfs :: Tree->Move->Tree
dfs tree move = 
	pruneSubTree d newTree
	where
		s = move !! 1
		d = move !! 2
		srcAdjList = adjList ((filter (\x -> (nodeId x)==s) tree) !! 0)
		newAdjList = foldl (\acc x -> if fst(x)==d then acc else [x]++acc) [] srcAdjList
		newTree = 
			foldl (\acc x ->	if (nodeId x)==s 
									then [Node {nodeId=s, adjList=newAdjList}] ++ acc
								else [x]++acc) [] tree

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


-- functions for the AI part

--returns the combined edgeList of ALL trees as a list of 4 tuple (tId, sourceId, DestId, EdgeColour)
--intially pass idx=0
getAllEdges :: State->Int->[(Int,Int,Int,Char)]
getAllEdges (tree:[]) idx = getEdgesOneTree tree idx
getAllEdges (tree:xs) idx = (getEdgesOneTree tree idx)++(getAllEdges xs (idx+1))

--returns the edgeList of tree#tId as a list of 4 tuple (tId, sourceId, DestId, EdgeColour)
getEdgesOneTree :: Tree->Int->[(Int,Int,Int,Char)]
getEdgesOneTree tree tId = 
	foldl (\acc n -> if((length (adjList n)) == 0) then acc else (edgeList tId (nodeId n) (adjList n)) ++acc) [] tree

--returns the edgeList of for a particular source of tree#tId as a list of 4 tuple (tId, sourceId, DestId, EdgeColour)
edgeList :: Int->Int->[(Int, Char)]->[(Int,Int,Int,Char)]
edgeList tId s childList = zipWith (\x y -> (tId, s, fst(y), snd(y))) [s..] childList

--given the combined edges of ALL trees, it returns an edge removing which is a winning strategy for player#id,
--if no such edge exists, an invalid edge
-- edge is represented as the 4-tuple (Bool, tId, sourceId, destId) where bool = false means invalid edge.
temp :: [(Int,Int,Int,Char)]
temp = [(1,1,2,'R'),(1,2,3,'G'),(1,3,4,'R'),(2,1,2,'R'),(3,1,2,'G')]

winOnOne :: [(Int,Int,Int,Char)]->[(Int,Int,Int,Char)]->Int->(Bool, Int, Int, Int)
winOnOne allEdges [] pId = (False, -1, -1, -1)
winOnOne allEdges (e:edges) pId = 
	if ((frth'(e)==(colour !! pId) || frth'(e)=='B') && (looseOnAll allEdges' allEdges' (mod' (pId+1) 2)) == True)
		then
			(True, fst'(e), snd'(e), thrd'(e))
		else
			winOnOne allEdges edges pId
	where
		fst' (f,_,_,_) = f
		snd' (_,s,_,_) = s
		thrd' (_,_,t,_) = t
		frth' (_,_,_,f) = f
		allEdges' = 
			foldl (\acc x ->if fst'(x)==fst'(e) && snd'(x)==snd'(e) && thrd'(x)==thrd'(e) && frth'(x)==frth'(e)
								then acc
							else [x]++acc) [] allEdges

--given the combined edges of ALL trees, it returns True if there does not exist any winning strategy for player#pId in the form of removal of an edge 
--if atleast one such edge exists, it returns False
looseOnAll :: [(Int,Int,Int,Char)]->[(Int,Int,Int,Char)]->Int->Bool
looseOnAll allEdges [] pId = True
looseOnAll allEdges (e:edges) pId = 
	if ((frth'(e)==(colour !! pId) || frth'(e)=='B'))
		then
			if(fst'(winOnOne allEdges' allEdges' (mod' (pId+1) 2)) == False)
				then
					False
				else
					looseOnAll allEdges edges pId
		else
			looseOnAll allEdges edges pId
	where
		fst' (f,_,_,_) = f
		snd' (_,s,_,_) = s
		thrd' (_,t,_,_) = t
		frth' (_,_,_,f) = f
		allEdges' = 
			foldl (\acc x ->if fst'(x)==fst'(e) && snd'(x)==snd'(e) && thrd'(x)==thrd'(e) && frth'(x)==frth'(e)
								then acc
							else [x]++acc) [] allEdges

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
		= [
			Node {nodeId=1, adjList=[(2,'R'),(3,'G'),(4,'B')]},
			Node {nodeId=2, adjList=[(5,'B')]},
			Node {nodeId=3, adjList=[]},
			Node {nodeId=4, adjList=[(6,'G')]},
			Node {nodeId=5, adjList=[]},
			Node {nodeId=6, adjList=[(7,'G'),(8,'R')]},
			Node {nodeId=7, adjList=[]},
			Node {nodeId=8, adjList=[]}
		] 

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