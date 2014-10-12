module TreeGame where
import Data.Fixed

player = ["A","B"]

type Move = (Int, Int, Int) -- (tree no., Level no., branch no.) 
-- need to re-design State for efficient dfs
type State = [Int]

display :: State -> IO ()
display state = undefined

isFinished :: State -> Bool
isFinished state = undefined

getLevel :: IO Int
getLevel = do
	input <- getLine
	return (read input :: Int)

loadState :: Int->State
loadState level = undefined

getInput :: IO Move
getInput = undefined

isValid :: Move->State->Int->Bool
isValid move state id= undefined

modify :: State->Move->State
modify state move = undefined



main = do
	level <- getLevel
	let state = loadState level
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