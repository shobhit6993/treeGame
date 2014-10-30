import System.Environment(getArgs)
import System.Random
import Control.Monad.Trans
import Control.Monad.IO.Class
import Graphics.UI.GLUT
import Data.IORef
import Data.Char
import Control.Monad
import GameUtils
import Levels
import Control.Monad.State.Lazy
import qualified Foreign.C.Types

type NDouble = Foreign.C.Types.CDouble

main = do
	initialWindowSize $= Size 1300 700
	initialDisplayMode $= [DoubleBuffered]
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "treeGame"
	--displayCallback $= display
	reshapeCallback $= Just reshape
	state 		<- newIORef 0
	treeNum 	<- newIORef 0
	srcNum 		<- newIORef 0
	destNum 	<- newIORef 0

	args <- getArgs
	gs 			<- newIORef (gameSt args)
	--state 	<- get state'
	gs' <- readIORef gs

	if (read $ head args :: Int) == 1
	then displayCallback $= display state treeNum srcNum destNum gs (points gs')
	else displayCallback $= display' state treeNum srcNum destNum gs (points gs')

	keyboardMouseCallback $= Just (keyboardMouse ((read $ head args :: Int) == 1) state treeNum srcNum destNum gs) 
	
	mainLoop
	where

		levelid args = (read $ head (tail args) :: Int)
		points gs = (computeTreeListCoordinates (treeList gs) (-1.05) (-0.9) (-0.9))
		gameSt args = initgs (levelid args) 0 
				
playGame :: IORef Int -> IORef Int -> IORef Int -> IORef GameState -> IO ()
playGame treeNum' srcNum' destNum' gs' = do
	treeNum <- readIORef treeNum'
	srcNum <- readIORef srcNum'
	destNum <- readIORef destNum'
	gs <- readIORef gs'	
	if (isValid  [treeNum, srcNum, destNum] (treeList gs) (getPlayerID (turn gs)))
		then modifyIORef gs' $ playMove (treeNum, srcNum, destNum)
		else return ()



drawAllTrees [] [] treeIdx = return ()
drawAllTrees (tree:treelist) (points:pointslist) treeIdx = do
	drawTree tree tree points treeIdx
	drawAllTrees treelist pointslist (treeIdx + 1)

display :: IORef Int -> IORef Int -> IORef Int -> IORef Int -> IORef GameState -> [[(Int, (GLfloat, GLfloat))]] -> DisplayCallback
display state' treeNum' srcNum' destNum' gs' points = do
	gs <- readIORef gs'
	if (turn gs)
	then display' state' treeNum' srcNum' destNum' gs' points
	else display'' gs' points

display' state' treeNum' srcNum' destNum' gs' points = do
	state 	<- Graphics.UI.GLUT.get state'
	treeNum <- Graphics.UI.GLUT.get treeNum'
	srcNum	<- Graphics.UI.GLUT.get srcNum'
	destNum <- Graphics.UI.GLUT.get destNum'
	gs <- Graphics.UI.GLUT.get gs'
	playGame treeNum' srcNum' destNum' gs'
	clear [ColorBuffer,DepthBuffer]
	color $ Color3 0 1 (0 :: GLfloat)
	currentRasterPosition $= vertex4 (-0.99) (0.8) 0 1
	renderString Fixed8By13 (displayString state treeNum srcNum destNum)
	currentRasterPosition $= vertex4 (-0.99) (0.7) 0 1
	renderString Fixed8By13 (displayString' state treeNum srcNum destNum)
	--currentRasterPosition $= vertex4 (-0.75) (0.3) 0 1
	--renderString Fixed8By13 "Jhu"
  	--renderPrimitive Points $ mapM_ (\(x, (y, z)) -> vertex $ Vertex2 ((fromIntegral (y-2) :: GLfloat)/3) ((fromIntegral (z-2) :: GLfloat)/3)) points
	drawAllTrees (treeList gs) points 0

	if ((isFinished (treeList gs)) || (prediction gs == []))
		then drawString (-0.99, 0.9) ("Game Over!! " ++ (currentTurn gs) ++ " wins!!")
		else drawString (-0.99, 0.9) ((currentTurn' gs) ++ "'s turn")
	--drawTree (tree0) (nodeList tree0) (points !! 0)
	swapBuffers
	where
		prediction gs = computerMove (if (turn gs)==True then 0 else 1) (treeList gs) 
		displayString state treeNum srcNum destNum = case state of
			0 -> "Enter Tree Number: "
			1 -> "Entered Tree Number: " 		++ (show treeNum)
			2 -> "Entered Source Number: " 		++ (show srcNum)
			3 -> "Entered Destination Number: " ++ (show destNum)

		displayString' state treeNum srcNum destNum = case state of
			0 -> ""
			1 -> "Enter Source Number: "
			2 -> "Enter Destination Number: "
			3 -> "Play Move?"

		currentTurn gs
			| (turn gs) = "Player 2"
			| otherwise = "Player 1"

		currentTurn' gs
			| (turn gs) = "Player 1"
			| otherwise = "Player 2"

playAIMove gs = GameState{ turn = not (turn gs), treeList = (GameUtils.modify (treeList gs) move)}
	where
		move = computerMove pid (treeList gs)
		pid = if (turn gs)==True then 0 else 1

display'' gs' points = do
	gs <- Graphics.UI.GLUT.get gs'
	clear [ColorBuffer,DepthBuffer]
	color $ Color3 0 1 (0 :: GLfloat)
	modifyIORef gs' $ playAIMove
	drawAllTrees (treeList gs) points 0
	if (isFinished (treeList gs))
		then drawString (-0.99, 0.9) ("Game Over!! " ++ (currentTurn gs) ++ " wins!!")
		else drawString (-0.99, 0.9) ((currentTurn' gs) ++ "'s turn")
	swapBuffers
	where
		currentTurn gs
			| (turn gs) = "Player 2"
			| otherwise = "Player 1"

		currentTurn' gs
			| (turn gs) = "Player 1"
			| otherwise = "Player 2"

drawLine :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> Int -> IO ()
drawLine (x1, y1) (x2, y2) colour = renderPrimitive Lines $ do
						color $ getColour colour
						vertex $ Vertex2 ((x1) :: GLfloat) ((y1) :: GLfloat)
						vertex $ Vertex2 ((x2) :: GLfloat) ((y2) :: GLfloat)
						where
							getColour colour
								| colour == 1 		= Color3 1 0 (0 :: GLfloat)
								| colour == 2  		= Color3 0 0 (1 :: GLfloat)
								| otherwise 		= Color3 0 1 (0 :: GLfloat) 

drawString (x,y) str = do
	color $ Color3 1 1 (1 :: GLfloat)
	currentRasterPosition $= vertex4 ((x) :: GLfloat) ((y) ::GLfloat) 0 1
	renderString Fixed8By13 str

drawNode :: Tree -> [(Int,(GLfloat,GLfloat))] -> Int -> Int -> IO()
drawNode tree points root treeIdx = do
	drawEdge root dstList points
	drawString (findCoord root points) (show root)
	drawTreeNum root (findCoord root points) treeIdx
	where
		dstList = getDestList tree root
		findCoord node (x:points) = if (node == (fst x))
										then (snd x)
										else findCoord node points
		drawEdge src [] points = return ()
		drawEdge src (dest : destList) points = do
			if fst(dest) /= -1 then drawLine (findCoord src points) (findCoord (fst dest) points) (snd dest) else return ()
			drawEdge src destList points 

drawTreeNum root (x,y) treeIdx = if root == 1 then drawString (x-0.05,y-0.05) ("Tree " ++ (show treeIdx)) else return ()


drawTree tree [] points treeIdx = return ()
drawTree tree (node:nodeList) points treeIdx = do
	drawTree tree nodeList points treeIdx
	--print (src node)
	if ((nodeId node) == -1) 
		then return () 
		else drawNode tree points (nodeId node) treeIdx


postOrderList :: [Tree] -> Int -> Int -> [[(Int, Int)]]
postOrderList treeList root depth =
	foldl (\acc tree -> (postOrder tree root depth) : acc) [] treeList

postOrder :: Tree -> Int -> Int -> [(Int, Int)]
postOrder tree root depth =
	foldl (\acc (x, _) -> (if x /= -1 then ((postOrder tree x (depth + 1)) ++ acc) else acc)) [(root, depth)] dstList
	where
		dstList = getDestList tree root


plotted' :: [(Int, (GLfloat, GLfloat))] -> GLfloat -> (Int, Int) -> Tree -> GLfloat -> [(Int, (GLfloat, GLfloat))]
plotted' plotted maxX x tree yoffset
	| length (getDestList tree (fst x)) == 0 	= (((fst x),(maxX + 0.1, ((fromIntegral (snd x) :: GLfloat)/7) + yoffset)) :plotted)
	| otherwise									= ((fst x,( getChildAvg tree plotted (fst x), ((fromIntegral (snd x) :: GLfloat)/7) + yoffset)):plotted) 

maxX' :: GLfloat -> (Int, Int) -> Tree -> GLfloat
maxX' maxX x tree
	| length (getDestList tree (fst x)) == 0 	= maxX + 0.1
	| otherwise									= maxX


maxY' :: GLfloat -> (Int, Int) -> Tree -> GLfloat -> GLfloat
maxY' maxY x tree yoffset
	| (((fromIntegral (snd x) :: GLfloat)/7) + yoffset > maxY) 	= ((fromIntegral (snd x) :: GLfloat)/7) + yoffset
	| otherwise											= maxY




getChildAvg tree plotted src = (sumX tree plotted src) / (lenX tree plotted src)

sumX :: Tree -> [(Int, (GLfloat, GLfloat))] -> Int -> GLfloat
sumX tree plotted src = foldl(\acc x -> acc + x) 0.0 (getChildX tree plotted src)

lenX :: Tree -> [(Int, (GLfloat, GLfloat))] -> Int -> GLfloat
lenX tree plotted src = fromIntegral (length (getChildX tree plotted src)) :: GLfloat

getChildX tree plotted src = map(\x -> (getX plotted (fst x))) (getDestList tree src)

--get [] node = -1
getX (x:xs) node = if fst(x) == node then (fst(snd(x))) else (getX xs node)



computeTreeCoordinates :: Tree -> [(Int, Int)] -> [(Int, Int)] -> [(Int, (GLfloat, GLfloat))] -> GLfloat -> GLfloat -> GLfloat -> ([(Int, (GLfloat, GLfloat))], GLfloat, GLfloat, GLfloat)
computeTreeCoordinates tree [] (backupPostOrder) plotted maxX maxY yoffset = if (maxX > 1.0)
																				then computeTreeCoordinates tree backupPostOrder backupPostOrder [] (-1.1) maxY (maxY+0.1)
																				else (plotted, maxX, yoffset, maxY)
computeTreeCoordinates tree (x:xs) (backupPostOrder) plotted maxX maxY yoffset =
	computeTreeCoordinates tree xs (backupPostOrder) (plotted' plotted maxX x tree yoffset) (maxX' maxX x tree) (maxY' maxY x tree yoffset) yoffset




--getCurMaxX [] endTree
--getCurMaxX (tree:treeList) endTree 

computeTreeListCoordinates :: [Tree] -> GLfloat -> GLfloat -> GLfloat -> [[(Int, (GLfloat, GLfloat))]]
computeTreeListCoordinates [] xoffset yoffset maxY = []
computeTreeListCoordinates (tree:treeList) xoffset yoffset maxY = (treePoints : (computeTreeListCoordinates treeList xoffset' yoffset' maxY'))
	where
		(treePoints, xoffset', yoffset', maxY') = computeTreeCoordinates tree (postOrder tree 1 0) (postOrder tree 1 0) [] xoffset maxY yoffset



keyboardMouse :: Bool -> IORef Int -> IORef Int -> IORef Int -> IORef Int -> IORef GameState -> KeyboardMouseCallback
keyboardMouse isAI state treeNum srcNum destNum gs key Down _ _ = case key of
	(Char '0') -> modifyNum '0' treeNum srcNum destNum state
	(Char '1') -> modifyNum '1' treeNum srcNum destNum state
	(Char '2') -> modifyNum '2' treeNum srcNum destNum state
	(Char '3') -> modifyNum '3' treeNum srcNum destNum state
	(Char '4') -> modifyNum '4' treeNum srcNum destNum state
	(Char '5') -> modifyNum '5' treeNum srcNum destNum state
	(Char '6') -> modifyNum '6' treeNum srcNum destNum state
	(Char '7') -> modifyNum '7' treeNum srcNum destNum state
	(Char '8') -> modifyNum '8' treeNum srcNum destNum state
	(Char '9') -> modifyNum '9' treeNum srcNum destNum state
	(SpecialKey KeyRight) -> if isAI
							then modifyState state treeNum srcNum destNum
							else modifyState' state treeNum srcNum destNum
	_ -> return ()

	where
		modifyNum'' num key = do
			num $~! (* 10)
			num $~! (+ (ord key - ord '0'))
		modifyNum key treeNum srcNum destNum state = do
			state' <- Graphics.UI.GLUT.get state
			modifyNum' key treeNum srcNum destNum state'
			where
				modifyNum' key treeNum srcNum destNum state = case state of
					0 -> modifyNum'' treeNum key
					1 -> modifyNum'' srcNum  key
					2 -> modifyNum'' destNum key
					_ -> return ()
		modifyState state treeNum srcNum destNum = do
			gs' <- readIORef gs
			if (turn gs')
			then modifyState' state treeNum srcNum destNum
			else postRedisplay Nothing

		modifyState' state treeList srcNum destNum = do
			state $~! (+1)
			state $~! (`mod` 4)
			resetNums state treeNum srcNum destNum
			postRedisplay Nothing
			where
				resetNums state treeNum srcNum destNum = do
					state' <- Graphics.UI.GLUT.get state
					if (state' == 0)
						then resetNums' treeNum srcNum destNum
						else return ()
						where
							resetNums' treeNum srcNum destNum = do
								treeNum $~! (*0)
								srcNum 	$~! (*0)
								destNum $~! (*0) 

keyboardMouse _ _ _ _ _ _ _ _ _ _ = return ()

vertex4 = Vertex4

reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
