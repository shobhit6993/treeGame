module Levels where

data GameState = GameState
				{
					turn :: Bool
					,treeList :: [Tree]
				}

data Node = Node 
			{
				nodeId :: Int
				,adjList :: [(Int,Char)]
			}	deriving (Show)

type Move = [Int] -- (tree no., source node id, dest node id). Size must be THREE (3)
type State = [Tree]
type Tree = [Node]

loadState :: Int->State
loadState levelNo = levels !! levelNo
	where levels =	[
						[ 
							[
								Node {nodeId = 1, adjList = []}
							]
						],

						--1
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

						--2
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

						],

						--3
						[
							[
								Node {nodeId = 1, adjList = []}
							],

							[
								Node {nodeId=1, adjList=[(2,'R')]},
								Node {nodeId=2, adjList=[(3,'G')]},
								Node {nodeId=3, adjList=[(4,'R')]},
								Node {nodeId=4, adjList=[]}
							],

							[
								Node {nodeId=1, adjList=[(2,'R')]},
								Node {nodeId=2, adjList=[]}
							],

							[
								Node {nodeId=1, adjList=[(2,'G')]},
								Node {nodeId=2, adjList=[]}
							]

						],

						--4
						[
							[
								Node {nodeId = 1, adjList = []}
							],
							
							[
								Node {nodeId = 1, adjList = [(2,'R'),(5,'B')]},
								Node {nodeId = 2, adjList = [(3,'R')]},
								Node {nodeId = 3, adjList = [(4,'B')]},
								Node {nodeId = 4, adjList = []},
								Node {nodeId = 5, adjList = [(6,'B')]},
								Node {nodeId = 6, adjList = []}
							]
						],

						--5
						[
							[
								Node {nodeId = 1, adjList = []}
							],

							[
								Node {nodeId = 1, adjList = [(2,'R'), (3,'G'), (4,'G')]},
								Node {nodeId = 2, adjList = [(5,'B')]},
								Node {nodeId = 3, adjList = [(6,'B'), (7,'G'), (8,'B'), (9,'G')]},
								Node {nodeId = 4, adjList = []},
								Node {nodeId = 5, adjList = []},
								Node {nodeId = 6, adjList = []},
								Node {nodeId = 7, adjList = []},
								Node {nodeId = 8, adjList = [(10,'G')]},
								Node {nodeId = 9, adjList = [(11,'B')]},
								Node {nodeId = 10, adjList = []},
								Node {nodeId = 11, adjList = []}
							],
							[
								Node {nodeId = 1, adjList = [(2,'R'),(5,'B')]},
								Node {nodeId = 2, adjList = [(3,'R')]},
								Node {nodeId = 3, adjList = [(4,'B')]},
								Node {nodeId = 4, adjList = []},
								Node {nodeId = 5, adjList = [(6,'B')]},
								Node {nodeId = 6, adjList = []}
							]
						],

						--6
						[
							[
								Node {nodeId = 1, adjList = []}
							],
							[
								Node {nodeId = 1, adjList = [(2,'R'), (3,'R')]},
								Node {nodeId = 2, adjList = [(4,'G')]},
								Node {nodeId = 3, adjList = [(5,'G')]},
								Node {nodeId = 4, adjList = [(6,'B'), (7,'B')]},
								Node {nodeId = 5, adjList = []},
								Node {nodeId = 6, adjList = []},
								Node {nodeId = 7, adjList = []}
							]
						],

						--7
						[
							[
								Node {nodeId = 1, adjList = []}
							],

							[
								Node {nodeId = 1, adjList = [(2,'G')]},
								Node {nodeId = 2, adjList = [(3,'R'), (4,'B')]},
								Node {nodeId = 3, adjList = [(5,'G'), (6,'G')]},
								Node {nodeId = 4, adjList = [(7,'R'), (8,'B')]},
								Node {nodeId = 5, adjList = []},
								Node {nodeId = 6, adjList = []},
								Node {nodeId = 7, adjList = []},
								Node {nodeId = 8, adjList = []}
								--Node {nodeId = 9, adjList = []},
								--Node {nodeId = 10, adjList = []},
								--Node {nodeId = 11, adjList = []},
								--Node {nodeId = 12, adjList = []},
								--Node {nodeId = 13, adjList = []}
							],
							[
								Node {nodeId = 1, adjList = [(2,'R'), (3,'R')]},
								Node {nodeId = 2, adjList = [(4,'G')]},
								Node {nodeId = 3, adjList = [(5,'G')]},
								Node {nodeId = 4, adjList = [(6,'B'), (7,'B')]},
								Node {nodeId = 5, adjList = []},
								Node {nodeId = 6, adjList = []},
								Node {nodeId = 7, adjList = []}
							],
							[
								Node {nodeId=1, adjList=[(2,'R')]},
								Node {nodeId=2, adjList=[(3,'G')]},
								Node {nodeId=3, adjList=[(4,'R')]},
								Node {nodeId=4, adjList=[]}
							],

							[
								Node {nodeId=1, adjList=[(2,'R')]},
								Node {nodeId=2, adjList=[]}
							]

						],

						--8
						[
							[
								Node {nodeId = 1, adjList = []}
							],
							[
								Node {nodeId = 1, adjList = [(2,'R'), (3,'G'), (4,'B'), (5,'G')]},
								Node {nodeId = 2, adjList = [(6,'R'), (7,'R'), (8,'B')]},
								Node {nodeId = 3, adjList = [(9,'G'), (10,'G'), (11,'B')]},
								Node {nodeId = 4, adjList = [(12,'G'), (13,'G'), (14,'R')]},
								Node {nodeId = 5, adjList = [(15,'G'), (16,'R'), (17,'B')]},
								Node {nodeId = 6, adjList = []},
								Node {nodeId = 7, adjList = []},
								Node {nodeId = 8, adjList = []},
								Node {nodeId = 9, adjList = []},
								Node {nodeId = 10, adjList = [(18,'R'), (19,'G')]},
								Node {nodeId = 11, adjList = []},
								Node {nodeId = 12, adjList = []},
								Node {nodeId = 13, adjList = []},
								Node {nodeId = 14, adjList = []},
								Node {nodeId = 15, adjList = []},
								Node {nodeId = 16, adjList = []},
								Node {nodeId = 17, adjList = []},
								Node {nodeId = 18, adjList = []},
								Node {nodeId = 19, adjList = []}
								--Node {nodeId = 20, adjList = []},
								--Node {nodeId = 21, adjList = []},
								--Node {nodeId = 22, adjList = []},
								--Node {nodeId = 23, adjList = []},
								--Node {nodeId = 24, adjList = []},
								--Node {nodeId = 25, adjList = []}
							] 
						]
					]