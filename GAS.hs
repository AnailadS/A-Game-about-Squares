{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M

{-
	Board positions, in (line, column) format, where both coordinates can be
	negative numbers.
-}
type Position = (Int, Int)

{-
	Color information for squares and circles.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
	Heading information for squares and arrows.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
	An object on the board can be square, cicle, arrow.
	Empty is defined to represent empty cells.
-}
data Object = Square Color Heading |
				Circle Color|
				Arrow  Heading|
				Empty
    deriving (Eq, Ord)

{-
	Text representation for every object.
-}

instance Show Object where
    show (Arrow h) = show h

    show (Square c h)  = letter ++ (show h)
    						where letter
    							| c == Red = "R"
    							| c == Blue = "B"
    							| otherwise = "G"

    show (Circle c )  = letter
						where letter
							| c == Red = "r"
							| c == Blue = "b"
							| otherwise = "g"
    show Empty = " "


{-
	A spot = object/objects in a cell. (Sometimes, there will be two
	objects on the same position - e.g. : square and circle.
	If a cell has only one object (e.g. : Circle) we represent it as
	(Circle, Empty).
-}
data Spot = Spot (Object, Object) deriving (Eq, Ord)

instance  Show Spot where
	show (Spot (Empty, o)) = show Empty ++ show Empty ++ show o
	show (Spot (o1, o2)) = show o1 ++ show o2

{-
	Data for a level : a map of
	(key = position of cell, value = objects in cell)
-}

data Level = Board (M.Map Position Spot) 
   deriving (Eq, Ord)

{-
    Text representation for a level.
-}

instance Show Level where
	show (Board m) = foldl (++) "" (map f [ (keyX, keyY) | keyX <- [heightMin..heightMax], keyY <- [widthMin..widthMax]] )
			where
				heightMax = maximum (map fst (M.keys m))
				widthMax = maximum (map snd (M.keys m))
				heightMin = minimum (map fst (M.keys m))
				widthMin = minimum (map snd (M.keys m))
				f (lin, col) | col == widthMax && lin == heightMax = show (M.findWithDefault (Spot (Empty, Empty)) (lin, col) m)
							 | col == widthMax =  (show (M.findWithDefault (Spot (Empty, Empty)) (lin, col) m) ++ "\n")
							 | otherwise = (show (M.findWithDefault (Spot (Empty, Empty)) (lin, col) m) ++ "|") 

{-
    Level without objects.
-}
emptyLevel :: Level
emptyLevel = Board M.empty

{-
	Add a square with given color and heading at the specified position.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare c h p (Board m) = Board (M.insert p (Spot ( (Square c heading), b)) m)
								where
									Spot (_, b)  =(M.findWithDefault (Spot (Empty, Empty)) p m)
									heading
										| b == (Arrow North) = North
										| b == (Arrow South) = South
										| b == (Arrow West) = West
										| b == (Arrow East) = East
										| otherwise = h


{-
	Add a circle with given color at the specified position.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle c p (Board m) = Board (M.insert p (Spot ( a , (Circle c))) m) where Spot (a, _) = (M.findWithDefault (Spot (Empty, Empty)) p m)

{-
	Add an arrow with the given heading at the specified position.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h p (Board m)= Board (M.insert p (Spot ( a , (Arrow h))) m) where Spot (a, _) = (M.findWithDefault (Spot (Empty, Empty)) p m)

{-
	Move the square from a specified position in a level. If at the given position
	there is no square, return the parameter. 
-}
push :: Object -> Position -> Heading -> Level -> Level
push (Square c h) (x,y) heading (Board i)	| e == Empty = addSquare c h (x, y) (Board i)
											| heading == North = addSquare c h (x, y) (push e (x-1, y) heading (Board i))
											| heading == South = addSquare c h (x, y) (push e (x+1, y) heading (Board i))
											| heading == East = addSquare c h (x, y) (push e (x, y+1) heading (Board i))
											| heading == West = addSquare c h (x, y) (push e (x, y-1) heading (Board i))
											where
												(Spot (e, _)) = (M.findWithDefault (Spot (Empty, Empty)) (x, y) i)
push _ _ _ l = l

move :: Position  -- Position
     -> Level     -- Initial level
     -> Level     -- Final level
move (x, y) (Board i)	| e == Empty = (Board i)
						| otherwise =
						   		case h of 
									North -> push (Square c h) (x-1, y) North (Board m) 
									South -> push (Square c h) (x+1, y) South (Board m)
									East -> push (Square c h) (x, y+1) East (Board m)
									West -> push (Square c h) (x, y-1) West (Board m)
									where	
										f = \ (Spot (_, o2)) -> if o2 == Empty
																	then Nothing
																	else Just (Spot (Empty, o2))
										m = M.update f (x, y) i
										Spot (Square c h, _) = M.findWithDefault (Spot (Square c h, Empty)) (x, y) i
										(Spot (e, _)) = (M.findWithDefault (Spot (Empty, Empty)) (x, y) i)

isMatch :: Spot -> Bool
isMatch (Spot (Square c1 _, Circle c2)) = c1 == c2
isMatch (Spot (_, _)) = False

hasSquare :: Spot -> Bool
hasSquare (Spot (Square _ _, _)) = True
hasSquare (Spot (_, _)) = False

getDistance :: Position -> Position -> Int
getDistance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

circlePair :: Color -> (Position, Spot)  -> Bool
circlePair c (_, Spot(_, Circle c2 )) = c == c2
circlePair _ _ = False

score :: M.Map Position Spot -> (Position, Spot) -> Int
score m (p, Spot (Square c _, _)) = (getDistance p p1) where p1 = fst (head ( filter (circlePair c) (M.toList m)))
score _ _ = 0

instance ProblemState Level Position where
{-
	To generate the successors, find each square and move it.
-}
    successors (Board m) = zip moves (map f moves)
    										where
    											f = \ p -> move p (Board m)
    											moves = M.keys (M.filter hasSquare m)

    isGoal (Board m) = M.size (M.filter isMatch m) == M.size (M.filter hasSquare m)

{-
	The heuristic sums the distances from each square to its corresponding
	circle.
-}
    heuristic (Board m) = foldl (+) 0 (map (score m)  (M.toList (M.filter hasSquare m)))
