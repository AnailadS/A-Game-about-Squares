{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState

import qualified Data.Set as S
import qualified Data.List as L
{-
	The node of the search tree contains information about :
	* current state;
	* action that led to this state;
	* parent node
	* depth/level in tree
-}
data Node s a = Root { state :: s, d :: (Int) } |
				Node
				{ state :: s
				, action :: a
				, parent :: (Node s a)
				, d :: Int
				} deriving (Eq, Show)

{-
	Returns the state of a node.
-}
nodeState :: Node s a -> s
nodeState (Root s _) = s
nodeState (Node s _ _ _) = s

kidzNodesFactory :: (Ord s, ProblemState s a) =>S.Set s -> Node s a -> Bool -> [(Node s a)]
kidzNodesFactory visited node b = map makeNode (filter notMyKidz kidz)
						where
							st = state(node)
							depth = d (node)
							kidz = if b then (L.sortBy myComp (successors st)) else (successors st)
							notMyKidz = \(_, s) -> S.notMember s visited
							makeNode = \(a, s) -> (Node s a node ( depth+1))
							myComp = \(_, s1) (_, s2) -> if (heuristic s1) >= (heuristic s2) then GT else LT

explore :: (Ord s, ProblemState s a) => Node s a -> [(Node s a)] -> Int -> Bool -> [(Node s a)]
explore node path depth b = if (d (node)) == depth
									then node:path
									else  foldl digger (node:path) (kidzNodesFactory seen node b)
									where
										digger = \path -> \n -> explore n path depth b
										seen = S.fromList (map (\n -> state(n)) path)

{-
	Returns the list of nodes found with limited dfs in state space.

-}
limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Initial state
           -> Bool        -- Set True if you want to use the heuristic.
           -> Int         -- Max depth
           -> [Node s a]  -- List of nodes.

limitedDfs initialState b depth  = if depth == 0
									then [(Root initialState 0)]
									else tail (reverse (explore (Root initialState 0) [(Root initialState 0)] depth b))


findGoal :: (ProblemState s a, Ord s) => [(Node s a)] -> Int
findGoal [] = 1
findGoal (x : xs) = if isGoal (state(x))
					then 1
					else 1 + (findGoal xs)


try :: (ProblemState s a, Ord s) => s -> Bool -> Int  -> Int-> (Node s a, Int)
try s b x sum = if goal <= (length nodes) 
				then ((nodes !! (goal-1)), goal + sum - 1  )
				else (try s b (x + 1) (sum + (length nodes)))
					where
						goal = findGoal nodes
						nodes = (limitedDfs s b x)

{-
	Explore the state space, using iterative deepening, until
	the final state/solution is found.
-}
iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Initial state
    -> Bool             -- Set True for heuristic
    -> (Node s a, Int)  -- (Node with first final state,
                        -- number of non-final states visited.)
iterativeDeepening s b = try s b 0 0

{-
	Starting from a node, restores the path to initial node.
	Return a list of nodes(action, state) without the final state.
-}
extractPath :: Node s a -> [(a, s)]
extractPath (Root _ _) = []
extractPath (Node s a parent _) = (extractPath parent) ++ [(a, s)]

{-
	Can be used to print each element of a list on a separated line.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
