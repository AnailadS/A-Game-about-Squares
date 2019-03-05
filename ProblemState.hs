{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module ProblemState where

{-
	This class defines the functions that generate and evaluate
	the states in the state space.
-}
class ProblemState s a | s -> a where
    {-
		For the current state, generate all the (a, s) pairs, where
			s = next state
			a = action that led to next state.
    -}
    successors :: s -> [(a, s)]

    {-
		Returns 'True' if the current state is the final one(solution).
    -}
    isGoal :: s -> Bool

    {-
		Returns the distance(current state, final state/solution)).
		The smaller the distance, the better the state.
    -}
    heuristic :: s -> Int
    heuristic = const 0
