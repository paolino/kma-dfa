{- KMP algorithm implementation based on Deterministic Finite State Automata -}

module Algorithms.DFA.KMP where

import Data.Bifunctor (bimap)
import Data.Maybe (isNothing)
import Data.Array (listArray , (!))
import Data.List (mapAccumL)

-- | a specialized DFA with distinguish between Left and Right jumps. Nothing signals a final state
newtype DFA a = DFA (a -> Maybe (Either (DFA a) (DFA a)))

-- | create a DFA step
step    :: Eq a 
        => (Int -> DFA a) -- ^ index to state solver
        ->  [(a,Int)] -- ^ Right argument to index list
        -> Either Int Int  -- ^ fallback index
        -> [a] -- ^ final state arguments
        -> a -- ^ the selector
        -> Maybe (Either (DFA a) (DFA a)) 
step m rs l ns c 
        | c `elem` ns = Nothing
        | True = Just $ maybe (bimap m m  l) (Right . m) $ lookup c rs

-- | run the automata against an input Nothing signal success
run :: DFA a -> [a] -> Maybe (DFA a)
run m [] = Just m
run (DFA m) (x:xs) = m x >>= either (flip run $ x:xs) (flip run xs)

-- | build a DFA from a pattern zipeed with the prefix
mkDFA :: Eq a => [(a,Int)] -> DFA a
mkDFA xs = let
    a = listArray (0,length xs) ys
    m = step (a !)
    ys = map DFA $ from xs
    from [(x,0)] = [m [] (Right 0) [x]]
    from ((x,0):xs) = m [(x,1)] (Right 0) [] : core (zip [2..] xs)
    core [(_,(x,i))] = [m  [] (Left i) [x]]
    core ((n,(x,i)) : xs) = m [(x,n)] (Left i) [] : core xs
    in a ! 0

-- | A list of prefixes to serve mkDFA
prefix :: Eq a => [a] -> [Int]        
prefix xs = (0:) . snd . mapAccumL f (0,xs) $ tail xs where
        f (n, z:zs)  x
                | x == z = ((n + 1,zs),n)
                | otherwise = ((0,xs),n)        

-- | test a match exists
match :: Eq a => [a] -> [a] -> Bool
match p s = isNothing $ run (mkDFA $ zip <*> prefix $ p) s


