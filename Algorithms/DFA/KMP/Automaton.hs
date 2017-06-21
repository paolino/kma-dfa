

-- | Automaton behind KMP implementatio
{-# language GeneralizedNewtypeDeriving, DeriveFunctor #-}

module Algorithms.DFA.KMP.Automaton where

import Data.Array (listArray , (!))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

-- | Index to select the DFA states
newtype Index = Index Int deriving (Integral, Real, Num, Enum, Ord, Eq, Show)

-- | Type of effect of a transition on the stream
data Jump a     = Hold a -- ^ hold on the stream 
                | Step a -- ^ step the stream 
                deriving Functor

-- | Machine transition
type Transition m b a = a -> m (Jump b)

-- | State functional definition
type Process m a = Transition m (Automaton m a) a

-- | State application definition
type Interface m a = Transition m Index a

-- | A wrapper do deal with types cycle 
newtype Automaton m a = Automaton (Process m a) 

-- | backend, set the complexity
type Query m a = Index -> Automaton m a

-- | from 'Interface' to 'Process', given a 'Query' as backend
core    :: (Eq a, Functor m)
        => Query m a  -- ^ backend
        -> Interface m a -- ^ relational definition
        -> Process m a -- ^ functional definition
core m h x = fmap m <$> h x

-- | build a Automaton from a non empty list of 'Interface', one for each state. 
-- An 'Array' backend is used to select states in O(1)
automatonWithArray :: (Eq a, Functor m) => NonEmpty (Interface m a) -> Automaton m a
automatonWithArray xs = let
    a = listArray (0, NE.length xs) $ NE.toList ys
    ys = NE.map (Automaton . core ((a !) . fromIntegral)) $ xs
    in a ! 0

-- | Chances to step
data Run m a 
    = Run (m (Run m a))   -- ^ can step 
    | Feed ([a] -> Run m a) -- ^ must be fed

-- | start running an automaton
run :: Monad m => Automaton m a -> [a] -> Run m a
run a [] = Feed $ run a
run (Automaton a) xs@(x:xt) = Run $ do
    r <- a x
    return $ case r of 
        Hold a' -> run a' xs
        Step a' -> run a' xt
    
consume :: Monad m => Automaton m a -> [a] -> m ()
consume a xs = let
    f (Feed _) = return ()
    f (Run g) = g >>= f
    in f (run a xs)


{-
-- | run an 'Automaton' against a stream
-- When result is Left compute the 'Automaton' state after the last value processed,
-- when result is Right compute the *b* of the last reset and the remaining stream
run     :: ([a] -> b)
        -> Automaton a  -- ^ initial machine state
        -> [a] -- ^ stream of input
        -> Run a b -- ^ reached machine state 
run g m0 xs = run' xs m0 xs where
        run' rs m [] = Supply $ \xs -> run' (rs ++ xs) m xs  
        run' rs (Automaton m) xt@(x:xs) =  case m x of
                Accept -> Match $ (g rs, run' xs m0 xs)
                Hold t s -> run' (if t then xt else rs) s xt
                Step t s -> run' (if t then xs else rs) s xs 
-}

