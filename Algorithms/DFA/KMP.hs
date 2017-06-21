-- | KMP logic 
{-# language ScopedTypeVariables #-}

module Algorithms.DFA.KMP where

import Data.List (mapAccumL,tails)
import Algorithms.DFA.KMP.Automaton
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Function
import qualified Data.List.NonEmpty as NE
import Control.Monad.Writer



-- | A list of longest suffix which is prefix length. 
-- This is the classical KMP prefix table with indexes row shifted right by one
table  :: Eq a 
        => NonEmpty a  -- pattern
        -> Table a
table (x :| []) = Single x
table xt@(x :| xs) = Multi x (zip xs is) z where
        xz = toList xt
        ((z,_),is) = mapAccumL f (0, xz) $ xs
        f (n, z:zs)  x
                | x == z = ((n + 1, zs), n)
                | otherwise = ((0, xz), n) 

data Table a = Single a | Multi a [(a,Index)] Index deriving Show

data Logic m a b  = Logic 
    {   fromStream :: b -> a
    ,   logZero :: b -> m ()
    }

logic :: forall c b a m . (Monad m , Eq a) =>  Logic m a b -> Table a -> NonEmpty (Interface m b)
logic (Logic fromStream logZero) = logic'  where
    match x f g y 
        | x == fromStream y = f y
        | otherwise = return g
    
    logic' (Single x) = r :| [] where
        r = match x (\y -> logZero y >> return (Step 0)) (Step 0)  

    logic' (Multi x xs zi) = r :| core (zip [2..] xs) where
        r = match x (const $ return $ Step 1) (Step 0)
        core [(_,(x,i))] = 
            [match x (\y -> logZero y >> return (Step zi)) (Hold i)]
        core ((n,(x,i)) : xs) =
            match x (const $ return $ Step n) (Hold i) : core xs

(==!) :: Eq a 
        => [a] -- ^ pattern
        -> [a] -- ^ stream
        -> Bool
p ==! s = (>0) . length $ matches p s

matches :: Eq a => [a] -> [a] -> [Int]
matches pt@(p : ps) s 
        = execWriter 
        $ consume 
        (automatonWithArray 
            $ logic (Logic snd (tell . return . subtract (length ps) . fst)) 
            $ table $ p :| ps
            ) 
        $ zip [0..] s 

