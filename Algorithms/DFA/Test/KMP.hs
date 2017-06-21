{- Test against isPrefixOf -}

module Algorithms.DFA.Test.KMP where

import Algorithms.DFA.KMP ((==!))
import Test.QuickCheck
import Data.List
import Control.Monad
import Debug.Trace

-- | make the result as isPrefixOf application
rif :: Eq a => [a] -> [a] -> Bool
rif p s = any (isPrefixOf p) $ tails s

-- | Correct results
data Rif a = Rif [a] [a] Bool deriving Show

instance (Show a, Eq a, Arbitrary a) => Arbitrary (Rif a) where
        arbitrary = do
                n <- choose (1,10)
                p <- vectorOf n arbitrary
                m <- choose (0,4*n)
                s <- vectorOf m arbitrary
                return $ Rif p s (rif p s)

-- | use Bools for high repetition and matching results
test :: Rif Bool -> Bool
test (Rif p s r) = p ==! s == r


