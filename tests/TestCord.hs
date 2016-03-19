module Main where
import GoWageWar.Board.Cord
import Test.QuickCheck

-- All distances in the circle are <= n
prop_distance n = all (\p -> manhattan p (0, 0) <= abs n) $ circle (abs n)

-- All distances up to n are in the circle 
prop_distance2 n = and [elem d xs| d <- [0..(abs n)]]
    where
        xs = map (manhattan (0, 0)) $ circle (abs n)

main = do
        quickCheck prop_distance
        quickCheck prop_distance2
