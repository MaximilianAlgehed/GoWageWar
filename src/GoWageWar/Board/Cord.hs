module GoWageWar.Board.Cord
    (
        Cord,
        Direction(..),
        manhattan,
        addC,
        circle,
        directionToCord
    ) where
import Prelude hiding (Left, Right)
import Data.List

-- | A Board co-ordinate
type Cord = (Int, Int)

-- | Add one Cord to another
addC :: Cord -> Cord -> Cord
addC (x, y) (z, w) = (x+z, y+w)

-- | Get the manhattan distance from one cord to another
manhattan :: Cord -> Cord -> Int
manhattan (a, b) (x, y) = abs (a - x) + abs (b - y)

-- | Get all cords in a manhattan radius around (0, 0)
circle :: Int -> [Cord]
circle 0 = [(0, 0)]
circle n = nub $ circle (n-1) ++ lst ++ map (\(x, y) -> (-x, -y)) lst
    where
        lst = [(x, n-x) | x <- [0..n]] ++ [(-x, n-x)| x <- [0..n]]

-- | A direction
data Direction = Up
               | Down
               | Left
               | Right

-- | Convert a direction to a Cord
directionToCord :: Direction -> Cord
directionToCord Up    = (-1, 0)
directionToCord Down  = (1, 0)
directionToCord Left  = (0, -1)
directionToCord Right = (0, 1)
