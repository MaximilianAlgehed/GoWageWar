module GoWageWar.Board
    (
        Colour (..),
        Tower (..),
        Board,
        Cord,
        manhattan,
        circle,
        influence
    ) where
import Data.Matrix

-- | Colour of a player
data Colour = Red
            | Blue
            deriving (Ord, Eq, Show)

-- | Different types of towers
data Tower  = Wall
            | Watchtower
            | Keep
            deriving (Ord, Eq, Show)

-- | The influence type
type Influence = Double

-- | The type of a cell
type Cell   = Maybe (Tower, Colour, Influence)

-- | The type of the board
type Board  = Matrix Cell

-- | A Board co-ordinate
type Cord   = (Int, Int)

-- | Add one Cord to another
addC :: Cord -> Cord -> Cord
addC (x, y) (z, w) = (x+z, y+w)

-- | Get the manhattan distance from one cord to another
manhattan :: Cord -> Cord -> Int
manhattan (a, b) (x, y) = abs (a - x) + abs (b - y)

-- | Get all cords in a manhattan radius around (0, 0)
circle :: Int -> [Cord]
circle 0 = [(0, 0)]
circle n = circle (n-1) ++ lst ++ map (\(x, y) -> (-x, -y)) lst
    where
        lst = [(x, n-x) | x <- [0..n]]

-- | The influence of a tower
influence :: Tower -> [(Double, Cord)]
influence Wall       = decCircle 3 3 1
influence Watchtower = decCircle 2 2 2
influence Keep       = decCircle 4 2 0

-- | A circle of decreasing influence in an area around (0, 0)
decCircle :: Double -> Double -> Int -> [(Double, Cord)]
decCircle value rate r = [(value/(rate**(fromIntegral (manhattan (0, 0) p))), p) | p <- circle r]
