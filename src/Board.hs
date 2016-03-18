module Board
    (
        Colour,
        Tower,
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

-- | The type of a cell
type Cell   = Maybe (Tower, Colour)

-- | The type of the board
type Board  = Matrix Cell

-- | A Board co-ordinate
type Cord   = (Int, Int)

-- Get the manhattan distance from one cord to another
manhattan :: Cord -> Cord -> Int
manhattan (a, b) (x, y) = abs (a - x) + abs (b - y)

-- Get all cords in a manhattan radius around (0, 0)
circle :: Int -> [Cord]
circle 0 = [(0, 0)]
circle n = circle (n-1) ++ lst ++ map (\(x, y) -> (-x, -y)) lst
    where
        lst = [(x, n-x) | x <- [0..n]]

influence :: Tower -> [(Double, Cord)]
influence Wall       = [(2, (0, 0)), (1, (1, 1)), (1, (-1, 1)), (1, (-1, -1)), (1, (1, -1))] 
influence Watchtower = undefined 
influence Keep       = [(3, (0, 0))]
