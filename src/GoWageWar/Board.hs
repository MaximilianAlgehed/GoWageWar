module GoWageWar.Board
    (
        Colour (..),
        Tower (..),
        Board,
        Cord,
        manhattan,
        circle,
        absoluteInfluences,
        applyInfluenceChange
    ) where
import Data.Matrix

-- | Colour of a player
data Colour = Red
            | Blue
            deriving (Ord, Eq, Show)

-- | The sign of a colour
colourSignum :: Colour -> Int
colourSignum Red  = -1
colourSignum Blue =  1

-- | Different types of towers
data Tower  = Wall
            | Watchtower
            | Keep
            deriving (Ord, Eq, Show)

-- | The influence type
type Influence = Int

-- | The type of a cell
type Cell      = (Maybe (Tower, Colour), Influence)

-- | The type of the board
type Board     = Matrix Cell

-- | A Board co-ordinate
type Cord      = (Int, Int)

-- | Add one Cord to another
addC :: Cord -> Cord -> Cord
addC (x, y) (z, w) = (x+z, y+w)

-- | Check if a Cord is in range of a board
inRange :: Board -> Cord -> Bool
inRange b (x, y) = (x > 0) && (x <= (ncols b)) && (y > 0) && (y <= (nrows b))

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
influence :: Tower -> [(Influence, Cord)]
influence Wall       = decCircle 6 2 1
influence Watchtower = decCircle 4 2 2
influence Keep       = decCircle 6 2 0

-- | A circle of decreasing influence in an area around (0, 0)
decCircle :: Influence -> Int -> Int -> [(Influence, Cord)]
decCircle value rate r = [(value `div` (rate^(manhattan (0, 0) p)), p) | p <- circle r]

-- | Get a list of influences in absolute board terms
absoluteInfluences :: Board -> [(Influence, Cord)]
absoluteInfluences b =
    do
        i <- [1..(ncols b)] -- Get the column
        j <- [1..(nrows b)] -- Get the row
        (Just (t, c), _) <- return $ b!(i, j) -- Get the tower and colour
        -- Get the new value
        let influences = influence t
            absinfs = map (\(influ, cord) -> (influ * (colourSignum c), addC (i, j) cord)) influences
        filter ((inRange b) . snd) absinfs

-- | Apply influence changes to a board
applyInfluenceChange :: Board -> [(Influence, Cord)] -> Board
applyInfluenceChange b xs = applyInfluenceChange' (fmap (\(x, y) -> (x, 0)) b) xs
    where
        applyInfluenceChange' :: Board -> [(Influence, Cord)] -> Board
        applyInfluenceChange' b' []             = b'
        applyInfluenceChange' b' ((x, cord):xs) = let (v, curinf) = b'!cord in
                                                  applyInfluenceChange' (setElem (v, curinf+x) cord b') xs
