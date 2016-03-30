module GoWageWar.Board
    (
        module GoWageWar.Board.Cord,
        Colour (..),
        colourSignum,
        Tower (..),
        price,
        Cell,
        Board,
        Influence,
        Resources,
        endTurn,
        recalculateInfluence,
        addTower,
        calculateResources,
        clamp,
        move
    ) where

import GoWageWar.Board.Cord
import Data.Matrix
import Control.Monad

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

-- | The radius in which keeps generate resources
resourceRadius :: Int
resourceRadius = 2

-- | The price of a tower
price :: Tower -> Resources
price Keep       = 5
price Wall       = 3
price Watchtower = 2

-- | The influence type
type Influence = Int

-- | The resource type
type Resources = Int

-- | The influence of a tower
influence :: Tower -> [(Influence, Cord)]
influence Wall       = decCircle 6 2 1
influence Watchtower = decCircle 4 2 2
influence Keep       = decCircle 6 2 0

-- | A circle of decreasing influence in an area around (0, 0)
decCircle :: Influence -> Int -> Int -> [(Influence, Cord)]
decCircle value rate r = [(value `div` (rate^(manhattan (0, 0) p)), p) | p <- circle r]

-- | The cost of a tower
cost :: Tower -> Resources
cost Wall       = 1
cost Watchtower = 3
cost Keep       = 4

-- | The type of a cell
type Cell      = (Maybe (Tower, Colour), Influence)

-- | The type of the board
type Board     = Matrix Cell

-- | Check if a Cord is in range of a board
inRange :: Board -> Cord -> Bool
inRange b (x, y) = (x > 0) && (x <= (nrows b)) && (y > 0) && (y <= (ncols b))

-- | Get a list of influences in absolute board terms
absoluteInfluences :: Board -> [(Influence, Cord)]
absoluteInfluences b =
    do
        i <- [1..(nrows b)] -- Get the column
        j <- [1..(ncols b)] -- Get the row
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

-- | Kill non-influential towers
killTowers :: Board -> Board
killTowers = fmap kill
    where
        -- | Kill a tower on a cell if it has no influence
        kill :: Cell -> Cell
        kill (Just (t, c), i)
            | i `div` (colourSignum c) < 0 = (Nothing, i)
            | otherwise                    = (Just (t, c), i)
        kill x                             = x                             

-- | Recalculate the influence on the board
recalculateInfluence :: Board -> Board
recalculateInfluence board = applyInfluenceChange board (absoluteInfluences board)

-- | End the turn
endTurn :: Board -> Board
endTurn = recalculateInfluence . killTowers . recalculateInfluence

-- | Add a tower to a cell
addTower :: Tower -> Colour -> Cord -> Board -> Maybe Board 
addTower t c cords board =
    -- Are we on the board
    if inRange board cords then
        -- Check the square
        case board!cords of
            -- The square is empty
            (Nothing, _) -> Just $ recalculateInfluence $ setElem (Just (t, c), 0) cords board
            -- This square is occupied
            _            -> Nothing
    else
        Nothing

-- | Calculate the resources gained from a tile
calculateResourcesAt :: Board -> Cord -> (Resources, Colour)
calculateResourcesAt board c = case board!c of
    (Just (Keep, colour), _) -> (length available, colour)
        where
            available = do
                (Nothing, infl) <- (map (\cord -> board!cord) $
                                    filter (inRange board) $
                                    (map (addC c)) (circle resourceRadius))
                guard (infl `div` (colourSignum colour) >= 0)
    _                        -> (0, Red)

-- | Calculate the resources for (red, blue)
calculateResources :: Board -> (Resources, Resources)
calculateResources board = foldl fun (0, 0) raw
    where
        raw = map (calculateResourcesAt board) [(i, j) | i <- [1..(nrows board)], j <- [1..(ncols board)]]
        fun (x, y) (z, Red)  = (x+z, y)
        fun (x, y) (z, Blue) = (x, y+z)

-- | Clamp a Cord to the bounds of a board
clamp :: Board -> Cord -> Cord
clamp b (r, c) = (min (max 1 r) (nrows b), min (max 1 c) (ncols b))

-- | Move a cord to a new cord
move :: Board -> Cord -> Cord -> Cord
move board c delta_c = clamp board (addC c delta_c)
