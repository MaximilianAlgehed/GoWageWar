{-# LANGUAGE OverloadedStrings #-}
module GoWageWar.Graphics
    (
        drawBoard,
        attributes,
        colourAttributeName
    ) where
import GoWageWar.Board
import GoWageWar.Board.Cord
import Data.Matrix
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.AttrMap
import Brick.Types
import Brick.Util
import Graphics.Vty.Attributes

colourAttributeName :: Colour -> AttrName
colourAttributeName Red  = "red"
colourAttributeName Blue = "blue"

-- | The attributes for red and blue
attributes :: [(AttrName, Attr)]
attributes = [("red", red `on` black), ("blue", blue `on` black)]

-- | Create a widget representing the board state
drawBoard :: Board -> Maybe Cord -> Widget
drawBoard board cord = withBorderStyle unicodeBold
                     $ border
                     $ foldl1 (<=>)
                     $ map (foldl1 (<+>))
                     $ toLists
                     $ cursorAt cord
                     $ fmap toWidget board

-- | Create a cursor widget at a position
cursorAt :: Maybe Cord -> Matrix Widget -> Matrix Widget
cursorAt Nothing matrix     = matrix
cursorAt (Just cord) matrix = setElem (str "-") cord matrix

-- | The padding around a cell
padding :: Int
padding = 0

-- | Create a widget representing a cell
toWidget :: Cell -> Widget
toWidget (Nothing, x)
    | x == 0                       = str "." -- No influence from either player
    | signum x == colourSignum Red = padAll padding $ withAttr "red" $ str (show (abs x)) -- Influence from red
    | otherwise                    = padAll padding $ withAttr "blue" $ str (show (abs x)) -- Influence from blue
toWidget (Just (t, colour), _)
    | colour == Red                = padAll padding
                                     $ withAttr (colourAttributeName colour)
                                     $ str
                                     $ towerStr t
    where
        towerStr Wall       = "⋄"
        towerStr Watchtower = "∆"
        towerStr Keep       = "⍟"
