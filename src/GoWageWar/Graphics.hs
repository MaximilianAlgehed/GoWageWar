{-# LANGUAGE OverloadedStrings #-}
module GoWageWar.Graphics
    (
        drawBoard,
        attributes
    ) where
import GoWageWar.Board
import Data.Matrix
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.AttrMap
import Brick.Types
import Brick.Util
import Graphics.Vty.Attributes

-- | The attributes for red and blue
attributes :: [(AttrName, Attr)]
attributes = [("red", red `on` black), ("blue", blue `on` black)]

-- | Create a widget representing the board state
drawBoard :: Board -> Widget
drawBoard board = withBorderStyle unicodeBold
                $ border
                $ foldl1 (<=>)
                $ map (foldl1 (<+>))
                $ toLists
                $ fmap toWidget board

padding :: Int
padding = 0

-- | Create a widget representing a cell
toWidget :: Cell -> Widget
toWidget (Nothing, x)
    | x == 0                       = str "." -- No influence from either player
    | signum x == colourSignum Red = padAll padding $ withAttr "red" $ str (show (abs x)) -- Influence from red
    | otherwise                    = padAll padding $ withAttr "blue" $ str (show (abs x)) -- Influence from blue
toWidget (Just (t, colour), _)
    | colour == Red                = padAll padding $ withAttr "red" $ str $ towerStr t -- Red tower
    | otherwise                    = padAll padding $ withAttr "blue" $ str $ towerStr t -- Blue tower
    where
        towerStr Wall       = "⋄"
        towerStr Watchtower = "∆"
        towerStr Keep       = "⍟"
