{-# LANGUAGE OverloadedStrings #-}
module GoWageWare.Graphics
    (
        drawBoard
    ) where
import GoWageWar.Board
import Data.Matrix
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Types

-- | Create a widget representing the board state
drawBoard :: Board -> Widget
drawBoard board = withBorderStyle unicodeBold
                $ foldl1 (<=>)
                $ map (foldl1 (<+>))
                $ toLists
                $ fmap toWidget board

-- | Create a widget representing a cell
toWidget :: Cell -> Widget
toWidget (Nothing, x)
    | x == 0                       = str (show x)
    | signum x == colourSignum Red = withAttr "red" $ str (show x) 
    | otherwise                    = withAttr "blue" $ str (show x)
toWidget (Just (t, colour), _)
    | colour == Red                = withAttr "red" $ str $ towerStr t
    | otherwise                    = withAttr "blue" $ str $ towerStr t
    where
        towerStr Wall       = "⋄"
        towerStr Watchtower = "∆"
        towerStr Keep       = "⍟"
