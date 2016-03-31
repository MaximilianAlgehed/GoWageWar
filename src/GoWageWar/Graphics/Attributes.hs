{-# LANGUAGE OverloadedStrings #-}
module GoWageWar.Graphics.Attributes
    (
        attributes,
        colourAttributeName
    ) where
import GoWageWar.Board
import Brick.AttrMap
import Brick.Util
import Graphics.Vty.Attributes

-- | Convert a colour to an attribute
colourAttributeName :: Colour -> AttrName
colourAttributeName Red  = "red"
colourAttributeName Blue = "blue"

-- | The attributes for red and blue
attributes :: [(AttrName, Attr)]
attributes = [("red", red `on` black), ("blue", blue `on` black)]
