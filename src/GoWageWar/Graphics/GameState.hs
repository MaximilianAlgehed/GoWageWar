module GoWageWar.Graphics.GameState
    (
        module GoWageWar.Graphics.Board,
        draw,
    ) where

import Data.Maybe
import GoWageWar.Graphics.Attributes
import GoWageWar.Board
import GoWageWar.Graphics.Board
import GoWageWar.GameState
import Control.Monad
import Data.Matrix
import qualified Data.Map as M
import Brick.Widgets.Core
import qualified Brick.Types as T
import Graphics.Vty

-- | Draw the game state
draw :: GameState -> T.Widget
draw st = drawBoard (board st) (cursor st) <+> (padAll 1 $ drawResources (M.assocs (resources st)))

-- | Draw the resources each player has
drawResources :: [(Colour, Resources)] -> T.Widget
drawResources xs = foldl1 (<=>)
                 $ map
                    (\(c, r) -> withAttr (colourAttributeName c)
                    $ str $ (show c)++" : "++(show r)
                    )
                   xs 
