module GoWageWar.GameState
    (
        GameState,
        draw,
        moveCursor
    ) where

import Prelude hiding (Left, Right)
import GoWageWar.Board
import GoWageWar.Graphics
import Control.Monad
import Data.Matrix
import Brick.Widgets.Core
import qualified Brick.Types as T
import Graphics.Vty

-- | Represents the state of the game
data GameState = GameState {
                    cursor    :: Maybe Cord,
                    board     :: Board,
                    turn      :: Colour,
                    resources :: [(Resources, Colour)]
                 }

-- | A direction to move the cursor
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

-- | Move the cursor in a given direction
moveCursor :: GameState -> Direction -> GameState
moveCursor st d = st {cursor = move (board st) c <$> (cursor st)}
    where
        c = directionToCord d

-- | Draw the game state
draw :: GameState -> T.Widget
draw st = drawBoard (board st) (cursor st) <+> drawResources (resources st)

-- | Draw the resources each player has
drawResources :: [(Resources, Colour)] -> T.Widget
drawResources xs = foldl1 (<=>)
                 $ map
                    (\(r, c) -> withAttr (colourAttributeName c)
                    $ str $ (show c)++" : "++(show r)
                    )
                   xs 
