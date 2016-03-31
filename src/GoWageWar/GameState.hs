module GoWageWar.GameState
    (
        GameState(..),
        moveCursor,
        placeTower,
        initialState
    ) where

import Prelude hiding (Left, Right)
import Data.Maybe
import GoWageWar.Board
import Control.Monad
import Data.Matrix
import qualified Data.Map as M

-- | Represents the state of the game
data GameState = GameState {
                    cursor    :: Maybe Cord,
                    board     :: Board,
                    turn      :: Colour,
                    resources :: M.Map Colour Resources
                 }

-- | Move the cursor in a given direction
moveCursor :: GameState -> Direction -> GameState
moveCursor st d = st {cursor = move (board st) c <$> (cursor st)}
    where
        c = directionToCord d

-- | Place a tower on the board
placeTower :: GameState -> Colour -> Tower -> GameState
placeTower st colour t = fromMaybe st (place st t)
    where
        place st t = do
                        cord <- cursor st
                        if ((resources st)M.!colour) < (price t) then
                            fail "Not enough resources"
                        else
                            case (board st) ! cord of
                                (Nothing, _) -> return st {
                                    board     = (recalculateInfluence (setElem (Just (t, colour), 0) cord (board st))),
                                    resources = M.adjust (\x -> x-(price t)) colour (resources st)
                                }
                                _            -> fail "Tile allready in use"

-- | Set up the initial state at the start of a game
initialState :: Int -> Int -> [Colour] -> GameState
initialState m n clrs = GameState {
                           cursor    = Just (1, 1),
                           board     = fromLists $ replicate m $ replicate n (Nothing, 0),
                           turn      = head clrs,
                           resources = M.fromList $ zip clrs $ repeat 10
                        }
