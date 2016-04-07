module GoWageWar.GameState
    (
        GameState(..),
        moveCursor,
        placeTower,
        initialState,
        endTurn
    ) where

import Prelude hiding (Left, Right)
import Data.Maybe
import qualified GoWageWar.Board as B
import GoWageWar.Board.Cord
import Control.Monad
import Data.Matrix
import qualified Data.Map as M
import Control.Monad.State.Lazy

-- | Represents the state of the game
data GameState = GameState {
                    cursor    :: Maybe Cord,
                    board     :: B.Board,
                    turn      :: B.Colour,
                    resources :: M.Map B.Colour B.Resources
                 }

-- | end the turn
endTurn :: State GameState ()
endTurn = do
            st <- get
            let board'     = B.endTurn (board st)
                (redResources, blueResources) = B.calculateResources board'
                resources' = M.alter (const (Just blueResources)) B.Blue $ M.alter (const (Just redResources)) B.Red $ resources st
            put $ st {board = board',
                      turn  = B.nextColour (turn st),
                      resources = resources'
                     }

-- | Move the cursor in a given direction
moveCursor :: Direction -> State GameState ()
moveCursor d = do
                    st <- get
                    put $ st {cursor = B.move (board st) c <$> (cursor st)}
    where
        c = directionToCord d

-- | Place a tower on the board
placeTower :: Maybe Cord -> B.Colour -> B.Tower -> State GameState ()
placeTower Nothing colour t = do
                                    st <- get
                                    put $ fromMaybe st (place (cursor st) st t colour)
placeTower cord colour t    = do
                                    st <- get
                                    put $ fromMaybe st (place cord st t colour)

-- | Do the placing
place cordm st t colour = do
                    cord <- cordm
                    if ((resources st)M.!colour) < (B.price t) then
                        fail "Not enough resources"
                    else
                        case (board st) ! cord of
                            (Nothing, _) -> return st {
                                board     = (B.recalculateInfluence (setElem (Just (t, colour), 0) cord (board st))),
                                resources = M.adjust (\x -> x-(B.price t)) colour (resources st)
                            }
                            _            -> fail "Tile allready in use"

-- | Set up the initial state at the start of a game
initialState :: Int -> Int -> [B.Colour] -> GameState
initialState m n clrs = GameState {
                           cursor    = Just (1, 1),
                           board     = fromLists $ replicate m $ replicate n (Nothing, 0),
                           turn      = head clrs,
                           resources = M.fromList $ zip clrs $ repeat 10
                        }
