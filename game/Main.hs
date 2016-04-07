module Main where

import Prelude hiding (Left, Right)
import Control.Monad.State.Lazy
import Data.Maybe
import Control.Monad
import Data.Matrix
import qualified Data.Map as M
import GoWageWar.Board hiding (move)
import GoWageWar.Event.KeyBindings
import GoWageWar.Graphics
import qualified GoWageWar.GameState as GS
import GoWageWar.Board.Cord
import GoWageWar.ApplicationState
import Brick.Main
import Brick.AttrMap
import qualified Brick.Types as T
import Graphics.Vty as VTY

actionMap :: M.Map Action (ApplicationState -> T.EventM (T.Next ApplicationState))
actionMap = M.fromList [
                        (ExitGame,  halt),
                        (EndTurn,   continue . nextTurn),
                        (MoveUp,    continue . ((flip moveCursor) Up)),
                        (MoveDown,  continue . ((flip moveCursor) Down)),
                        (MoveLeft,  continue . ((flip moveCursor) Left)),
                        (MoveRight, continue . ((flip moveCursor) Right)),
                        (PlaceKeep, continue . ((flip currentPlayerPlaceTower) Keep)),
                        (PlaceWall, continue . ((flip currentPlayerPlaceTower) Wall)), 
                        (PlaceWatchtower, continue . ((flip currentPlayerPlaceTower) Watchtower))
                       ]

m_handleEvent :: ApplicationState -> Event -> T.EventM (T.Next ApplicationState)
m_handleEvent st (EvKey k []) = fromMaybe (continue st) next
    where
        next = do
                a <- M.lookup k keyBindings
                f <- M.lookup a actionMap
                return (f st)
m_handleEvent st _            = continue st 

theApp :: App ApplicationState Event
theApp = App {appDraw = (\st -> [draw (gameState st)]),
              appChooseCursor = const (const Nothing),
              appHandleEvent = m_handleEvent,
              appStartEvent = return,
              appLiftVtyEvent = id,
              appAttrMap = const $ attrMap defAttr attributes
             }

main = defaultMain theApp (ApplicationState (GS.initialState 19 19 [Red, Blue])) 
