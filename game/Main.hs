module Main where

import Prelude hiding (Left, Right)
import Control.Monad
import Data.Matrix
import GoWageWar.Board hiding (move)
import GoWageWar.Graphics
import GoWageWar.Board.Cord
import GoWageWar.GameState
import Brick.Main
import Brick.AttrMap
import qualified Brick.Types as T
import Graphics.Vty

m_handleEvent :: GameState -> Event -> T.EventM (T.Next GameState)
m_handleEvent st ev =
    case ev of
        EvKey KEsc []        -> halt st
        EvKey (KChar 'j') [] -> continue $ moveCursor st Down
        EvKey (KChar 'k') [] -> continue $ moveCursor st Up
        EvKey (KChar 'h') [] -> continue $ moveCursor st Left
        EvKey (KChar 'l') [] -> continue $ moveCursor st Right
        EvKey (KChar '1') [] -> continue $ placeTower st Red Watchtower
        EvKey (KChar '2') [] -> continue $ placeTower st Red Keep
        EvKey (KChar '3') [] -> continue $ placeTower st Red Wall
        _                    -> continue st

theApp :: App GameState Event
theApp = App {appDraw = (\st -> [draw st]),
              appChooseCursor = const (const Nothing),
              appHandleEvent = m_handleEvent,
              appStartEvent = return,
              appLiftVtyEvent = id,
              appAttrMap = const $ attrMap defAttr attributes
             }

main = defaultMain theApp (initialState 19 19 [Red, Blue]) 
