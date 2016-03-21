module Main where
import Control.Monad
import Data.Matrix
import GoWageWar.Board
import GoWageWar.Graphics
import GoWageWar.Board.Cord
import Brick.Main
import Brick.AttrMap
import Brick.Types
import Graphics.Vty

data TheState = TheState {cursor :: Maybe Cord, board :: Board}

clamp :: Board -> Cord -> Cord
clamp b (r, c) = (min (max 1 r) (nrows b), min (max 1 c) (ncols b))

move :: Cord -> TheState -> TheState
move c st = TheState (move' c (cursor st)) (board st)
    where
        move' co mc = clamp (board st) <$> addC co <$> mc

m_handleEvent :: TheState -> Event -> EventM (Next TheState)
m_handleEvent st ev =
    case ev of
        EvKey KEsc []        -> halt st
        EvKey (KChar 'j') [] -> continue $ move (1, 0) st
        EvKey (KChar 'k') [] -> continue $ move (-1, 0) st
        EvKey (KChar 'h') [] -> continue $ move (0, -1) st
        EvKey (KChar 'l') [] -> continue $ move (0, 1) st
        _             -> continue st

theApp :: App TheState Event
theApp = App {appDraw = (\st -> [drawBoard (board st) (cursor st)]),
              appChooseCursor = const (const Nothing),
              appHandleEvent = m_handleEvent,
              appStartEvent = return,
              appLiftVtyEvent = id,
              appAttrMap = const $ attrMap defAttr attributes
             }

bboard = endTurn $ fromLists [
    [(Nothing, 0), (Nothing, 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Nothing, 0), (Nothing, 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Nothing, 0), (Just (Keep, Blue), 0),      (Nothing, 0),  (Nothing, 0),          (Just (Watchtower, Red), 0), (Nothing, 0)],
    [(Nothing, 0), (Just (Watchtower, Blue), 0),(Nothing, 0),  (Just (Keep, Red), 0), (Nothing, 0),                (Nothing, 0)],
    [(Nothing, 0), (Just (Wall, Blue), 0),      (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Just (Wall, Red), 0), (Nothing, 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Just (Watchtower, Red), 0), (Just (Wall, Red), 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Just (Wall, Red), 0)]
   ]

main = defaultMain theApp (TheState (Just (1, 1)) bboard)
