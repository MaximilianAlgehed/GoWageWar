module Main where
import Data.Matrix
import GoWageWar.Board
import GoWageWar.Graphics
import Brick.Main
import Brick.AttrMap
import Graphics.Vty

theApp :: App Board Event
theApp = App {appDraw = (:[]) . drawBoard,
              appChooseCursor = const (const Nothing),
              appHandleEvent = (\b _ -> halt b),
              appStartEvent = return,
              appLiftVtyEvent = id,
              appAttrMap = const $ attrMap defAttr attributes
             }

board = endTurn $ fromLists [
    [(Nothing, 0), (Nothing, 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Nothing, 0), (Nothing, 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Nothing, 0), (Just (Keep, Blue), 0),      (Nothing, 0),  (Nothing, 0),          (Just (Watchtower, Red), 0), (Nothing, 0)],
    [(Nothing, 0), (Just (Watchtower, Blue), 0),(Nothing, 0),  (Just (Keep, Red), 0), (Nothing, 0),                (Nothing, 0)],
    [(Nothing, 0), (Just (Wall, Blue), 0),      (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Just (Wall, Red), 0), (Nothing, 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Nothing, 0)],
    [(Just (Watchtower, Red), 0), (Just (Wall, Red), 0),                (Nothing, 0),  (Nothing, 0),          (Nothing, 0),                (Just (Wall, Red), 0)]
   ]

main = defaultMain theApp board
