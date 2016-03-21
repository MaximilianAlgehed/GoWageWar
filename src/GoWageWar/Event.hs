module GoWageWar.Event
    (Event(..),
     NetworkEvent(..)
    ) where

import qualified Graphics.Vty as VTY
import GoWageWar.Board.Cord
import GoWageWar.Board

data Event = VtyEvent VTY.Event
           | ClockTick Double
           | NEvent NetworkEvent

data NetworkEvent = Resign
                  | Play Tower Cord
