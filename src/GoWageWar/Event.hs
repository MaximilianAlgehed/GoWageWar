module GoWageWar.Event
    (Event(..),
     NetworkEvent(..)
    ) where

import qualified Graphics.Vty as VTY
import GoWageWar.Board.Cord

data Event = VtyEvent VTY.Event
           | ClockTick Double
           | NEvent NetworkEvent

data NetworkEvent = Resign
                  | Play Cord
