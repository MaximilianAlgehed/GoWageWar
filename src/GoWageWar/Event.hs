module GoWageWar.Event
    (Event(..),
     NetworkEvent(..),
     module GoWageWar.Event.KeyBindings
    ) where

import qualified Graphics.Vty as VTY
import GoWageWar.Event.KeyBindings
import GoWageWar.Board

-- | Possible events
data Event = VtyEvent VTY.Event
           | ClockTick Double
           | NEvent NetworkEvent

-- | Possible events coming from the network player
data NetworkEvent = Resign
                  | Play Tower Cord
