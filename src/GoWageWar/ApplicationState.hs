module GoWageWar.ApplicationState
    (ApplicationState,
     nextTurn
    ) where

import GoWageWar.GameState

-- | Model the current state of the application
data ApplicationState = ApplicationState {gameState :: GameState
                                         }

-- | Set everything up for the next turn
nextTurn :: ApplicationState -> ApplicationState
nextTurn st = st {gameState = endTurn (gameState st)
                 }
