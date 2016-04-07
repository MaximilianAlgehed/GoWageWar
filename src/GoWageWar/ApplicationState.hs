module GoWageWar.ApplicationState
    (ApplicationState(..),
     nextTurn,
     moveCursor,
     currentPlayerPlaceTower
    ) where

import qualified GoWageWar.GameState as G
import GoWageWar.Board
import Control.Monad.State.Lazy

-- | Model the current state of the application
data ApplicationState = ApplicationState {gameState :: G.GameState
                                         }

-- | Set everything up for the next turn
nextTurn :: ApplicationState -> ApplicationState
nextTurn st = st {gameState = execState G.endTurn (gameState st) 
                 }

-- | Move cursor wrapped
moveCursor :: ApplicationState -> Direction -> ApplicationState
moveCursor st d = st {gameState = execState 
                                    (G.moveCursor d)
                                    (gameState st)
                     }

-- | Place tower wrapped and specialised
currentPlayerPlaceTower :: ApplicationState -> Tower -> ApplicationState
currentPlayerPlaceTower st t = st {gameState = execState 
                                                   (get >>= (\s -> G.placeTower Nothing (G.turn s) t))
                                                   (gameState st) 
                                  }
