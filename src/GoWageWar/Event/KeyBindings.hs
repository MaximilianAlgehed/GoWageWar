module GoWageWar.Event.KeyBindings
    (Action,
     keyBindings
    ) where

import Graphics.Vty
import Data.Map as M

-- | An action the local user can take
data Action = EndTurn
            | PlaceWatchtower
            | PlaceKeep
            | PlaceWall
            | MoveUp
            | MoveDown
            | MoveLeft
            | MoveRight
            | ExitGame deriving(Ord, Eq)

-- | Key bindings for the local user
keyBindings :: Map Action Key
keyBindings = M.fromList
              [
                (EndTurn,         KChar ' '),
                (PlaceWatchtower, KChar '1'),
                (PlaceKeep,       KChar '2'),
                (PlaceWall,       KChar '3'),
                (MoveUp,          KChar 'j'),
                (MoveDown,        KChar 'k'),
                (MoveLeft,        KChar 'h'),
                (MoveRight,       KChar 'l'),
                (ExitGame,        KEsc)
              ]
