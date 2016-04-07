module GoWageWar.Event.KeyBindings
    (Action(..),
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
keyBindings :: Map Key Action
keyBindings = M.fromList
              [
                (KChar ' ', EndTurn),
                (KChar '1', PlaceWatchtower),
                (KChar '2', PlaceKeep),
                (KChar '3', PlaceWall),
                (KChar 'j', MoveDown),
                (KChar 'k', MoveUp),
                (KChar 'h', MoveLeft),
                (KChar 'l', MoveRight),
                (KEsc, ExitGame)
              ]
