module BindCarrier where

import qualified Memorist.Memorist as M

alternative :: M.Annotated Int -> M.Annotated Int
alternative action = action >>= \_ -> action
