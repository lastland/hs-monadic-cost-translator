module ApplicativeCarrier where

import qualified Memorist.Memorist as M

alternative :: M.Annotated (Int -> Int) -> M.Annotated Int -> M.Annotated Int
alternative = (<*>)
