module Generated where

import qualified Data.Proxy as P
import qualified Memorist.Memorist as M

escape :: M.Th Int -> Int
escape t = case (P.Proxy :: P.Proxy Int) of
    P.Proxy -> M.tval t
