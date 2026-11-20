module Generated where

import qualified Data.Proxy as P
import qualified Memorist.Memorist as M

witness :: P.Proxy Int
witness = P.Proxy

forward :: P.Proxy a -> P.Proxy a
forward x = x

phantomThunk :: P.Proxy (M.Th Int)
phantomThunk = P.Proxy
