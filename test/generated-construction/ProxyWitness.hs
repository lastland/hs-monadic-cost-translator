module Generated where

import qualified Data.Proxy as P

witness :: P.Proxy Int
witness = P.Proxy

forward :: P.Proxy a -> P.Proxy a
forward x = x
