module OrdinarySummaryControl where

import qualified Memorist.Memorist as M
import qualified Prelude as P
import Prelude

guarded :: Int -> ()
guarded number
    | number < 0 = ()
    | otherwise = number `seq` ()

qualifiedGuard :: Bool
qualifiedGuard = P.otherwise

unit :: () -> ()
unit () = ()

sequenced :: a -> ()
sequenced input = input `seq` ()

constructed :: M.MemoristM ()
constructed = pure ()
