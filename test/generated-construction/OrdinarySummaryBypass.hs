module OrdinarySummaryBypass where

import qualified Memorist.Memorist as M

guarded :: M.Th Int -> Int
guarded input | otherwise = M.tval input

unit :: M.Annotated () -> ()
unit = M.value

sequenced :: M.Th Int -> ()
sequenced input = M.tval input `seq` ()

wrongCarrier :: M.Annotated ()
wrongCarrier = pure ()
