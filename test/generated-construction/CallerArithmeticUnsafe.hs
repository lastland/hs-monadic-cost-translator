module CallerArithmeticUnsafe where

import qualified Memorist.Memorist as M

observer :: Num a => M.Th a -> a
observer = M.tval

comparison :: Eq a => a -> a -> Bool
comparison = (==)
