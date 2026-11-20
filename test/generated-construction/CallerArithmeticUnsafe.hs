{-# LANGUAGE FlexibleContexts #-}
module CallerArithmeticUnsafe where

import qualified Memorist.Memorist as M

observer :: Num a => M.Th a -> a
observer = M.tval

comparison :: Eq a => a -> a -> Bool
comparison = (==)

ordering :: Integral a => a -> a -> Bool
ordering = (<)

render :: Show a => a -> String
render = show

otherNumeric :: Num a => a -> a
otherNumeric = abs

protected :: Num (M.Th a) => M.Th a -> M.Th a
protected x = x + x
