module HiddenMappedSupport where
    import qualified Memorist.Memorist as M
    payload :: M.Th Int -> Int
{-# LINE 50 "SharedSupport.hs" #-}
    payload x = M.tval x + (0 :: Int)
