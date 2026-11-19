{-# LANGUAGE FlexibleContexts #-}
module MethodControl where

import qualified Memorist.Memorist as M

ret :: M.MemoristM Int
ret = pure 17

retSpelling :: M.MemoristM Int
retSpelling = return 17

binding :: M.MemoristM Int
binding = ret >>= \n -> M.tick >> return n

applicative :: M.MemoristM Int
applicative = (+) <$> ret <*> retSpelling

generic :: M.Memorist s m => M.Th Int -> m Int
generic thunk = M.force thunk >>= pure

representation :: M.MemoristM (M.MaybeT Int)
representation = M.reprM (Just (17 :: Int))
