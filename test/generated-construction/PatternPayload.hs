{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module PatternPayload where

import qualified Memorist.Memorist as M

pattern Observed :: a -> M.Th a
pattern Observed value <- (M.tval -> value)

leak :: M.Th Int -> M.MemoristM Int
leak (Observed number) = pure number
