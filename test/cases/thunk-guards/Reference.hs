-- Handwritten from thunk-matching/Example.hs. These pattern guards cannot
-- fail and bind their subject's existing suspension without demanding it.
module Generated where

import qualified Memorist.Memorist as M

guardAliasM, guardParenthesizedM, guardAnnotatedM, guardAsM
    :: M.Th a -> M.MemoristM (M.PairT a a)
guardAliasM x = M.tick >> pure (M.PairT x x)
guardParenthesizedM x = M.tick >> pure (M.PairT x x)
guardAnnotatedM x = M.tick >> pure (M.PairT x x)
guardAsM x = M.tick >> pure (M.PairT x x)
