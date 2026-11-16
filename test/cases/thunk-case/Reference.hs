-- Handwritten from thunk-matching/Example.hs. An irrefutable variable match
-- binds the existing suspension, rather than forcing or rewrapping it.
module Generated where

import qualified Memorist.Memorist as M

caseAliasM, caseParenthesizedM, caseAnnotatedM, caseAsM
    :: M.Th a -> M.MemoristM (M.PairT a a)
caseAliasM x = M.tick >> pure (M.PairT x x)
caseParenthesizedM x = M.tick >> pure (M.PairT x x)
caseAnnotatedM x = M.tick >> pure (M.PairT x x)
caseAsM x = M.tick >> pure (M.PairT x x)
