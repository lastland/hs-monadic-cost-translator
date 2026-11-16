-- Handwritten from Example.hs: forwarding reuses its argument; a computation
-- passed as an argument gets a suspension, even when the callee ignores it.
module Generated where

import qualified Memorist.Memorist as M

fM :: M.Th a -> M.MemoristM Int
fM _ = M.tick >> pure 42

gM, parenthesizedM, annotatedM :: M.Th a -> M.MemoristM Int
gM x = M.tick >> fM x
parenthesizedM x = M.tick >> fM x
annotatedM x = M.tick >> fM x

badM :: M.Th a -> M.MemoristM Int
badM _ = M.tick >> error "computed argument evaluated"

computedM :: M.Th a -> M.MemoristM Int
computedM x = do
    M.tick
    argument <- M.lazy (badM x)
    fM argument
