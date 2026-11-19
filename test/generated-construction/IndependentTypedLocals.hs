{-# LANGUAGE TypeApplications, TypeFamilies #-}
module IndependentTypedLocals where

import LexicalFamilyEvidence (Opaque)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

unknown :: M.Th (Opaque ()) -> Q.Gen (Opaque ())
unknown t = let observed = M.tval t in Q.elements [observed]

instantiated :: M.Th Int -> Q.Property
instantiated t =
    let pick :: forall a. Bounded a => a
        pick = M.tval t `seq` maxBound
    in Q.property (pick @Ordering `seq` pick @Bool)

run :: M.MemoristM Bool -> M.MemoristM Bool
run m = m >>= pure

handed :: M.Th Int -> Q.Property
handed t =
    let act :: M.MemoristM Bool
        act = pure () >>= \() -> M.tval t `seq` pure True
    in Q.property (M.value (M.eval1 (run act)))

lawful :: Q.Property
lawful = let act = pure True :: M.MemoristM Bool in Q.property (M.value (M.eval1 (run act)))
