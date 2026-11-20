{-# LANGUAGE GADTs, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
module IndependentTypedLocals where

import LexicalFamilyEvidence (Opaque)
import qualified Memorist.Memorist as M
import qualified Test.QuickCheck as Q

unknown :: M.Th (Opaque ()) -> Q.Gen (Opaque ())
unknown t = let observed = M.tval t in Q.elements [observed]

instantiated :: M.Th Int -> Q.Property
instantiated t =
    let pick :: forall a. Bounded a => a
        pick =
            M.tval t `seq` maxBound
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

data Shape a where
    Flag :: Shape Bool
    Observe :: Shape (Int -> Bool)

refined :: Shape a -> M.Th a -> Q.Property
refined shape t =
    let observed = M.tval t
    in case shape of
        Flag -> Q.property observed
        Observe -> Q.property (observed 3)

refinedPattern :: forall a. Shape a -> M.Th a -> Q.Property
refinedPattern shape t =
    let (observed :: a, _) = (M.tval t, ())
    in case shape of
        Flag -> Q.property observed
        Observe -> Q.property (observed 3)
