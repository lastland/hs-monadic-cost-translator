{-# LANGUAGE RankNTypes #-}
module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

producerM :: M.Th Int -> M.MemoristM (L.ListT element)
producerM input = do
    M.tick
    count <- M.force input
    if count <= 0 then pure L.NilT else do
        element <- M.lazy (pure (error "producer: demanded bottom element"))
        next <- M.lazy (pure (count - 1))
        rest <- M.lazy (producerM next)
        pure (L.ConsT element rest)

spineM :: M.Th (L.ListT element) -> M.MemoristM Int
spineM input = do
    M.tick
    xs <- M.force input
    case xs of
        L.NilT -> pure 0
        L.ConsT _ rest -> do
            count <- spineM rest
            pure (count + 1)

ignoreM :: M.Th unused -> M.MemoristM Int
ignoreM _ = M.tick >> pure 1

scopedM :: M.Th outer -> M.Th Int -> M.MemoristM Int
scopedM input count = do
    M.tick
    anchor <- M.lazy (pure ())
    let shared :: forall element. M.Th (L.ListT element)
        shared = M.value (M.eval (M.lazy (producerM count)) (M.tname anchor))
    ignored <- ignoreM input
    ints <- spineM (shared :: M.Th (L.ListT Int))
    bools <- spineM (shared :: M.Th (L.ListT Bool))
    pure (ignored + ints + bools)

viewsM :: M.Th outer -> M.Th Int -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
viewsM _ count = do
    M.tick
    anchor <- M.lazy (pure ())
    let shared :: forall element. M.Th (L.ListT element)
        shared = M.value (M.eval (M.lazy (producerM count)) (M.tname anchor))
    pure (M.PairT shared shared)

inferredM :: M.Th outer -> M.MemoristM (M.PairT (L.ListT Int) (L.ListT Bool))
inferredM input = do
    M.tick
    anchor <- M.lazy (pure ())
    let shared :: forall element. M.Th (L.ListT element)
        shared = M.value (M.eval (M.lazy (ignoreM input >> pure L.NilT)) (M.tname anchor))
    pure (M.PairT shared shared)

inferredSiblingsM :: M.Th outer -> M.Th sibling -> M.MemoristM (M.PairT (L.ListT outer) (M.PairT (L.ListT Int) (L.ListT Bool)))
inferredSiblingsM input sibling = do
    M.tick
    kept <- M.lazy $ do
        rest <- M.lazy (pure L.NilT)
        pure (L.ConsT input rest)
    firstAnchor <- M.lazy (pure ())
    secondAnchor <- M.lazy (pure ())
    let first :: forall element. M.Th (L.ListT element)
        first = M.value (M.eval (M.lazy (ignoreM input >> pure L.NilT)) (M.tname firstAnchor))
        second :: forall element. M.Th (L.ListT element)
        second = M.value (M.eval (M.lazy (ignoreM sibling >> M.force first)) (M.tname secondAnchor))
    rest <- M.lazy (pure (M.PairT first second))
    pure (M.PairT kept rest)
