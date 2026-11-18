{-# LANGUAGE ScopedTypeVariables #-}
module Generated (viewsM, sameM, distinctM, capturedM, nestedM) where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

-- A source-specific finite carrier: the only polymorphic scalar inhabitants
-- produced by this source are bottoms. Unit tokens give native allocation;
-- each view retains its existing name and annotation and installs a bottom.
bottomField :: M.Th () -> M.Th a
bottomField (M.Th name _ annotation) =
    M.Th name (error "reference: demanded polymorphic bottom field") annotation

bottomPair :: M.Th (M.PairT () ()) -> M.Th (M.PairT a b)
bottomPair (M.Th name body annotation) = M.Th name
    (case body of M.PairT left right -> M.PairT (bottomField left) (bottomField right))
    annotation

pairCarrier :: M.Th Int -> M.MemoristM (M.PairT () ())
pairCarrier count = do
    M.tick
    n <- M.force count
    if n < 0 then pure (error "reference pair root bottom") else do
        left <- M.lazy (pure (error "reference left bottom"))
        right <- M.lazy (pure (error "reference right bottom"))
        pure (M.PairT left right)

viewsM :: M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int Bool) (M.PairT Bool Int))
viewsM _ count = do
    M.tick
    root <- M.lazy (M.tick >> pairCarrier count)
    pure (M.PairT (bottomPair root) (bottomPair root))

sameM :: M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int Int) (M.PairT Bool Bool))
sameM _ count = do
    M.tick
    root <- M.lazy (M.tick >> pairCarrier count)
    pure (M.PairT (bottomPair root) (bottomPair root))

distinctM :: M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int Bool) (M.PairT Bool Int))
distinctM _ count = do
    M.tick
    right <- M.lazy (M.tick >> pairCarrier count)
    left <- M.lazy (M.tick >> pairCarrier count)
    pure (M.PairT (bottomPair left) (bottomPair right))

capturedView :: M.Th (M.PairT () t) -> M.Th (M.PairT a t)
capturedView (M.Th name body annotation) = M.Th name
    (case body of M.PairT left fixed -> M.PairT (bottomField left) fixed)
    annotation

capturedM :: M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (M.PairT Int t) (M.PairT Bool t))
capturedM input count = do
    M.tick
    root <- M.lazy $ do
        M.tick -- gate ignores the captured input
        M.tick -- fixedProducer entry
        n <- M.force count
        if n < 0 then pure (error "reference fixed root bottom") else do
            left <- M.lazy (pure (error "reference fixed left bottom"))
            pure (M.PairT left input)
    pure (M.PairT (capturedView root) (capturedView root))

type Nested a b = M.PairT (M.MaybeT (L.ListT a)) (L.ListT (M.MaybeT (M.PairT b a)))

-- These views handle exactly this source's finite Just [] and singleton
-- [Just (bottom,bottom)]. They are deliberately not a container mapper.
emptyView :: M.Th (M.MaybeT (L.ListT ())) -> M.Th (M.MaybeT (L.ListT a))
emptyView (M.Th name body annotation) = M.Th name
    (case body of
        M.NothingT -> M.NothingT
        M.JustT (M.Th listName _ listAnnotation) ->
            M.JustT (M.Th listName L.NilT listAnnotation)) annotation

singletonView :: M.Th (L.ListT (M.MaybeT (M.PairT () ()))) ->
    M.Th (L.ListT (M.MaybeT (M.PairT b a)))
singletonView (M.Th name body annotation) = M.Th name
    (case body of
        L.NilT -> L.NilT
        L.ConsT (M.Th headName item headAnnotation) (M.Th tailName _ tailAnnotation) ->
            L.ConsT (M.Th headName
                (case item of
                    M.NothingT -> M.NothingT
                    M.JustT pair -> M.JustT (bottomPair pair)) headAnnotation)
                (M.Th tailName L.NilT tailAnnotation)) annotation

nestedView :: M.Th (Nested () ()) -> M.Th (Nested a b)
nestedView (M.Th name body annotation) = M.Th name
    (case body of M.PairT left right -> M.PairT (emptyView left) (singletonView right))
    annotation

nestedM :: M.Th t -> M.Th Int -> M.MemoristM
    (M.PairT (Nested Int Bool) (Nested Bool Int))
nestedM _ count = do
    M.tick
    root <- M.lazy $ do
        M.tick -- gate
        left <- M.lazy $ do
            M.tick -- leftProducer
            n <- M.force count
            if n < 0 then pure (error "reference nested left bottom") else do
                nil <- M.lazy (pure L.NilT)
                pure (M.JustT nil)
        right <- M.lazy $ do
            M.tick -- rightProducer
            n <- M.force count
            if n < 0 then pure (error "reference nested right bottom") else do
                item <- M.lazy $ do
                    pair <- M.lazy $ do
                        first <- M.lazy (pure (error "reference nested left leaf bottom"))
                        second <- M.lazy (pure (error "reference nested right leaf bottom"))
                        pure (M.PairT first second)
                    pure (M.JustT pair)
                nil <- M.lazy (pure L.NilT)
                pure (L.ConsT item nil)
        pure (M.PairT left right)
    pure (M.PairT (nestedView root) (nestedView root))
