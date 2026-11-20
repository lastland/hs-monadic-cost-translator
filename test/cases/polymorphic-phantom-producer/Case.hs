module Case (probes) where

import qualified Example as S
import qualified Generated as G
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport (Probe, Sample(..), distinct, failure, scalarSource, snapshot)

type Views a = M.PairT (G.ParcelT L.ListT a) (G.ParcelT M.MaybeT a)

-- An ordinary caller can forward any tag, with no dictionary.
ordinary :: M.Th a -> M.Th Bool -> M.Th Int -> M.Th Int -> M.MemoristM (Views a)
ordinary = G.viewsM

paid :: a -> M.MemoristM (M.Th a)
paid value = M.lazy (M.tick >> pure value)

call :: Bool -> Int -> Int -> M.MemoristM (Views Char)
call present left right = do
    tag <- M.lazy (error "unused tag")
    flag <- paid present
    x <- paid left
    y <- paid right
    ordinary tag flag x y

rootTag :: G.ParcelT f a -> Int
rootTag (G.MissingT _) = 0
rootTag (G.PackedT _ _ _) = 1

readField :: Bool -> M.Th (G.ParcelT f a) -> M.MemoristM Int
readField first root = do
    parcel <- M.force root
    case parcel of
        G.MissingT _ -> pure 0
        G.PackedT _ left right -> M.force (if first then left else right)

bothFirst :: Views a -> M.MemoristM Int
bothFirst (M.PairT a b) = (+) <$> readField True a <*> readField True b

sourceField :: Bool -> S.Parcel f a -> Int
sourceField _ (S.Missing _) = 0
sourceField first (S.Packed _ left right) = if first then left else right

project :: Bool -> Bool -> Sample -> Int
project present same s =
    let (a,b) = S.views 'x' present (number s) (other s)
    in sourceField True a + sourceField same b

identities :: M.Th (G.ParcelT f a) -> M.MemoristM [M.ThName]
identities root = do
    body <- M.force root
    pure (M.tname root : case body of
        G.MissingT tag -> [M.tname tag]
        G.PackedT tag x y -> [M.tname tag, M.tname x, M.tname y])

probes :: [Probe]
probes =
    [ scalarSource "parcel/skip-producer" (const 0)
        (const (call (error "unused producer") 0 0 >> pure 0))
    , scalarSource "parcel/demand-tags-only" (\s -> if leftDemand s then 2 else 0) $ \s -> do
        M.PairT a b <- call (leftDemand s) (error "unused left") (error "unused right")
        (+) <$> (rootTag <$> M.force a) <*> (rootTag <$> M.force b)
    , scalarSource "parcel/one-paid-invocation-both-views" (project True True)
        (\s -> call True (number s) (error "unused right") >>= bothFirst)
    , scalarSource "parcel/independent-fields" (\s -> project (leftDemand s) False s) $ \s -> do
        M.PairT a b <- call (leftDemand s) (number s) (other s)
        (+) <$> readField True a <*> readField False b
    , scalarSource "parcel/two-fresh-invocations" (\s -> 2 * project True True s) $ \s -> do
        tag <- M.lazy (error "unused tag")
        flag <- paid True
        x <- paid (number s)
        y <- paid (error "unused right")
        let fresh = ordinary tag flag x y >>= bothFirst
        (+) <$> fresh <*> fresh
    , failure "parcel/demanded-bottom" (const (call True (error "demanded left") 0 >>= bothFirst))
    , scalarSource "parcel/inferred-helper-at-two-types"
        (\s -> let ((a,_),(_,b)) = S.inferred (number s) (leftDemand s)
               in sourceField True a + sourceField False b) $ \s -> do
        n <- paid (number s)
        b <- paid (leftDemand s)
        M.PairT left right <- G.inferredM n b
        M.PairT x _ <- M.force left
        M.PairT _ y <- M.force right
        (+) <$> readField True x <*> readField False y
    , snapshot "parcel/alias-partition" $ \s -> M.value $ M.eval1 $ do
        tag <- M.lazy (pure 'x')
        flag <- paid (leftDemand s)
        x <- paid (number s)
        y <- paid (other s)
        let names = do
                M.PairT a b <- ordinary tag flag x y
                (,) <$> identities a <*> identities b
        (a,b) <- names
        (c,d) <- names
        pure (a == b && c == d && distinct a && distinct c &&
              [shared | shared <- a, shared `elem` c] == [M.tname tag])
    ]
