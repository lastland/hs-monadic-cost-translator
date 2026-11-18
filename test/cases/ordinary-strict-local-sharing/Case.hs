module Case (probes) where

import Control.Monad (replicateM)
import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalar, scalarSource)

generic :: M.Th t -> M.MemoristM ()
generic = G.viewsM

genericAliases :: M.Th t -> M.MemoristM (M.PairT (M.PairT t Bool) (M.PairT t Bool))
genericAliases = G.aliasesM

unit :: () -> Int
unit () = 0

headPair :: ((a,b),(a,b)) -> Int
headPair ((_,_),(_,_)) = 0

aliases :: Int -> M.MemoristM (M.PairT (M.PairT Int Bool) (M.PairT Int Bool))
aliases n = M.lazy (pure n) >>= genericAliases

probes :: [Probe]
probes =
    [ scalarSource "unit/shallow-bottom-fields" (const (unit (Source.views (undefined :: Int)))) $ \_ -> do
        input <- M.lazy (M.tick >> pure (undefined :: Int))
        unit <$> generic input
    , scalarSource "aliases/heads-only" (const (headPair (Source.aliases (undefined :: Int)))) $ \_ -> do
        M.PairT left right <- aliases undefined
        a <- M.force left
        b <- M.force right
        a `seq` b `seq` pure 0
    , scalar "aliases/same-thunk" $ \s -> do
        M.PairT left right <- aliases (number s)
        pure (if M.tname left == M.tname right then 0 else 1)
    , scalar "aliases/fresh-call-thunks" $ \s -> do
        input <- M.lazy (pure (number s))
        M.PairT first _ <- genericAliases input
        M.PairT second _ <- genericAliases input
        pure (if M.tname first /= M.tname second then 0 else 1)
    , scalar "aliases/cached-call-thunk" $ \s -> do
        input <- M.lazy (pure (number s))
        cached <- M.lazy (genericAliases input)
        M.PairT first _ <- M.force cached
        M.PairT second _ <- M.force cached
        pure (if M.tname first == M.tname second then 0 else 1)
    , scalarSource "aliases/repeated-left-payload" (\s -> repeats s * case Source.aliases (number s) of ((x,_),_) -> x) $ \s -> do
        input <- M.lazy (M.tick >> pure (number s))
        M.PairT left _ <- genericAliases input
        sum <$> replicateM (repeats s) (do M.PairT payload _ <- M.force left; M.force payload)
    , scalarSource "aliases/demanded-left-bottom" (const (case Source.aliases (undefined :: Int) of ((x,_),_) -> x)) $ \_ -> do
        M.PairT left _ <- aliases undefined
        M.PairT payload _ <- M.force left
        M.force payload
    , scalarSource "aliases/demanded-right-bottom" (const (case Source.aliases (5 :: Int) of ((_,bad),_) -> if bad then 1 else 0)) $ \_ -> do
        M.PairT left _ <- aliases 5
        M.PairT _ payload <- M.force left
        bad <- M.force payload
        pure (if bad then 1 else 0)
    ]
