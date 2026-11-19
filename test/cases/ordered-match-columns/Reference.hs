module Generated (chooseM, barrierM, scopedM, keepM) where

import qualified Memorist.Memorist as M

type Entry = M.MaybeT (M.PairT Int Int)

-- The match must finish before the pair is returned. Its fields are the
-- original middle thunk and the selected thunk, neither forced by the pair.
chooseM :: M.Th Int -> M.Th Entry -> M.Th Bool -> M.MemoristM (M.PairT Entry Int)
chooseM limit middle enabled = do
    M.tick
    let result selected = pure (M.PairT middle selected)
    entry <- M.force middle
    case entry of
        M.NothingT -> do
            yes <- M.force enabled
            if yes then M.lazy (pure 17) >>= result else result limit
        M.JustT pair -> do
            M.PairT key payload <- M.force pair
            k <- M.force key
            takeFirst <- if k == 0 then M.force enabled else pure False
            if takeFirst then result payload else do
                n <- M.force payload
                minimumValue <- M.force limit
                if n >= minimumValue then result key else result limit

barrierM :: M.Th Bool -> M.Th (M.MaybeT Int) -> M.Th (M.MaybeT Int) -> M.MemoristM Int
barrierM stop left right = do
    M.tick
    done <- M.force stop
    if done then pure 11 else do
        r <- M.force right
        case r of
            M.JustT x -> M.force x
            M.NothingT -> do
                l <- M.force left
                case l of
                    M.JustT y -> M.force y
                    M.NothingT -> pure (-1)

scopedM :: M.Th Int -> M.Th Int -> M.Th Bool -> M.MemoristM Int
scopedM original offered accept = do
    M.tick
    local <- M.lazy ((+ 1) <$> M.force offered)
    yes <- M.force accept
    M.force (if yes then local else original)

keepM :: M.Th Int -> M.Th (M.MaybeT Int) -> M.MemoristM Int
keepM original entry = do
    M.tick
    value <- M.force entry
    case value of
        M.NothingT -> M.force original
        M.JustT field -> do
            x <- M.force field
            if x == 0 then pure x else M.force original
