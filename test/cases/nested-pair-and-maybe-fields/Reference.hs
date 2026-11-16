-- Handwritten from the source equations; ticks mark source function entries.
module Generated where

import qualified Memorist.Memorist as M

data Nested a
    = End (M.Th a)
    | More (M.Th (Nested (M.PairT a a)))

readNestedM :: M.Th (Nested a) -> M.MemoristM a
readNestedM nested = do
    M.tick
    outer <- M.force nested
    case outer of
        More child -> do
            inner <- M.force child
            case inner of
                End pair -> do
                    fields <- M.force pair
                    case fields of
                        M.PairT first _ -> M.force first
                _ -> error "readNested: expected End"
        _ -> error "readNested: expected More"

readMaybeM :: M.Th (M.MaybeT a) -> M.MemoristM a
readMaybeM optional = do
    M.tick
    value <- M.force optional
    case value of
        M.JustT field -> M.force field
        M.NothingT -> error "readMaybe: expected Just"

nestedM :: M.Th Int -> M.MemoristM Int
nestedM input = do
    M.tick
    optional <- M.lazy $ do
        result <- M.lazy $ do
            nested <- M.lazy $ do
                child <- M.lazy $ do
                    pair <- M.lazy $ do
                        ignored <- M.lazy (pure undefined)
                        pure (M.PairT input ignored)
                    pure (End pair)
                pure (More child)
            readNestedM nested
        pure (M.JustT result)
    readMaybeM optional
