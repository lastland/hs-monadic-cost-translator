module ViewCrossInput where

import StructuralViewControl (preserve)
import qualified Memorist.Memorist as M

swapped :: M.Th (M.PairT Int Int) -> M.Th (M.PairT Int Int)
swapped input = preserve (\value -> case value of
    M.PairT left right -> M.PairT (preserve id right) (preserve id left)) input

cross :: M.Th Int -> M.Th (M.MaybeT Int) -> M.Th (M.MaybeT Int)
cross other input = preserve (\value -> case value of
    M.NothingT -> M.NothingT
    M.JustT field -> M.JustT (preserve id other)) input
