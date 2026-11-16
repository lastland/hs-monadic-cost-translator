module Case (probes) where

import Generated
import qualified Example as S
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ failure "demanded-pattern-failure" (\s -> M.fromPure1 demandedM (number s))
    , failure "unmatched-case-failure" (\s -> M.fromPure1 unmatchedM (number s))
    , scalarSource "demanded-source-failure" (S.demanded . number) (\s -> M.fromPure1 demandedM (number s))
    , scalarSource "unmatched-source-failure" (S.unmatched . number) (\s -> M.fromPure1 unmatchedM (number s))
    , scalarSource "unused-bottom" (S.unused . number) (\s -> M.fromPure1 unusedM (number s))
    , scalarSource "match-first-finite" (\s -> S.matchFirst (number s) (numbers s))
        (\s -> M.fromPure2 matchFirstM (number s) (numbers s))
    , scalarSource "guard-first-finite" (\s -> S.guardFirst (number s) (numbers s))
        (\s -> M.fromPure2 guardFirstM (number s) (numbers s))
    , scalarSource "match-first-lazy-head" (\s -> S.matchFirst (other s) (lazyHead s))
        (\s -> M.fromPure2 matchFirstM (other s) (lazyHead s))
    , scalarSource "guard-first-lazy-head" (\s -> S.guardFirst (other s) (lazyHead s))
        (\s -> M.fromPure2 guardFirstM (other s) (lazyHead s))
    , failure "match-first-bottom-list" (\s -> M.fromPure2 matchFirstM (number s) bottomList)
    , scalarSource "guard-first-bottom-list" (\s -> S.guardFirst (number s) bottomList)
        (\s -> M.fromPure2 guardFirstM (number s) bottomList)
    ]
  where
    lazyHead s = error "undefined head" : numbers s
    bottomList = error "undefined list" :: [Int]
