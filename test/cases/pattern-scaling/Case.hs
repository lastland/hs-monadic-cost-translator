module Case (probes) where

import Generated (chooseM)
import qualified Example as S
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "sample-pair" (\s -> S.choose (number s) (other s))
        (\s -> M.fromPure2 chooseM (number s) (other s))
    , scalarSource "diagonal" (\s -> S.choose (diagonal s) (diagonal s))
        (\s -> M.fromPure2 chooseM (diagonal s) (diagonal s))
    , scalarSource "off-diagonal" (\s -> S.choose (diagonal s) ((diagonal s + 1) `mod` 10))
        (\s -> M.fromPure2 chooseM (diagonal s) ((diagonal s + 1) `mod` 10))
    , scalarSource "lazy-second-column" (\s -> S.choose (10 + abs (number s)) (error "unused second column" :: Int)) $ \s -> do
        a <- M.lazy (pure (10 + abs (number s)))
        b <- M.lazy (pure (error "second column must remain lazy" :: Int))
        chooseM a b
    , scalarSource "conditional-second-column" (\s -> S.choose (variant s `mod` 12 - 1) (error "demanded second column" :: Int)) $ \s -> do
        a <- M.lazy (pure (variant s `mod` 12 - 1))
        b <- M.lazy (pure (error "second column demanded" :: Int))
        chooseM a b
    ] ++
    -- Preserve the old exhaustive finite matrix as fixed catalog entries.
    [ scalarSource ("equation-" ++ show a ++ "-" ++ show b)
        (\_ -> S.choose a b) (\_ -> M.fromPure2 chooseM a b)
    | a <- [-1 .. 11 :: Int], b <- [-1 .. 11 :: Int]
    ]
  where
    diagonal s = number s `mod` 10
