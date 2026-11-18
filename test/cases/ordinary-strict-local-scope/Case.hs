module Case (probes) where

import qualified Example as Source
import qualified Generated as G
import qualified Memorist.Memorist as M
import ReferenceSupport (Sample(..), Probe, scalarSource)

generic, skipped, failed, dependent :: Num t => M.Th t -> M.MemoristM ()
generic = G.viewsM
skipped = G.skippedM
failed = G.failedM
dependent = G.dependentM

unit :: () -> Int
unit () = 0

call :: (M.Th Int -> M.MemoristM ()) -> Int -> M.MemoristM Int
call f n = do
    input <- M.lazy (pure n)
    unit <$> f input

probes :: [Probe]
probes = concat
    [[ scalarSource (label ++ "/finite") (unit . source . number) (call generated . number)
     , scalarSource (label ++ "/bottom") (const (unit (source undefined))) (const (call generated undefined))
     ] | (label, source, generated) <-
         [("where", Source.views, generic)
         ,("prior-false-guard", Source.skipped, skipped)
         ,("later-false-guard", Source.failed, failed)
         ,("forward-dependency", Source.dependent, dependent)]]
