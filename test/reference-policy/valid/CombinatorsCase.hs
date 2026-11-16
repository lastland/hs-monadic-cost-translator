module Case (probes) where

import Generated
import ReferenceSupport
import qualified Memorist.Memorist as M

table = [("first", identityM, True), ("second", identityM, False)]
masks = [("left", (True, False)), ("right", (False, True))]

probes :: [Probe]
probes = concatMap build table ++ concat
    [ [wrapper (label ++ "/" ++ suffix) make]
    | (label, make, _) <- table
    , (suffix, mask@(left, right)) <- masks
    , left || right
    ] ++ map (\(label, make, _) -> partial (label ++ "/partial")
        (\sample -> M.fromPure1 make (number sample))) table ++
    [scalar ("range/" ++ show index) (\sample -> M.fromPure1 identityM (number sample))
    | index <- [-1 .. 1 :: Int]]
  where
    build (label, make, chosen) = if chosen
        then [wrapper (label ++ "/chosen") make]
        else [wrapper (label ++ "/other") make]
    wrapper label make = scalar label (\sample -> M.fromPure1 make (number sample))
    partial label = scalar label
