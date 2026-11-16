module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ demanded "conversion" (\_ (BoxT field) -> M.tval field) (\_ -> M.fullDemand)
        (\s -> M.reprM (Source.Box (number s)) :: M.MemoristM BoxT)
    , demanded "skipped-field" (\_ (BoxT _) -> "Box") (\_ _ -> BoxD M.Skip)
        (\_ -> M.reprM (Source.Box undefined) :: M.MemoristM BoxT)
    , failure "demanded-field" $ \_ -> do
        box <- M.reprM (Source.Box undefined) :: M.MemoristM BoxT
        case box of BoxT field -> M.force field
    ]
