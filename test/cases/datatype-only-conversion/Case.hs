module Case (probes) where

import Control.Monad (replicateM, replicateM_)
import Generated
import qualified Example as Source
import qualified Memorist.Base.Thunk as MT
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
    , failure "conversion-outer-bottom" $ \_ -> do
        box <- M.reprM (undefined :: Source.Box) :: M.MemoristM BoxT
        case box of BoxT _ -> pure 0
    , demanded "caller/lazy-bottom-skipped" (\_ _ -> "Box") (\_ _ -> BoxD M.Skip)
        (\_ -> BoxT <$> M.lazy (error "unused caller computation"))
    , demanded "caller/undefined-thunk-skipped" (\_ _ -> "Box") (\_ _ -> BoxD M.Skip)
        (\_ -> pure (BoxT undefined))
    , demanded "caller/abstract-outer-only" (\_ box -> case M.abstract box of Source.Box _ -> "Box")
        (\_ _ -> BoxD M.Skip) (\_ -> pure (BoxT undefined))
    , failure "caller/lazy-bottom-demanded" $ \_ -> do
        BoxT field <- BoxT <$> M.lazy (error "demanded caller computation")
        M.force field
    , failure "caller/undefined-thunk-demanded" $ \_ -> do
        let BoxT field = BoxT undefined
        M.force field
    , failure "caller/abstract-bottom-demanded" $ \_ -> do
        field <- M.lazy (pure undefined)
        pure (abstractValue (BoxT field))
    , failure "caller/abstract-outer-bottom" $ \_ ->
        pure (abstractValue undefined)
    , failure "caller/full-demand-outer-bottom" $ \s ->
        pure (M.fullDemand (undefined :: BoxT) `seq` number s)
    , failure "caller/usage-outer-bottom" $ \s ->
        pure (M.usage (undefined :: BoxT) (BoxD M.Skip) `seq` number s)
    ] ++ concatMap paidProbes
        [("one", const 1), ("three", const 3), ("varied", \s -> width s + variant s + 1)] ++
    [ demanded "ancestor/full" (\_ -> abstractValue) (\_ -> M.fullDemand) ancestorBox
    , demanded "ancestor/skipped" (\_ _ -> "Box") (\_ _ -> BoxD M.Skip) ancestorBox
    , snapshot "ancestor/abstract-and-usage" (\s -> accounting (M.value (M.eval1 (ancestorBox s))))
    , scalar "ancestor/repeated-force" $ \s -> do
        BoxT field <- ancestorBox s
        sum <$> replicateM (repeats s) (M.force field)
    ]

-- These are actual public-constructor callers, not translations or reprM output.
-- Compare original accounting with Reference.hs; never normalize thunk ancestry.
paidBox :: Int -> Sample -> M.MemoristM BoxT
paidBox ticks s = BoxT <$> M.lazy (replicateM_ ticks M.tick >> pure (number s))

paidProbes :: (String, Sample -> Int) -> [Probe]
paidProbes (label, ticks) =
    [ demanded (prefix ++ "full") (\_ -> abstractValue) (\_ -> M.fullDemand) make
    , demanded (prefix ++ "skipped") (\_ _ -> "Box") (\_ _ -> BoxD M.Skip) make
    , snapshot (prefix ++ "abstract-and-usage") (\s -> accounting (M.value (M.eval1 (make s))))
    , scalar (prefix ++ "repeated-force") $ \s -> do
        BoxT field <- make s
        sum <$> replicateM (repeats s) (M.force field)
    ]
  where
    prefix = "caller/" ++ label ++ "/"
    make s = paidBox (ticks s) s

abstractValue :: BoxT -> Int
abstractValue box = case M.abstract box of Source.Box n -> n

accounting :: BoxT -> (Int, Integer, Integer)
accounting box =
    ( abstractValue box
    , MT.toInteger (M.sumcost (M.usage box (M.fullDemand box)))
    , MT.toInteger (M.sumcost (M.usage box (BoxD M.Skip))))

ancestorBox :: Sample -> M.MemoristM BoxT
ancestorBox s = do
    ancestor <- M.lazy (replicateM_ (width s + 1) M.tick >> pure (number s))
    field <- M.lazy $ do
        n <- M.force ancestor
        replicateM_ (variant s + 1) M.tick
        pure (n + other s)
    pure (BoxT field)
