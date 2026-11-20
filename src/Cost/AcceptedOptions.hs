module Cost.AcceptedOptions
    ( AcceptedOptions
    , acceptedOptions
    , changedOptions
    ) where

import GHC
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Driver.Env (hscSetFlags)
import GHC.Driver.Session (gopt_unset)
import GHC.Iface.Recomp.Binary (putNameLiterally)
import GHC.Iface.Recomp.Flags
import GHC.Types.Basic (IntWithInf)
import GHC.Utils.Fingerprint (Fingerprint)

type SolverLimits = (IntWithInf, IntWithInf, Int, Int, Int)

data AcceptedOptions
    = AcceptedOptions
        Fingerprint
        Fingerprint
        Fingerprint
        [Int]
        SafeHaskellMode
        SolverLimits
    deriving (Eq)

acceptedOptions :: HscEnv -> ModSummary -> IO AcceptedOptions
acceptedOptions session summary = do
    let flags = gopt_unset (ms_hspp_opts summary) Opt_IgnoreInterfacePragmas
    compiler <-
        fingerprintDynFlags
            (hscSetFlags flags session)
            (ms_mod summary)
            putNameLiterally
    optimization <- fingerprintOptFlags flags putNameLiterally
    coverage <- fingerprintHpcFlags flags putNameLiterally
    pure
        ( AcceptedOptions
            compiler
            optimization
            coverage
            (map fromEnum (EnumSet.toList (generalFlags flags)))
            (safeHaskell flags)
            ( reductionDepth flags
            , solverIterations flags
            , givensFuel flags
            , wantedsFuel flags
            , qcsFuel flags
            )
        )

changedOptions :: AcceptedOptions -> AcceptedOptions -> [String]
changedOptions
    (AcceptedOptions c o h g s l)
    (AcceptedOptions c' o' h' g' s' l') =
        [ label
        | (label, changed) <-
            [ ("compiler flags", c /= c')
            , ("optimization flags", o /= o')
            , ("coverage flags", h /= h')
            , ("general flags", g /= g')
            , ("safe Haskell", s /= s')
            , ("typechecker limits", l /= l')
            ]
        , changed
        ]
