module Cost.Translate.Sharing.Paths where

import Cost.Translate.Kinds (visibleArguments)
import Cost.Translate.Output qualified as O
    ( Ty
    , promoted
    , promotedList
    )
import Cost.Translate.Sharing.Specialization
    ( constrainedVariables
    )
import Data.List (minimumBy)
import Data.Ord (comparing)
import GHC (Type)
import GHC.Core.Type
    ( getTyVar_maybe
    , splitAppTy_maybe
    , splitForAllTyVars
    , splitFunTy_maybe
    , splitFunTys
    , splitTyConApp_maybe
    )
import GHC.Types.Name (getOccString)
import GHC.Types.Var (tyVarKind)

sharingPaths :: Type -> Either String (Maybe O.Ty)
sharingPaths ty = case constrainedVariables ty of
    [] -> Right Nothing
    vars -> Just . O.promotedList <$> mapM path vars
    where
        result = snd (splitFunTys (snd (splitForAllTyVars ty)))
        path v = case paths v result of
            [] ->
                Left
                    ( "cannot locate constrained type parameter "
                        ++ getOccString v
                        ++ " in shared value result"
                    )
            choices ->
                Right
                    ( O.promotedList
                        [O.promoted step [] | step <- shortest choices]
                    )
        paths wanted t
            | Just v <- getTyVar_maybe t =
                if v == wanted
                    then [[]]
                    else map ("SharingKind" :) (paths wanted (tyVarKind v))
            | Just (_, _, a, b) <- splitFunTy_maybe t =
                map ("SharingDomain" :) (paths wanted a)
                    ++ map ("SharingCodomain" :) (paths wanted b)
            | Just (tc, args) <- splitTyConApp_maybe t =
                let visible = visibleArguments tc args
                 in concat
                        [ map
                            (argumentPath (length visible - i - 1) ++)
                            (paths wanted a)
                        | (i, a) <- zip [0 ..] visible
                        ]
            | Just (f, a) <- splitAppTy_maybe t =
                map ("SharingFunction" :) (paths wanted f)
                    ++ map ("SharingArgument" :) (paths wanted a)
            | otherwise = []
        shortest = minimumBy (comparing length)
        argumentPath n = replicate n "SharingFunction" ++ ["SharingArgument"]
