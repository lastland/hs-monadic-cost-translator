module Cost.Translate.Datatypes.Constructor
    ( Constructor (..)
    , constructors
    , constructorInfo
    , withWitnesses
    , patternFor
    , applicative
    ) where

import Control.Monad (unless, when)
import Cost.Translate.Datatypes.Types (render)
import Cost.Translate.Kinds (visibleArguments)
import Cost.Translate.Names (generatedName, sourceConstructor)
import Cost.Translate.Output qualified as O
import Data.List (nubBy)
import GHC
import GHC.Core.DataCon
    ( dataConExTyCoVars
    , dataConImplBangs
    , dataConOrigArgTys
    , dataConTheta
    , isBanged
    )
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (splitAppTy_maybe, splitTyConApp_maybe)
import GHC.Types.FieldLabel (flSelector)

data Constructor = Constructor
    { sourceCon :: String
    , targetCon :: String
    , demandCon :: String
    , fieldNames :: [String]
    , targetFields :: [O.Ty]
    , demandFields :: [O.Ty]
    , reprWitnesses :: [O.Ty]
    , demandWitnesses :: [O.Ty]
    }

constructors :: [TyCon] -> [(Name, String, Int, [Name])]
constructors types =
    [ ( getName c
      , generatedName targetSuffix (getName c)
      , length (dataConOrigArgTys c)
      , map flSelector (dataConFieldLabels c)
      )
    | t <- types
    , c <- tyConDataCons t
    ]

constructorInfo :: [TyCon] -> DataCon -> Either String Constructor
constructorInfo locals con = do
    unless
        (null (dataConExTyCoVars con) && null (dataConTheta con))
        (Left "existential/constrained constructors are unsupported")
    when
        (any isBanged (dataConImplBangs con))
        (Left "strict fields are unsupported")
    let fields = map scaledThing (dataConOrigArgTys con)
    Constructor
        (sourceConstructor (getName con))
        (generatedName targetSuffix (getName con))
        (generatedName "D" (getName con))
        ["x" ++ show i | i <- [1 :: Int .. length fields]]
        <$> mapM (render targetSuffix locals) fields
        <*> mapM (render "D" locals) fields
        <*> applicationWitnesses "ReprApp" "" locals fields
        <*> applicationWitnesses "DemandApp" targetSuffix locals fields

applicationWitnesses ::
    String -> String -> [TyCon] -> [Type] -> Either String [O.Ty]
applicationWitnesses cls suffix locals fields =
    mapM witness (nubBy same (concatMap applications fields))
    where
        same (f, a) (g, b) = eqType f g && eqType a b
        witness (f, a) = do
            f' <- render suffix locals f
            a' <- render suffix locals a
            pure (O.tc cls [f', a'])
        applications ty
            | Just (tc, args) <- splitTyConApp_maybe ty =
                concatMap applications (visibleArguments tc args)
            | Just (f, a) <- splitAppTy_maybe ty =
                applications f ++ applications a ++ [(f, a)]
            | otherwise = []

withWitnesses :: [O.Ty] -> O.Expr -> O.Expr
withWitnesses constraints body = foldr wrap body constraints
    where
        wrap c rest = O.call "withConstraint" [proxy c, rest]
        proxy c = O.ann (O.v "Proxy") (O.tc "Proxy" [c])

patternFor :: String -> [String] -> O.Pat
patternFor con = O.conP con . map O.p

applicative :: String -> [O.Expr] -> O.Expr
applicative con [] = O.ret (O.v con)
applicative con (field : fields) =
    foldl (O.op "<*>") (O.op "<$>" (O.v con) field) fields

targetSuffix :: String
targetSuffix = "T"
