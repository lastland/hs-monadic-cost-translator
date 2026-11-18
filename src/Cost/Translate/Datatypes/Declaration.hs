module Cost.Translate.Datatypes.Declaration
    ( Datatype (..)
    , datatypeInfo
    , generatedType
    , targetType
    , originalType
    , mappedVariables
    , demandType
    , constraints
    , signature
    , inst
    , shape
    , dataDecl
    , derived
    ) where

import Control.Monad (when)
import Cost.Translate.Datatypes.Constructor
import Cost.Translate.Kinds (renderBinder, visibleVariables)
import Cost.Translate.Names (generatedName, sourceConstructor)
import Cost.Translate.Output qualified as O
import GHC
import GHC.Types.Name (getOccString)

data Datatype = Datatype
    { datatypeName :: Name
    , typeVariables :: [O.Ty]
    , typeBinders :: [O.Binder]
    , quantifiedVariables :: [O.Binder]
    , datatypeConstructors :: [Constructor]
    }

datatypeInfo :: [TyCon] -> TyCon -> Either String Datatype
datatypeInfo types tc = do
    when (isNewTyCon tc) (Left "newtype declarations are unsupported")
    cs <- mapM (constructorInfo types) (tyConDataCons tc)
    binders <- mapM (renderBinder types) (visibleVariables tc)
    quantified <- mapM (renderBinder types) (tyConTyVars tc)
    pure
        ( Datatype
            (getName tc)
            (map (O.t . getOccString) (visibleVariables tc))
            binders
            quantified
            cs
        )

generatedType :: Datatype -> String -> O.Ty
generatedType datatype suffix =
    O.tc (generatedName suffix (datatypeName datatype)) (typeVariables datatype)

targetType :: Datatype -> O.Ty
targetType datatype = generatedType datatype "T"

originalType :: Datatype -> O.Ty
originalType datatype =
    O.tc (sourceConstructor (datatypeName datatype)) (typeVariables datatype)

mappedVariables :: Datatype -> String -> [O.Ty]
mappedVariables datatype family = [O.tc family [v] | v <- vs]
    where
        vs = typeVariables datatype

demandType :: Datatype -> O.Ty
demandType datatype =
    O.tc
        (generatedName "D" (datatypeName datatype))
        (mappedVariables datatype "DemOf")

constraints :: Datatype -> String -> [O.Ty]
constraints datatype cls = [O.tc cls [v] | v <- typeVariables datatype]

signature :: Datatype -> [O.Ty] -> O.Ty -> O.Ty
signature datatype ctxt body =
    O.forallT (quantifiedVariables datatype) (O.qualified ctxt body)

inst :: Datatype -> [O.Ty] -> O.Ty -> [O.Decl] -> O.Decl
inst datatype ctxt target = O.instanceD (signature datatype ctxt target)

shape :: String -> O.Ty -> O.Ty
shape cls ty = O.tc "ShapeK" [O.t cls, ty]

dataDecl ::
    Datatype ->
    String ->
    (Constructor -> [O.Ty]) ->
    (Constructor -> String) ->
    O.Decl
dataDecl datatype suffix fields con =
    O.dataD
        (generatedName suffix (datatypeName datatype))
        (typeBinders datatype)
        [ (con c, [O.tc thunk [f] | f <- fields c])
        | c <- datatypeConstructors datatype
        ]
    where
        thunk = if suffix == "T" then "M.Th" else "M.ThD"

derived :: Datatype -> String -> O.Decl
derived datatype cls =
    O.derivingD
        ( signature
            datatype
            (map (shape cls) (typeVariables datatype))
            (O.tc cls [generatedType datatype "D"])
        )
