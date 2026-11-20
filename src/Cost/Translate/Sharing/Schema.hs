module Cost.Translate.Sharing.Schema
    ( NativeSchemas
    , planNativeSchemas
    , nativeSchemaNames
    , nativeSchemaSupport
    , schemaDatatypes
    , schemaMapper
    ) where

import Cost.Translate.Frontend.Allocation (fixedReservedNames, freshPrimed)
import Cost.Translate.Kinds (renderBinder, visibleVariables)
import Cost.Translate.Names (generatedName)
import Cost.Translate.Output qualified as O
import Cost.Translate.Sharing.Structural
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC
    ( Name
    , TyCon
    , TyVar
    , Type
    , getName
    , isVanillaDataCon
    , tyConDataCons
    , tyConTyVars
    )
import GHC.Core.DataCon (dataConInstOrigArgTys)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.TyCon (isAlgTyCon)
import GHC.Core.Type (isLiftedTypeKind, mkTyVarTy)
import GHC.Types.Name (getOccString)
import GHC.Types.Var (tyVarKind)

data NativeSchemas = NativeSchemas
    { schemaDatatypes :: [TyCon]
    , schemaMappings :: Map.Map Slot SchemaMapping
    }

data SchemaMapping = SchemaMapping
    { mappingName :: String
    , mappingDeclarations :: [O.Decl]
    , mappingDependencies :: [Slot]
    }

planNativeSchemas :: Set.Set String -> [TyCon] -> NativeSchemas
planNativeSchemas reserved datatypes = NativeSchemas datatypes (prune proposed)
    where
        candidates =
            [ (tc, index, variable)
            | tc <- datatypes
            , isAlgTyCon tc
            , all isVanillaDataCon (tyConDataCons tc)
            , (index, variable) <- zip [0 ..] (visibleVariables tc)
            , isLiftedTypeKind (tyVarKind variable)
            ]
        (_, assignments) =
            mapAccumL assign reservedNames candidates
        reservedNames = reserved `Set.union` fixedReservedNames
        names = Map.fromList assignments
        assign used (tc, index, _) =
            let base = "map" ++ representationName (getName tc) ++ show index
                name = freshPrimed used base
             in (Set.insert name used, ((getName tc, index), name))
        proposed =
            Map.fromList
                [ (key, mapping)
                | candidate@(tc, index, _) <- candidates
                , let key = (getName tc, index)
                , Just name <- [Map.lookup key names]
                , Right mapping <- [buildMapping datatypes names name candidate]
                ]
        prune mappings =
            let supported =
                    Map.filter
                        (all (`Map.member` mappings) . mappingDependencies)
                        mappings
             in if Map.size supported == Map.size mappings
                    then mappings
                    else prune supported

nativeSchemaNames :: NativeSchemas -> Set.Set String
nativeSchemaNames = Set.fromList . Map.elems . mapperNames

mapperNames :: NativeSchemas -> Map.Map Slot String
mapperNames = Map.map mappingName . schemaMappings

schemaMapper :: NativeSchemas -> [TyVar] -> Type -> Maybe O.Expr
schemaMapper schemas variables value =
    viewMapper
        <$> planView
            (mapperNames schemas)
            [(variable, O.v "Void.absurd") | variable <- variables]
            value

nativeSchemaSupport :: NativeSchemas -> Set.Set String -> [O.Decl]
nativeSchemaSupport schemas references =
    concatMap mappingDeclarations (Map.elems selected)
    where
        mappings = schemaMappings schemas
        roots =
            Map.keysSet $
                Map.filter ((`Set.member` references) . mappingName) mappings
        selected = Map.restrictKeys mappings (closure roots)
        closure used =
            let next =
                    used
                        `Set.union` Set.fromList
                            ( concatMap mappingDependencies $
                                Map.elems (Map.restrictKeys mappings used)
                            )
             in if used == next then used else closure next

buildMapping ::
    [TyCon] ->
    Map.Map Slot String ->
    String ->
    (TyCon, Int, TyVar) ->
    Either String SchemaMapping
buildMapping datatypes names name (tc, index, variable) = do
    signature <- mappingSignature datatypes tc index variable
    constructors <- mapM constructor (tyConDataCons tc)
    pure $
        SchemaMapping
            name
            [ O.sig name signature
            , O.val name (map O.p ["convert", "structuralValue"]) $
                O.caseE (O.v "structuralValue") (map fst constructors)
            ]
            (concatMap snd constructors)
    where
        constructor dc = do
            let arguments = map mkTyVarTy (tyConTyVars tc)
                fields =
                    map scaledThing $
                        dataConInstOrigArgTys dc arguments
                target = representationName (getName dc)
            views <-
                maybe (Left "unsupported structural mapping") Right $
                    mapM (planView names [(variable, O.v "convert")]) fields
            pure
                ( constructorView target views
                , concatMap viewDependencies views
                )

mappingSignature :: [TyCon] -> TyCon -> Int -> TyVar -> Either String O.Ty
mappingSignature datatypes tc index variable = do
    binders <- mapM (renderBinder datatypes) (tyConTyVars tc)
    let used = Set.fromList (map getOccString (tyConTyVars tc))
        result = freshPrimed used "mapped"
        arguments = map (O.t . getOccString) (visibleVariables tc)
        target =
            [ if slot == index then O.t result else argument
            | (slot, argument) <- zip [0 ..] arguments
            ]
        representation = O.tc (representationName (getName tc))
    pure $
        O.forallT
            (binders ++ [O.bk result (O.t "Type")])
            ( O.arrows
                [ O.arrow (O.t (getOccString variable)) (O.t result)
                , representation arguments
                ]
                (representation target)
            )

representationName :: Name -> String
representationName = generatedName "T"
