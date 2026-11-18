module Cost.Translate.Datatypes.Assembly
    ( emitDatatypes
    , emitDatatypeSections
    , checkConversionExports
    ) where

import Control.Monad (unless)
import Cost.Translate.Datatypes.BuiltinDemand (builtinDemandSupport)
import Cost.Translate.Datatypes.CharSupport (charSupport)
import Cost.Translate.Datatypes.Constructor
    ( demandCon
    , demandFields
    , targetCon
    , targetFields
    )
import Cost.Translate.Datatypes.Conversion
    ( conversionInstance
    , reprDemandInstances
    )
import Cost.Translate.Datatypes.Declaration
import Cost.Translate.Datatypes.Demand (demandInstances)
import Cost.Translate.Datatypes.DemandSupport (generationSupport)
import Cost.Translate.Datatypes.Generation
import Cost.Translate.Datatypes.Selectors (selectorDefinitions)
import Cost.Translate.Kinds (kindSupport)
import Cost.Translate.Output qualified as O
import Data.Set qualified as Set
import GHC
import GHC.Types.Name (getOccString)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

emitDatatypes :: [TyCon] -> Either String [O.Decl]
emitDatatypes types = do
    (common, support, declarations) <- emitDatatypeSections types
    pure (common ++ support ++ declarations)

emitDatatypeSections :: [TyCon] -> Either String ([O.Decl], [O.Decl], [O.Decl])
emitDatatypeSections types = do
    support <- kindSupport types
    selectorDecls <- selectorDefinitions types
    declarations <- concat <$> mapM located types
    pure
        ( charSupport ++ generationSupport ++ builtinDemandSupport
        , support
        , selectorDecls ++ declarations
        )
    where
        located tc =
            either
                (Left . (location tc ++))
                (Right . datatypeDeclarations)
                (datatypeInfo types tc)
        location tc = showSDocUnsafe (ppr (nameSrcSpan (getName tc))) ++ ": "

datatypeDeclarations :: Datatype -> [O.Decl]
datatypeDeclarations datatype =
    [ dataDecl datatype "T" targetFields targetCon
    , conversionInstance datatype
    , dataDecl datatype "D" demandFields demandCon
    , derived datatype "Eq"
    , derived datatype "Show"
    ]
        ++ demandInstances datatype
        ++ [generatedInstance datatype]
        ++ demandHelpers datatype
        ++ [arbitraryInstance datatype, demandArbitraryInstance datatype]
        ++ reprDemandInstances datatype

checkConversionExports :: [Name] -> [TyCon] -> Either String ()
checkConversionExports exported = mapM_ check
    where
        visible = Set.fromList exported
        check tc = do
            mapM_ (requireExport "constructor" . getName) (tyConDataCons tc)
            requireExport "type" (getName tc)
        requireExport kind name =
            unless (name `Set.member` visible) $
                Left
                    ( showSDocUnsafe (ppr (nameSrcSpan name))
                        ++ ": generated conversions require the source module \
                           \to export "
                        ++ kind
                        ++ " "
                        ++ getOccString name
                    )
