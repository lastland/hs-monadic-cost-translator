-- | Source-bound summaries of the external construction interface. A module,
-- package, spelling, or pure result type is never a permission by itself.
module ConstructionRuntime
    ( Runtime, Role(..), runtime, roleOf, roleNames, runtimeOwns, runtimeName
    , resolveValue, resolveExport, resolveType, describeName, isMemoristType, isProtected
    , boundInstance, scalarType, standardNames, standardMethods, Family(..)
    ) where

import Control.Monad (forM, unless)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import GHC
import GHC.Builtin.Types (intTyCon, integerTyCon, boolTyCon, charTyCon, floatTyCon, doubleTyCon, unitTyCon, unitDataCon, nilDataCon, consDataCon)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.InstEnv (is_dfun)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (mkTyConApp, splitTyConApp_maybe, splitForAllTyVars, splitFunTys)
import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Iface.Syntax (ifDFun)

data Role
    = Primitive | Derived | Representation | DemandData
    | View | Payload | EmptyAccounting | UnionAccounting | Annotation
    | ReprMethod | AbstractMethod | FullMethod | UsageMethod | MinimumMethod | DemandArbitraryMethod
    deriving (Eq, Show)

data Runtime = Runtime
    { runtimeUnit :: Unit
    , roles :: Map.Map Name Role
    , typeNames :: Map.Map (String, String) TyCon
    , instances :: Map.Map Name (Name, [Type])
    , standardNames :: [(String, Name)]
    }

-- Audited against the actual library source and its loaded mi_src_hash. Keep
-- these fixed: re-resolving an old Name must not certify changed library code.
-- Installed/sdist tests need the matching interfaces, not a source checkout.
pins :: [(String, Fingerprint)]
pins =
    [ ("Memorist.Base.Thunk", Fingerprint 0x1c174c34ac15bbaf 0x90386e0ed12c45e6)
    , ("Memorist.Base.Core", Fingerprint 0x9ad5430a569dbdbb 0x00a35a0d48e463e2)
    , ("Memorist.Base.ANT", Fingerprint 0xeaa910623344bdb2 0x2b9fc48a9e4dffa2)
    , ("Memorist.Base.Extension", Fingerprint 0x11cc78a47e2cce33 0x951ac618339d4df3)
    , ("Memorist.Data.Interface.Data", Fingerprint 0xb82378992011237e 0x3025c905fd5cee2c)
    , ("Memorist.Data.Interface.Demand", Fingerprint 0xf65523e7434daf92 0xf87992e080779742)
    , ("Memorist.Data.List", Fingerprint 0xa1764b3b79c4468c 0x595ccc8538e28252)
    , ("Memorist.Example.List", Fingerprint 0xc92bc252e6db55ed 0x104330bc6dcec512)
    , ("Memorist.Example.ListInf", Fingerprint 0x0c7c5a2054914e40 0x7e7728b744898243)
    , ("Memorist.Example.NewtonRaphson", Fingerprint 0x23af28df8cfbb379 0xb53365001103f587)
    ]

runtime :: Ghc Runtime
runtime = do
    root <- findModule (mkModuleName "Memorist.Memorist") Nothing
    owners <- forM pins $ \(label, expected) -> do
        actual <- findModule (mkModuleName label) Nothing
        unless (moduleUnit actual == moduleUnit root) (fail ("construction setup: runtime unit mismatch: " ++ label))
        info <- getModuleInfo actual >>= maybe (fail ("construction setup: missing module: " ++ label)) pure
        iface <- maybe (fail ("construction setup: missing interface: " ++ label)) pure (modInfoIface info)
        unless (mi_module iface == actual && mi_src_hash iface == expected) $
            fail ("construction setup: audited source fingerprint mismatch: " ++ label ++
                " expected=" ++ show expected ++ " interface=" ++ show (mi_src_hash iface))
        definitions <- forM (mi_insts iface) $ \instance' -> do
            let name = ifDFun instance'
            thing <- lookupName name
            case thing of
                Just (AnId identifier) | Just (cls, arguments) <- getClassPredTys_maybe
                        (snd (splitFunTys (snd (splitForAllTyVars (idType identifier))))) ->
                    pure (name, (getName cls, arguments))
                _ -> fail ("construction setup: cannot inspect audited instance: " ++ describeName name)
        pure (label, actual, definitions)
    let owner label = case [actual | (name, actual, _) <- owners, name == label] of
            [actual] -> pure actual
            _ -> fail ("construction setup: unbound summary module: " ++ label)
    entries <- forM summaries $ \(label, occurrence, role) -> do
        actual <- owner label
        name <- resolveValue actual occurrence
        pure (name, role)
    types <- fmap concat $ forM typeSummaries $ \(label, occurrences) -> do
        actual <- owner label
        forM occurrences $ \occurrence -> do
            constructor <- resolveType actual occurrence
            pure ((label, occurrence), constructor)
    ordinary <- forM ordinaryValues $ \(label, occurrence) -> do
        actual <- findModule (mkModuleName label) Nothing
        name <- resolveExport actual occurrence
        pure (label ++ "." ++ occurrence, name)
    standard <- standardInstances ordinary
    pure (Runtime (moduleUnit root) (Map.fromList entries) (Map.fromList types)
        (Map.fromList (standard ++ concat [is | (_, _, is) <- owners]))
        (("syntax.()", getName unitDataCon) : ("syntax.[]", getName nilDataCon) : ("syntax.:", getName consDataCon) : ordinary))

-- Select closed standard instance heads, not all definitions in a base unit.
-- The compiler version is pinned; each use must still match the selected class,
-- complete concrete head and this actual DFun Name. No home/given dictionary is
-- covered. Identity is only a dependency of the audited Memorist carrier.
standardInstances :: [(String, Name)] -> Ghc [(Name, (Name, [Type]))]
standardInstances ordinary = do
    classes <- fmap concat $ forM ordinary $ \(_, name) -> do
        thing <- lookupName name
        pure [getName cls | Just (AnId identifier) <- [thing], Just cls <- [isClassOpId_maybe identifier]]
    identityModule <- findModule (mkModuleName "Data.Functor.Identity") Nothing
    identity <- resolveType identityModule "Identity"
    setContext [IIDecl (simpleImportDecl (mkModuleName label)) | label <- ["Prelude", "Data.Functor.Identity"]]
    fmap concat $ forM (nub classes) $ \expected -> do
        details <- getInfo False expected
        declared <- case details of
            Just (_, _, found, _, _) -> pure found
            Nothing -> fail ("construction setup: missing standard class: " ++ describeName expected)
        pure [(getName identifier, (getName cls, [ty])) | instance' <- declared, let identifier = is_dfun instance',
            ([], body) <- [splitForAllTyVars (idType identifier)], ([], predicate) <- [splitFunTys body],
            Just (cls, [ty]) <- [getClassPredTys_maybe predicate], getName cls == expected,
            scalarType ty || eqType ty (mkTyConApp identity [])]

summaries :: [(String, String, Role)]
summaries =
    [("Memorist.Base.Core", name, Primitive) | name <- ["lazy", "force", "tick"]] ++
    [("Memorist.Example.List", name, Derived) | name <-
        ["headM", "tailM", "filterM", "mapMM", "foldrM", "foldlM", "zipWithM", "concatMapM", "appendM"]] ++
    [("Memorist.Example.ListInf", "scanlM", Derived), ("Memorist.Example.NewtonRaphson", "repeatlyM", Derived)] ++
    [("Memorist.Data.Interface.Data", name, Representation) | name <- ["NothingT", "JustT", "PairT"]] ++
    [("Memorist.Data.List", name, Representation) | name <- ["NilT", "ConsT"]] ++
    [("Memorist.Data.Interface.Demand", name, DemandData) | name <- ["Skip", "Need", "NothingD", "JustD", "PairD"]] ++
    [("Memorist.Data.List", name, DemandData) | name <- ["NilD", "ConsD"]] ++
    [ ("Memorist.Base.Thunk", "Th", View)
    , ("Memorist.Base.Thunk", "tval", Payload)
    , ("Memorist.Base.Thunk", "empty", EmptyAccounting)
    , ("Memorist.Base.Thunk", "union", UnionAccounting)
    , ("Memorist.Base.Core", "Annotated", Annotation)
    , ("Memorist.Data.Interface.Data", "reprM", ReprMethod)
    , ("Memorist.Data.Interface.Data", "abstract", AbstractMethod)
    , ("Memorist.Data.Interface.Demand", "fullDemand", FullMethod)
    , ("Memorist.Data.Interface.Demand", "usage", UsageMethod)
    , ("Memorist.Data.Interface.Demand", "minDemand", MinimumMethod)
    , ("Memorist.Data.Interface.Demand", "arbDemandFrom", DemandArbitraryMethod)
    ]

typeSummaries :: [(String, [String])]
typeSummaries =
    [ ("Memorist.Base.Thunk", ["Th", "ThName", "Annot", "Cost", "NameCost", "USet"])
    , ("Memorist.Base.Core", ["Annotated", "Memorist", "MonadMemorist", "MonadAnnot"])
    , ("Memorist.Base.ANT", ["MemoristM", "MemoristT"])
    , ("Memorist.Data.Interface.Data", ["Repr", "Rep", "MaybeT", "PairT"])
    , ("Memorist.Data.Interface.Demand", ["Demand", "Dem", "ThD", "MaybeD", "PairD", "LBoundDemand", "DemandArb"])
    , ("Memorist.Data.List", ["ListT", "ListD"])
    ]

-- Pure syntax combinators and scalar operations have separate, finite entries.
-- Methods still need a selected-carrier/instance certificate at each use site.
ordinaryValues :: [(String, String)]
ordinaryValues = [("Prelude", name) | name <-
    [ "id", ".", "$", "const", "flip", "error", "undefined", "not", "&&", "||", "++", "otherwise", "seq"
    , "True", "False", "Just", "Nothing", "Left", "Right", "LT", "EQ", "GT"
    ]] ++ [("Data.Void", "absurd")] ++ [("Prelude", name) | (name, _) <- standardMethods]

data Family = Computation | RepresentationMethod | ScalarMethod | ForbiddenMethod | UnknownMethod deriving (Eq)

standardMethods :: [(String, Family)]
standardMethods =
    [(name, Computation) | name <- ["pure", "return", ">>=", ">>", "=<<", "fmap", "<$>", "<*>", "<*", "*>"]] ++
    [(name, ScalarMethod) | name <-
        ["+", "-", "*", "negate", "abs", "signum", "fromInteger", "fromRational", "/", "recip",
         "div", "mod", "quot", "rem", "divMod", "quotRem", "==", "/=", "compare", "<", "<=", ">", ">=", "max", "min",
         "show", "showsPrec", "showList"]]

resolveValue :: Module -> String -> Ghc Name
resolveValue owner occurrence = resolve owner occurrence True

resolveExport :: Module -> String -> Ghc Name
resolveExport owner occurrence = resolve owner occurrence False

resolve :: Module -> String -> Bool -> Ghc Name
resolve owner occurrence definedHere = do
    info <- getModuleInfo owner >>= maybe (fail "construction setup: missing exports") pure
    candidates <- forM [name | name <- modInfoExports info, getOccString name == occurrence,
            not definedHere || nameModule_maybe name == Just owner] $ \name -> do
        thing <- lookupName name
        pure [name | Just actual <- [thing], case actual of
            AnId{} -> True
            AConLike (RealDataCon _) -> True
            _ -> False]
    case concat candidates of
        [name] -> pure name
        _ -> fail ("construction setup: expected one value: " ++ moduleNameString (moduleName owner) ++ "." ++ occurrence)

resolveType :: Module -> String -> Ghc TyCon
resolveType owner occurrence = do
    info <- getModuleInfo owner >>= maybe (fail "construction setup: missing type exports") pure
    things <- mapM lookupName [name | name <- modInfoExports info, getOccString name == occurrence]
    case [constructor | Just (ATyCon constructor) <- things] of
        [constructor] -> pure constructor
        _ -> fail ("construction setup: expected one type: " ++ occurrence)

roleOf :: Runtime -> Name -> Maybe Role
roleOf rt name = Map.lookup name (roles rt)

roleNames :: Runtime -> Role -> [Name]
roleNames rt role = [name | (name, actual) <- Map.toList (roles rt), actual == role]

runtimeOwns :: Runtime -> Name -> Bool
runtimeOwns rt name = maybe False ((== runtimeUnit rt) . moduleUnit) (nameModule_maybe name)

runtimeName :: Runtime -> String -> String -> Name
runtimeName rt label occurrence = maybe (error "construction setup: absent type summary") getName
    (Map.lookup (label, occurrence) (typeNames rt))

isMemoristType :: Runtime -> Type -> Bool
isMemoristType rt ty = case Map.lookup ("Memorist.Base.ANT", "MemoristM") (typeNames rt) of
    Just constructor -> eqType ty (mkTyConApp constructor [])
    Nothing -> False

isProtected :: Runtime -> Type -> Bool
isProtected rt ty = case splitTyConApp_maybe ty of
    Just (constructor, arguments) -> getName constructor `elem`
        [runtimeName rt "Memorist.Base.Thunk" name | name <- ["Th", "ThName", "Annot", "Cost", "NameCost", "USet"]] ||
        getName constructor == runtimeName rt "Memorist.Base.Core" "Annotated" || any (isProtected rt) arguments
    Nothing -> False

-- Only a selected method's audited instance head is summarized. This does not
-- grant all other methods stored in that dictionary (notably Repr.abstract).
boundInstance :: Runtime -> Name -> Maybe (Name, [Type])
boundInstance rt name = Map.lookup name (instances rt)

scalarType :: Type -> Bool
scalarType ty = case splitTyConApp_maybe ty of
    Just (constructor, []) -> getName constructor `elem`
        [getName intTyCon, getName integerTyCon, getName boolTyCon, getName charTyCon,
         getName floatTyCon, getName doubleTyCon, getName unitTyCon]
    _ -> False

describeName :: Name -> String
describeName name = maybe "<local>" (moduleNameString . moduleName) (nameModule_maybe name) ++ "." ++ getOccString name
