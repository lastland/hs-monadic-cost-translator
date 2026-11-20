module Cost.Translate.Frontend.Match.Row
    ( Body
    , Result
    , Row (..)
    , Step (..)
    , Subject (..)
    , Check (..)
    , Branch
    , tag
    , emitBranch
    , prepareRow
    ) where

import Cost.Translate.Frontend.Allocation (simpleIdentifier)
import Cost.Translate.Frontend.Environment
import Cost.Translate.Frontend.LocalValues (usedNames)
import Cost.Translate.Frontend.Match.Facts
import Cost.Translate.Frontend.Match.Fallback (freeTerms)
import Cost.Translate.Frontend.Match.Pattern
import Cost.Translate.Frontend.NamePlan (NamePlan (..))
import Cost.Translate.Frontend.Resolve (resolvedConstructor)
import Cost.Translate.Frontend.Syntax (patternRoot)
import Cost.Translate.Output qualified as O
import Data.IntMap.Strict qualified as IntMap
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (varName)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

type Result = Either String O.Expr
type Body = Env -> O.Expr -> Result

data Row = Row
    { scalarPositions :: Set.Set [Int]
    , nextEquation :: String
    , rowStep :: Step
    }

data Step = Ready (Facts -> O.Expr -> Result) | Inspect Subject Check

data Subject = Subject
    { position :: [Int]
    , reference :: O.Expr
    , matchedName :: String
    }

data Check = Compare O.Expr (Either String Row) | Choose Branch

data Branch = Branch
    { tag :: String
    , fieldCount :: Int
    , reserved :: Set.Set String
    , fieldNames :: IntMap.IntMap Field
    , enter :: Slots -> Either String Row
    }

type Slots = IntMap.IntMap String

data Field = Field String [(String, String)]

prepareRow ::
    Env ->
    Sources ->
    (SrcSpan, [(O.Expr, LPat GhcRn)], Body) ->
    Either String Row
prepareRow env sources (location, patterns, body) =
    mapM root indexed >>= advance (incomingNames env patterns) env sources
    where
        indexed = zip [0 ..] patterns
        next = actionName env "nextEquation" location
        root (index, (thunk, pat)) =
            ([index],thunk,) <$> patternAt labels pat (patternRoot pat index)
        labels name = case lookupEnv name env of
            Just (Constructor _ _ names) -> Just names
            _ -> Nothing
        row bindings = Row (Set.fromList (map fst bindings)) next
        advance _ scope bindings [] = Right (row bindings (Ready finish))
            where
                finish facts = body (refine facts bindings scope)
        advance used scope bs ((column, thunk, pat) : rest) = case test pat of
            Skip -> done
            Equal value -> inspect (Right (Compare value done))
            Deconstruct head' fields ->
                inspect $
                    Choose <$> prepareBranch used scope head' fields descend
            where
                bound = extendEnv [(n, Thunk thunk) | n <- aliases pat] scope
                bindings' = bindAliases scope column (aliases pat) ++ bs
                done = advance used bound bindings' rest
                inspect makeCheck = do
                    name <- planned (plannedMatches (namePlan scope)) pat
                    let pending =
                            concat
                                [ bindAliases scope p (aliases q)
                                | (p, _, q) <- rest
                                ]
                        subject = Subject column thunk name
                    row (bindings' ++ pending) . Inspect subject <$> makeCheck
                descend slots fields =
                    let child (i, slot, p) = (i : column, O.v slot, p)
                        siblings = Set.fromList (IntMap.elems slots)
                        pending = map child fields ++ rest
                     in advance (used <> siblings) bound bindings' pending

incomingNames :: Env -> [(O.Expr, a)] -> Set.Set String
incomingNames env patterns =
    Set.unions
        ( occupiedNames env
            : Set.fromList (Map.elems bindings)
            : map (usedNames . fst) patterns
        )
    where
        plan = namePlan env
        bindings = Map.intersection (plannedLocals plan) (plannedBodies plan)

prepareBranch ::
    Set.Set String ->
    Env ->
    Head ->
    [(Int, Pattern)] ->
    (Slots -> [(Int, String, Pattern)] -> Either String Row) ->
    Either String Branch
prepareBranch used env head' fields descend = do
    (target, count) <- resolveHead env head' (length fields)
    names <- traverse (traverse field) live
    pure $
        Branch target count used (IntMap.fromList names) $ \slots ->
            descend slots [(i, slotAt slots i, p) | (i, p) <- live]
    where
        live = filter (needsField . snd) fields
        field pat = do
            backup <- planned (plannedFields (namePlan env)) pat
            pure $
                Field
                    backup
                    [ (occurrence, localName env n)
                    | n <- reverse (aliases pat)
                    , let occurrence = getOccString n
                    , simpleIdentifier occurrence
                    ]

sharedSlots :: [Branch] -> Slots
sharedSlots branches = snd (IntMap.mapAccum choose used fields)
    where
        used = Set.unions (map reserved branches)
        fields = foldr combine IntMap.empty branches
        combine b = IntMap.unionWith merge (fieldNames b)
        merge (Field backup xs) (Field _ ys) = Field backup (xs ++ ys)
        choose occupied (Field backup aliases') =
            let choices = map fst aliases' ++ map snd aliases'
                fits = (`Set.notMember` occupied)
                name = fromMaybe backup (find fits choices)
             in (Set.insert name occupied, name)

slotAt :: Slots -> Int -> String
slotAt slots index = IntMap.findWithDefault "_" index slots

emitBranch ::
    NonEmpty Branch -> ([Row] -> Result) -> Either String (O.Pat, O.Expr)
emitBranch (first :| rest) compile = do
    let branches = first : rest
        slots = sharedSlots branches
    rows <- mapM (`enter` slots) branches
    body <- compile rows
    let used = freeTerms body
        field i = case IntMap.lookup i slots of
            Just n | O.rdr varName n `Set.member` used -> O.p n
            _ -> O.p "_"
    pure (O.conP (tag first) (map field [0 .. fieldCount first - 1]), body)

planned :: Map.Map String String -> Pattern -> Either String String
planned names pat =
    maybe
        (Left ("missing planned pattern name: " ++ path))
        Right
        (Map.lookup path names)
    where
        path = sourcePath pat

resolveHead :: Env -> Head -> Int -> Either String (String, Int)
resolveHead _ (Builtin target) count = Right (target, count)
resolveHead env (Named name) count = case lookupEnv name env of
    Just (Constructor target arity _) -> Right (target, arity)
    _ -> case resolvedConstructor env name of
        Just target -> Right (target, count)
        Nothing ->
            Left $
                showSDocUnsafe (ppr name)
                    ++ ": constructor pattern not implemented yet"
