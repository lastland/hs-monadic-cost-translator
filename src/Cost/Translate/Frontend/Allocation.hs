module Cost.Translate.Frontend.Allocation
    ( assignBinders
    , assignParameters
    , assignArguments
    , assignActions
    , assignFields
    , assignPrimed
    , assignBindingNames
    , assignSpecializations
    , freshPrimed
    , simpleIdentifier
    , fixedReservedNames
    ) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC (Name)
import GHC.Types.Name (getOccString)

assignBinders ::
    Set.Set String -> [Name] -> (Map.Map Name String, Set.Set String)
assignBinders = go Map.empty
    where
        go assigned used [] = (assigned, used)
        go assigned used (name : names) =
            go (Map.insert name fresh assigned) (Set.insert fresh used) names
            where
                occurrence = getOccString name
                base
                    | simpleIdentifier occurrence = occurrence
                    | otherwise = "local"
                fresh = freshPrimed used base

assignParameters ::
    Map.Map Name String ->
    Set.Set String ->
    [((Name, Int), Maybe Name)] ->
    (Map.Map (Name, Int) String, Set.Set String)
assignParameters locals = go Map.empty 0
    where
        go assigned _ used [] = (assigned, used)
        go assigned next used ((key, source) : rest) =
            case source >>= (`Map.lookup` locals) of
                Just name -> go (Map.insert key name assigned) next used rest
                Nothing ->
                    let (name, next') = freshNumbered used "arg" next
                     in go
                            (Map.insert key name assigned)
                            next'
                            (Set.insert name used)
                            rest

assignArguments ::
    Set.Set String ->
    [(String, Int)] ->
    (Map.Map String [String], Set.Set String)
assignArguments = go Map.empty 0
    where
        go assigned _ used [] = (assigned, used)
        go assigned next used ((key, count) : sites) =
            go (Map.insert key names assigned) next' used' sites
            where
                (names, next', used') = allocate count next used
                allocate 0 number occupied = ([], number, occupied)
                allocate remaining number occupied =
                    let (name, following) = freshNumbered occupied "arg" number
                        claimed = Set.insert name occupied
                        (more, final, occupied') =
                            allocate (remaining - 1) following claimed
                     in (name : more, final, occupied')

assignActions ::
    Set.Set String ->
    Map.Map String String ->
    Map.Map String Int ->
    [String] ->
    (Map.Map String String, Set.Set String)
assignActions used assigned _ [] = (assigned, used)
assignActions used assigned counters (key : keys) =
    assignActions
        (Set.insert name used)
        (Map.insert key name assigned)
        (Map.insert prefix next counters)
        keys
    where
        prefix = takeWhile (/= '_') key
        counter = Map.findWithDefault 0 prefix counters
        (name, next) = freshNumbered used prefix counter

assignFields ::
    Set.Set String ->
    [String] ->
    (Map.Map String String, Set.Set String)
assignFields = go Map.empty 0
    where
        go assigned _ used [] = (assigned, used)
        go assigned next used (key : keys) =
            go (Map.insert key name assigned) next' (Set.insert name used) keys
            where
                (name, next') = freshNumbered used "field" next

assignPrimed ::
    Set.Set String ->
    String ->
    [String] ->
    (Map.Map String String, Set.Set String)
assignPrimed = go Map.empty
    where
        go assigned used _ [] = (assigned, used)
        go assigned used base (key : keys) =
            go (Map.insert key name assigned) (Set.insert name used) base keys
            where
                name = freshPrimed used base

assignBindingNames ::
    String ->
    Map.Map Name String ->
    Set.Set String ->
    [Name] ->
    (Map.Map Name String, Set.Set String)
assignBindingNames suffix locals = go Map.empty
    where
        go assigned used [] = (assigned, used)
        go assigned used (source : sources) =
            go (Map.insert source name assigned) (Set.insert name used) sources
            where
                name = freshPrimed used (sourceBase locals source ++ suffix)

assignSpecializations ::
    Map.Map Name String ->
    Set.Set String ->
    [(Name, [a])] ->
    (Map.Map Name [(a, String)], Set.Set String)
assignSpecializations locals = go Map.empty
    where
        go assigned used [] = (assigned, used)
        go assigned used ((source, types) : rest) =
            go (Map.insert source names assigned) used' rest
            where
                indexed = zip [0 :: Int ..] types
                (used', names) = mapAccumL allocate used indexed
                allocate occupied (index, ty) =
                    let name =
                            freshPrimed
                                occupied
                                (sourceBase locals source ++ show index)
                     in (Set.insert name occupied, (ty, name))

freshNumbered :: Set.Set String -> String -> Int -> (String, Int)
freshNumbered used prefix number =
    (freshPrimed used (prefix ++ show number), number + 1)

freshPrimed :: Set.Set String -> String -> String
freshPrimed used base = choose (0 :: Int)
    where
        choose count
            | candidate `Set.member` used = choose (count + 1)
            | otherwise = candidate
            where
                candidate = base ++ replicate count '\''

sourceBase :: Map.Map Name String -> Name -> String
sourceBase locals source = Map.findWithDefault "local" source locals

simpleIdentifier :: String -> Bool
simpleIdentifier (initial : rest) =
    (isAlpha initial || initial == '_')
        && all valid rest
    where
        valid character = isAlphaNum character || character `elem` "_'"
simpleIdentifier [] = False

fixedReservedNames :: Set.Set String
fixedReservedNames =
    Set.fromList
        [ "M"
        , "L"
        , "Source"
        , "ML"
        , "MI"
        , "MN"
        , "Reflection"
        , "TypeLits"
        , "NE"
        , "QC"
        , "Void"
        , "error"
        , "negate"
        , "pure"
        , "Proxy"
        , "sharedAt"
        , "sharedAtBy"
        , "foldrT"
        , "foldlT"
        , "concatMapT"
        , "operand"
        , "condition"
        , "guardResult"
        , "matched"
        , "mapThunk"
        , "mapListT"
        , "id"
        , "a"
        , "b"
        , "f"
        , "ta"
        , "tb"
        , "tf"
        , "tx"
        ]
