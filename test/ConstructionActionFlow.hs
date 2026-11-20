-- | Follow only first-order value transfers. A constructor introduces a path
-- to a field, not a demand on it. A known runner supplies the demand.
module ConstructionActionFlow (Node(..), Slot(..), Flow(..), executedFields) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (Module, Name)
import GHC.Types.SrcLoc (BufSpan)

data Node = Binder Name | Argument Name Int | Result Name | Written Module BufSpan | Tail Node Int | Item Node Int deriving (Eq, Ord)
data Slot = Slot Name Int deriving (Eq, Ord)
data Flow = From Node Node | Pack Node Slot Node | Project Node Slot Node | Root Node | Runs Node Node | Call Node Name [(Int, Node)]

type Edges a = Map.Map a (Set.Set a)

index :: (Ord k, Ord a) => [(k, a)] -> Map.Map k (Set.Set a)
index entries = Map.fromListWith Set.union [(k, Set.singleton v) | (k, v) <- entries]

reachable :: Ord a => Edges a -> [a] -> Set.Set a
reachable graph = go Set.empty
  where
    go seen [] = seen
    go seen (n:ns) | Set.member n seen = go seen ns
                   | otherwise = go (Set.insert n seen) (Set.toList (Map.findWithDefault Set.empty n graph) ++ ns)

-- The same first-order producer can build both the selected job and the
-- ignored job. Keep its actual arguments with its return value. Re-entering
-- an active function reuses its frame and adds the recursive call's actuals;
-- neither recursive data types nor recursive calls generate unbounded paths.
executedFields :: [Flow] -> Set.Set Node
executedFields flows = close (Set.fromList roots) Map.empty
  where
    roots = [([], n) | Root n <- flows]
    direct = index [(to, from) | From to from <- flows]
    operates = index [(result, operand) | Runs result operand <- flows]
    fields = index [((box, slot), field) | Pack box slot field <- flows]
    projects = index [(to, (box, slot)) | Project to slot box <- flows]
    calls = index [(to, (f, xs)) | Call to f xs <- flows]
    actuals = index [(Argument f i, x) | Call _ f xs <- flows, (i, x) <- xs]
    entries k table = Set.toList (Map.findWithDefault Set.empty k table)
    frame name = dropWhile ((/= name) . fst)
    enter f n ctx = case frame f ctx of [] -> (f, n):ctx; active -> active
    parents at@(ctx, n) =
        [(at, (ctx, from)) | from <- entries n direct] ++
        concat [let active = enter f n ctx in (at, (active, Result f)) :
            [((active, Argument f i), (ctx, x)) | (i, x) <- xs] | (f, xs) <- entries n calls] ++
        case n of
            Argument f _ | null ctx -> [(at, ([], x)) | x <- entries n actuals]
                         | let owner = frame f ctx, owner /= ctx -> [(at, (owner, n))]
            _ -> []

    close seen graph =
        let nodes = Set.toList seen
            projections = [(at, (ctx, box), slot) | at@(ctx, n) <- nodes, (box, slot) <- entries n projects]
            boxes = Set.fromList [box | (_, box, _) <- projections]
            producers = Map.fromSet (reachable graph . pure) boxes
            selected = [(at, (origin, field)) | (at, box, slot) <- projections,
                (origin, con) <- Set.toList (producers Map.! box), field <- entries (con, slot) fields]
            edges = selected ++ concatMap parents nodes
            runs = [(at, (ctx, operand)) | at@(ctx, n) <- nodes, operand <- entries n operates]
            next = Map.unionWith Set.union graph (index edges)
            -- Only explore the arguments of a call if a body actually reads them.
            more = Set.unions [seen, boxes, Set.fromList (map snd runs), Set.fromList [from | at <- nodes, from <- entries at next]]
        in if seen == more && graph == next
           then let execution = Map.unionWith Set.union graph (index runs)
                    live = reachable execution roots
                    -- An action returned unchanged has no ordinary operation.
                    -- A bind used only in a discarded argument is not run.
                    operands = reachable execution [x | (at, x) <- runs, Set.member at live]
                in Set.map snd (reachable graph [field | (at, field) <- selected, Set.member at operands])
           else close more next
