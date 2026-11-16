module Generated where

import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M

tagM :: M.Th Char -> M.MemoristM Int
tagM c = do
    M.tick
    character <- M.force c
    pure (case character of 'a' -> 1; _ -> 2)

namedM :: M.Th (L.ListT Char) -> M.MemoristM Int
namedM text = do
    M.tick
    first <- M.force text
    case first of
        L.NilT -> pure 4
        L.ConsT a rest -> do
            firstCharacter <- M.force a
            if firstCharacter /= 'a' then pure 4 else do
                second <- M.force rest
                case second of
                    L.NilT -> pure 4
                    L.ConsT b ending -> do
                        secondCharacter <- M.force b
                        if secondCharacter /= 'b' then pure 4 else do
                            end <- M.force ending
                            pure (case end of L.NilT -> 3; L.ConsT _ _ -> 4)

pickM :: M.Th Char -> M.MemoristM Int
pickM c = do
    M.tick
    character <- M.force c
    pure (if character == 'z' then 10 else 20)

observedM :: M.Th a -> M.MemoristM Int
observedM _ = do
    M.tick
    a <- M.lazy (pure 'a')
    tagged <- tagM a
    text <- M.lazy (characters "ab")
    name <- namedM text
    z <- M.lazy (pure 'z')
    selected <- pickM z
    pure (tagged + name + selected)

-- Building a source literal is not a source-function call.
characters :: [Char] -> M.MemoristM (L.ListT Char)
characters [] = pure L.NilT
characters (c:cs) = do
    h <- M.lazy (pure c)
    t <- M.lazy (characters cs)
    pure (L.ConsT h t)
