module Case (probes) where

import Generated
import qualified Example as Source
import qualified Memorist.Data.List as L
import qualified Memorist.Memorist as M
import ReferenceSupport

probes :: [Probe]
probes =
    [ scalarSource "observed" (Source.observed . number) (\s -> M.fromPure1 observedM (number s))
    , scalar "unused-bottom-input" (\_ -> M.fromPure1 observedM (undefined :: Int))
    , scalar "tag-branch" (\s -> withCharacter tagM (character s))
    , scalar "pick-branch" (\s -> withCharacter pickM (character s))
    , scalar "named-branch" (\s -> withText (selectedText s))
    , scalar "matching-text" (\_ -> withText "ab")
    , scalar "mismatch-skips-bottom-tail" $ \_ -> do
        text <- M.lazy $ do
            h <- M.lazy (pure 'b')
            rest <- M.lazy (error "unused mismatching tail")
            pure (L.ConsT h rest)
        namedM text
    , failure "matching-prefix-demands-ending" $ \_ -> do
        text <- M.lazy (literalText ('a' : 'b' : undefined))
        namedM text
    , failure "tag-demands-character" $ \_ -> do
        c <- M.lazy (error "bottom character")
        tagM c
    , failure "named-demands-spine" $ \_ -> do
        text <- M.lazy (error "bottom string")
        namedM text
    ]
  where
    character s = ['a', 'b', 'z', '\0'] !! (variant s `mod` 4)
    selectedText s = ["ab", "", "a", "abc", "ba", "aa", "b", "z"] !! variant s

withCharacter :: (M.Th Char -> M.MemoristM Int) -> Char -> M.MemoristM Int
withCharacter f c = M.lazy (pure c) >>= f

withText :: [Char] -> M.MemoristM Int
withText text = M.lazy (literalText text) >>= namedM

literalText :: [Char] -> M.MemoristM (L.ListT Char)
literalText [] = pure L.NilT
literalText (c:cs) = do
    h <- M.lazy (pure c)
    t <- M.lazy (literalText cs)
    pure (L.ConsT h t)
