module Generated where

import qualified Example as CallerDatatype
import qualified Memorist.Memorist as M

keepM :: M.Th Int -> M.MemoristM Int
keepM x = do
    M.tick
    value <- M.force x
    pure (CallerDatatype.keep value)

partialM :: M.Th a -> M.MemoristM a
partialM xs = M.tick >> M.force xs

unusedLive :: Int -> Int
unusedLive x = CallerDatatype.keep x

unusedDead :: Int -> Int
unusedDead x = if True then x else CallerDatatype.keep x

callback :: (Int -> Int) -> Int
callback f = f 0

passed :: Int
passed = callback CallerDatatype.keep

partial :: [Int] -> [Int]
partial xs = CallerDatatype.partial xs

partialDead :: [Int] -> [Int]
partialDead xs = if True then xs else CallerDatatype.partial xs

class Concrete a where
    concrete :: a -> Int

instance Concrete Bool where
    concrete _ = CallerDatatype.keep 0

class ConcreteDead a where
    concreteDead :: a -> Int

instance ConcreteDead Bool where
    concreteDead x = if x then 0 else if True then 1 else CallerDatatype.keep 0

selector :: CallerDatatype.Box -> Int
selector x = CallerDatatype.payload x

valueEscape :: Int
valueEscape = CallerDatatype.zero

patternLabel :: CallerDatatype.Box -> Int
patternLabel CallerDatatype.Box { CallerDatatype.payload = x } = x
