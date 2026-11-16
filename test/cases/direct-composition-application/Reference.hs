module Generated where

import Memorist.Memorist

add3M :: Num a => Th a -> Th a -> Th a -> MemoristM a
add3M ta tb tc = do
    tick
    a <- force ta
    b <- force tb
    c <- force tc
    pure (a + b + c)

doubleM :: Num a => Th a -> MemoristM a
doubleM tx = do
    tick
    x <- force tx
    pure (x + x)

composedM :: Num a => Th a -> MemoristM a
composedM tx = do
    tick
    -- (add3 x . double) x captures double x before its final argument.
    middle <- lazy (doubleM tx)
    add3M tx middle tx
