module Example where

data Tree a = Leaf | Node a (Tree a) (Tree a)
