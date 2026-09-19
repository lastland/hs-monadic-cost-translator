module Datatypes where

data Tree a = Leaf | Node a (Tree a) (Tree a)

data Nested a = End a | More (Nested [a])

grow x = Node x (grow x) (grow x)

treeSum Leaf = 0
treeSum (Node a l r) = a + treeSum l + treeSum r

total x = treeSum (Node x Leaf Leaf)

deep x = treeSum (Node x (Node (x + 1) Leaf Leaf) Leaf)

readNested (More (End (x : _))) = x

sample = readNested (More (End [7, 8, 9]))
