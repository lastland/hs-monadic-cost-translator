module Example where
data Tree a = Leaf | Node a (Tree a) (Tree a)
data Nested a = End a | More (Nested [a])
grow x = Node x (grow x) (grow x)
