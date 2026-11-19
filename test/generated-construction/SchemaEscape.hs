module SchemaEscape where

import SchemaViewControl (Tree, branch)

escape :: (a -> b) -> Tree a -> Tree b
escape = branch
