module HiddenConstruction where

import qualified Memorist.Memorist as M

hidden :: M.Th Int -> Int
hidden = M.tval
