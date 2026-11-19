module PureCarrier where

import qualified Memorist.Memorist as M

-- The spelling pure is not ret at this alternative accounting carrier.
alternative :: M.Annotated Int
alternative = pure 17

unused :: M.MemoristM Int
unused = let hidden = (pure 17 :: M.Annotated Int) in pure 3
