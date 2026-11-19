{-# LANGUAGE TemplateHaskell #-}
module SplicedDeclaration where

import qualified Memorist.Memorist as M

$([d| ordinary :: M.MemoristM Int
      ordinary = pure 17 |])
