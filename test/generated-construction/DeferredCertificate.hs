{-# OPTIONS_GHC -fdefer-type-errors #-}
module DeferredCertificate where

import qualified Memorist.Memorist as M

ordinary :: M.MemoristM Int
ordinary = pure 17
