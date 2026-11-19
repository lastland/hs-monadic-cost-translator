{-# LANGUAGE TypeFamilies #-}
module HomeReprControl where

import qualified Memorist.Memorist as M

data Box = Box Int

instance M.Repr Box where
    type Rep Box = Box
    reprM = pure
    abstract = id
