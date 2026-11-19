{-# LANGUAGE FlexibleContexts #-}
module HomeReprGiven where

import HomeReprControl (Box)
import qualified Memorist.Memorist as M

generic :: M.Memorist s m => Box -> m Box
generic = M.reprM
