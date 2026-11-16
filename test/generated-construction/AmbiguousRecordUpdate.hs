{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module AmbiguousRecordUpdate where

import Memorist.Memorist (Annotated, value)

newtype UserRecord = UserRecord { value :: Int }

update :: Annotated Int -> Annotated Int
update record = record { value = 3 }
