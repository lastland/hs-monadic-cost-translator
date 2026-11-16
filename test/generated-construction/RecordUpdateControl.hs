{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module RecordUpdateControl where

import Memorist.Memorist

newtype UserRecord = UserRecord { value :: Int }

update :: UserRecord -> UserRecord
update record = record { value = 3 }

annotation :: Annotated Int -> Annotated Int
annotation = id
