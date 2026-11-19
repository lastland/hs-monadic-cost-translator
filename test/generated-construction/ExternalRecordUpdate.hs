{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module ExternalRecordUpdate where

import Test.QuickCheck (Args, maxSuccess)

newtype UserRecord = UserRecord { maxSuccess :: Int }

update :: Args -> Args
update record = record { maxSuccess = 3 }
