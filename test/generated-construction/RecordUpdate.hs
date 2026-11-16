module RecordUpdate where

import qualified Memorist.Memorist as M

update :: M.Annotated Int -> M.Annotated Int
update record = record { M.value = 3 }
