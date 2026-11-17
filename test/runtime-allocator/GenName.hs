module GenName (attempt) where

import qualified Memorist.Base.ANT as Runtime
import qualified Memorist.Base.Thunk as Thunk

attempt :: Runtime.MemoristM Thunk.ThName
attempt = Runtime.genName
