module ManualNaming where

import qualified Memorist.Base.Thunk as Runtime

unused :: Runtime.ThName -> Runtime.ThName
unused = Runtime.nnext
