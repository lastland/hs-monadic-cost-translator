module Example where

import qualified System.IO as IO

save = writeFile "compile.log"
append = IO.appendFile "append.log"
open = IO.openFile "open.log"
with = IO.withFile "with.log" IO.WriteMode
