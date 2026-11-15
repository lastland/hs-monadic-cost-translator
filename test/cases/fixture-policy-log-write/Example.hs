module Example where

import System.FilePath ((</>))

run root out err = do
    writeFile (root </> "compile.log") (out ++ err)
    writeFile "run.log" "finished"
