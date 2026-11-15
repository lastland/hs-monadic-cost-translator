module Example where

import System.FilePath ((</>))

run root = writeFile (root </> "Example.hs") "module Example where"
