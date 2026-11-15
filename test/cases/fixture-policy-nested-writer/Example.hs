module Example where

import System.IO (withFile, IOMode(WriteMode))

run = withFile "compile.log" WriteMode (\_ -> writeFile "Main.hs" "not a fixture")
