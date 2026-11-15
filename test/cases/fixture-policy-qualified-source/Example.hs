module Example where

import qualified Data.ByteString as Bytes
import System.FilePath ((</>))

run root contents =
    ((Bytes.writeFile))
        ((root) </> ("Main.hs"))
        contents
