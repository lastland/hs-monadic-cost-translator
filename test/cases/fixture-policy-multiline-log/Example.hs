module Example where

import qualified Data.Text.IO as Text
import System.FilePath ((</>))

run root contents =
    ((Text.writeFile))
        ((root) </> ("compile.log"))
        contents
