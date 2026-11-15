module Example where

import qualified Data.ByteString as Bytes

run contents = Bytes.appendFile "Main.hs" contents
