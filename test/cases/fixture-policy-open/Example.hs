module Example where

import qualified System.IO as IO

run = do
    IO.openFile "Example.hs" IO.WriteMode
    IO.withFile "Main.hs" IO.AppendMode (\handle -> pure ())
    IO.openBinaryFile "Input.hs" IO.WriteMode
    IO.withBinaryFile "Driver.hs" IO.WriteMode (\handle -> pure ())
