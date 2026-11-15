module Example where

import qualified System.IO as IO
import System.FilePath ((</>))

run root = do
    appendFile (root </> "append.log") "more"
    _ <- IO.openFile (root </> "open.log") IO.AppendMode
    IO.withFile "with.log" IO.WriteMode (\handle -> IO.hPutStr handle "out")
    _ <- IO.openBinaryFile "binary.log" IO.WriteMode
    IO.withBinaryFile (root </> "with-binary.log") IO.WriteMode (\_ -> pure ())
