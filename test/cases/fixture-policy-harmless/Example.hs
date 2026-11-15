module Example where

-- writeFile (root </> "Example.hs") generated
{- IO.withFile "Main.hs" WriteMode $ \handle -> hPutStr handle generated
   {- Bytes.appendFile destination contents -}
-}
snippets =
    [ "writeFile \"Example.hs\" \"module Example where\""
    , "Text.writeFile destination contents"
    , "openFile \"Main.hs\" WriteMode"
    ]
