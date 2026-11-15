module Example where

-- BlockArguments is not enabled: GHC can recover an AST with a parser error.
run = writeFile "compile.log" do pure ()
