module Example where
choose xs | False = undefined
choose [] = undefined
choose (y:_) | True, False = undefined | True = y
ordered x = choose (x : undefined)
