module Example where
pick field0 ((x,_),_) = field0 + x
observed n = pick n ((3,0),0)
