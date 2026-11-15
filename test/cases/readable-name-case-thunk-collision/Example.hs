module Example where
observed x = let caseThunk0 y = y + 1 in case x of z -> caseThunk0 z
