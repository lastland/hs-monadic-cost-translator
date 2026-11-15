module Example where
keep x = True
filtered x = head (filter keep (x : undefined))
