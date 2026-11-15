module Example where
guarded x = case x:undefined of { ys | h:_ <- ys, let z = h+h, z > 0 && True -> z; _ -> undefined }
