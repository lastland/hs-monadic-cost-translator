module Example where

data Bundle = Bundle
    { items :: [Int]
    , choice :: (Int, Bool)
    , nested :: Maybe [Int]
    }

bundle :: Int -> Bool -> Bundle
bundle x flag = Bundle [x] (x, flag) (Just [x])

partial :: Int -> Bundle
partial x = Bundle undefined (x, undefined) undefined

first :: Bundle -> Int
first (Bundle _ (x, _) _) = x

second :: Bundle -> Bool
second (Bundle _ (_, flag) _) = flag

ignore :: Bundle -> Int
ignore _ = 0

twice :: Bundle -> (Int, Int)
twice b = (first b, first b)
