
multiple :: Int -> Int -> Int
multiple x y = x * y

add:: Int -> Int -> Int
add x y = x + y

complex :: Int -> Int -> Int
complex x y = add (x + y) x


[2,3,4]
2 : (3 : (4 : []))

filterT :: (a -> Bool) -> [a] -> [a]
filterT f  (x:xs) = if f x
    then
        x : (filterT f xs)
    else
        filterT f xs
filterT :: (a -> Bool) -> [a] -> [a]
filterT f  (x:xs)
    | f x       = x : (filterT f xs)
    | otherwise = filterT f xs


if f x
    then
        x : (filterT f xs)
    else
        filterT f xs
filter f [] = []

