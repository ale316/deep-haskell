module Vector where

    import Data.List
    -- Can be Vec Int or Vec Float
    data Vec t = Vec [t] deriving (Show, Eq, Ord)

    -- Adds a scalar to every entry of a vector
    (+|) :: Num t => Vec t -> t -> Vec t
    Vec v +| n = Vec (map (+ n) v)

    -- Multiplies a scalar by every entry of a vector
    (*|) :: Num t => Vec t -> t -> Vec t
    Vec v *| n = Vec (map (* n) v)

    -- Adds two vectors
    -- Note: if the declared dimension of the vectors are different, it returns Nothing
    --       but it doesn't actually check the length of the list.
    (|+) :: Num t => Vec t -> Vec t -> Maybe (Vec t)
    Vec v1 |+ Vec v2 | (length v1) == (length v2) = Just (Vec (zipWith (+) v1 v2))
                     | otherwise = Nothing

    -- Multiply two vectors
    -- Note: if the declared dimension of the vectors are different, it returns Nothing
    --       but it doesn't actually check the length of the list.
    (|.) :: Num t => Vec t -> Vec t -> Maybe t
    Vec v1 |. Vec v2 | (length v1) == (length v2) = Just (foldr1 (*) (zipWith (+) v1 v2))
                     | otherwise = Nothing

-- main = print ((Vec [1, 2, 3]) |+ (Vec [2, 3, 4]))
-- main = print ((Vec [1, 2, 3]) |. (Vec [2, 3, 4]))
-- main = print ((Vec [1, 2, 3]) +| 5)
-- main = print ((Vec [1, 2, 3]) *| 5)