import Data.List

-- Can be Vec Int or Vec Float
data Vec t = Vec [t] Int deriving (Show, Eq, Ord)

-- Adds a scalar to every entry of a vector
(+|) :: Num t => Vec t -> t -> Vec t
Vec v l +| n = Vec (map (+ n) v) l

-- Multiplies a scalar by every entry of a vector
(*|) :: Num t => Vec t -> t -> Vec t
Vec v l *| n = Vec (map (* n) v) l

-- Adds two vectors
-- Note: if the declared dimension of the vectors are different, it returns Nothing
--       but it doesn't actually check the length of the list.
(|+) :: Num t => Vec t -> Vec t -> Maybe (Vec t)
Vec v1 l1 |+ Vec v2 l2 | l1 == l2 = Just (Vec (zipWith (+) v1 v2) l1)
                        | l1 /= l2 = Nothing

-- Multiply two vectors
-- Note: if the declared dimension of the vectors are different, it returns Nothing
--       but it doesn't actually check the length of the list.
(|.) :: Num t => Vec t -> Vec t -> Maybe t
Vec v1 l1 |. Vec v2 l2 | l1 == l2 = Just (foldr1 (*) (zipWith (+) v1 v2))
                       | l1 /= l2 = Nothing

-- main = print ((Vec [1, 2, 3] 3) <+> (Vec [2, 3, 4] 3))
main = print ((Vec [1, 2, 3] 3) |. (Vec [2, 3, 4] 3))
-- main = print ((Vec [1, 2, 3] 3) |+| 5)
-- main = print ((Vec [1, 2, 3] 3) |*| 5)