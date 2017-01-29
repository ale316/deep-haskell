import Data.List

-- Can be Vec Int or Vec Float
data Vec t = Vec [t] Int deriving (Show, Eq, Ord)

(<+>) :: Num t => Vec t -> Vec t -> Maybe (Vec t)
Vec v1 l1 <+> Vec v2 l2 | l1 == l2 = Just (Vec (zipWith (+) v1 v2) l1)
                        | l1 /= l2 = Nothing

main = print ((Vec [1, 2, 3] 3) <+> (Vec [2, 3, 4] 3))