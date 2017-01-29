import Data.List

-- Can be Vec Int or Vec Float
data Vec t = Vec [t] Int deriving (Show, Eq, Ord)

(<+>) :: Num t => Vec t -> Vec t -> Vec t
(Vec v1 l1) <+> (Vec v2 l2) = Vec (zipWith (+) v1 v2) l1

main = print ((Vec [1, 2, 3] 3) <+> (Vec [2, 3, 4] 3))