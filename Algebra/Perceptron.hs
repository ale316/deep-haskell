module Perceptron where
    import Vector

    data Perceptron w b = Perceptron (Vec w) b deriving (Show, Eq, Ord)

    stepEval :: Num t => Perceptron t b -> Vec t -> Int
    stepEval (Perceptron w b) x | ((w |. x) + b <= 0) = 0
                                | otherwise = 1

