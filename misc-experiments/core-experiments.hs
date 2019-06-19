import GHC.Plugin.SingletonOptimizer ( OptimizeSingleton(OptimizeSingleton) )

{-# ANN r OptimizeSingleton #-}
r 0 = 0
r n = r (n-1)

{-# ANN f OptimizeSingleton #-}
f n = n+1

g n = h n - 2
  where h n' = n'-1

-- apparently ghc is too smart and turns these two bindings into simple calls
-- to a nonrecursive (well, internally recursive (=optimizable)) function
rec1, rec2 :: (Num n, Eq n) => n -> ()
rec1 0 = ()
rec1 x = rec2 (x - 1)
{-# ANN rec2 OptimizeSingleton #-}
rec2 0 = ()
rec2 x = rec1 (x - 1)

-- This turns into a Rec
accumulatorOdd, accumulatorEven :: Int -> [Bool]
{-# ANN accumulatorOdd OptimizeSingleton #-}
accumulatorOdd n
  | n < 0     = []
  | otherwise = True : accumulatorEven (n - 1)
accumulatorEven n
  | n < 0     = []
  | otherwise = False : accumulatorOdd (n - 1)

main = do
  print $ accumulatorOdd 1
  print $ rec1 2
  print $ r 3
  print $ f 3
  print $ g 3

