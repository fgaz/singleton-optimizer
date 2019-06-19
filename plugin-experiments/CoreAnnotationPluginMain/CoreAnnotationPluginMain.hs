module Main(main, r,f) where
import CoreAnnotationPlugin ( SomeAnn(..) )

{-# ANN r SomeAnn #-}
r 0 = 0
r n = r (n-1)

{-# ANN f SomeAnn #-}
f n = n+1

main = print $ f 5 + r 5
