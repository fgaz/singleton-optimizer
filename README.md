# Tesi Triennale -- Optimizing Total Singletons

TODO write a proper readme

* `thesis` → the tex source of the thesis
* `optimizer` → code
  * `optimizer/singleton-optimizer` → the actual optimizer (the rest are experiments)

## Usage

You'll need a development version of Liquid Haskell. If you use cabal new-build,
you can find an appropriate source-repository in the `cabal.project` file.

Add the `-fplugin GHC.Plugin.SingletonOptimizer` ghc option and mark the
singletons to optimize with the annotation `OptimizeSingleton`.

For example, in your .cabal file:

```cabal
library somelib
  ghc-options:         -fplugin GHC.Plugin.SingletonOptimizer
  build-depends:       base, singleton-optimizer
  exposed-modules:     YourModule
  hs-source-dirs:      src
  default-language:    Haskell2010
```

And in your module:

```haskell
{-# ANN optimizedEquality OptimizeSingleton #-}
optimizedEquality :: SomeType :~: SomeEquivalentType
optimizedEquality = -- some expensive expression that eventually returns a 'Refl'
```

