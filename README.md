# singleton-optimizer GHC plugin

[![builds.sr.ht status](https://builds.sr.ht/~fgaz/singleton-optimizer.svg)](https://builds.sr.ht/~fgaz/singleton-optimizer?)

**A GHC plugin that optimizes away the implementation of total singletons**

TODO write a more complete readme

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

If Liquid Haskell's termination checker is unable to prove the totality of one
of your functions, you can use the `UnsafeTotal` annotation to manually mark
a binding as total.

## Current limitations

### No intra-module totality checking

Due to limitations in how GHC plugins work, all names referenced by a marked
binding have to be either whitelisted or from the same module.

### `Bytecode compiler can't handle unboxed tuples and sums` error

If all the following conditions are satisfied:

* This plugin is used on a component with multiple modules
* A module contains a typeclass instance declaration
* That module gets imported by another module of the same component
* Optimizations are enabled for that component (`-O1` or `-O2`)

Then compilation will fail with this error

```
Error: bytecode compiler can't handle unboxed tuples and sums.
  Possibly due to foreign import/export decls in source.
  Workaround: use -fobject-code, or compile this module to .o separately.
```

This is a limitation of liquidhaskell and of how it calls ghc inside an
existing ghc session.

Two possible fixes are:

* Move the instance declaration to the module where it's used
* Or separate the modules in two different components

