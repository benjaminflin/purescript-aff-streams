# Purescript Aff Streams

Purescript Aff Stream using AVars

Basic usage:

```purescript
module Main where

import Prelude
import Data.AffStream (fromCallback, fromFoldable, subscribe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let
    s =
      fromCallback \emit -> do
        delay $ Milliseconds 10.0
        emit 1
        delay $ Milliseconds 10.0
        emit 2

    s' = do
      x <- s
      fromFoldable [ x, x * 2 ]
  launchAff_ $ subscribe (liftEffect <<< log <<< show) s'
```
to yield
```
1
2
2
4
```

For most use cases look at `tests/Main.purs`

You can install by adding this repository in the `packages.dhall` file and running with `spago install`.
