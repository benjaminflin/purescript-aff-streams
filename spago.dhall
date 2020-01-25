{-
Purescript Aff Streams based on AVars
-}
{ name = "aff-streams"
, dependencies = [ "console", "effect", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
