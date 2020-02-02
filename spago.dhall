{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aff-coroutines"
    , "console"
    , "coroutines"
    , "effect"
    , "halogen"
    , "html-parser-halogen"
    , "psci-support"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
