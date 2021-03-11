{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-halo"
, license = "BSD-3-Clause"
, repository = "https://github.com/robertdp/purescript-react-halo.git"
, dependencies =
  [ "aff"
  , "free"
  , "freeap"
  , "halogen-subscriptions"
  , "react-basic-hooks"
  , "refs"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
