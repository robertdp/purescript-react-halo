{ name = "react-halo"
, license = "BSD-3-Clause"
, repository = "https://github.com/robertdp/purescript-react-halo.git"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "freeap"
  , "halogen-subscriptions"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "react-basic-hooks"
  , "refs"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
