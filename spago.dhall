{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-halo"
, dependencies =
  [ "aff"
  , "avar"
  , "free"
  , "freeap"
  , "react-basic-hooks"
  , "wire"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
