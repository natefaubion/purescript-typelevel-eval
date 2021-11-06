{ name = "typelevel-eval"
, dependencies =
  [ "bifunctors"
  , "contravariant"
  , "control"
  , "effect"
  , "either"
  , "leibniz"
  , "prelude"
  , "profunctor"
  , "psci-support"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
