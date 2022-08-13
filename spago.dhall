{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "deku"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "hyrule"
  , "hyrule-paraglider"
  , "maybe"
  , "monoid-extras"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "routing"
  , "routing-duplex"
  , "tuples"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
}
