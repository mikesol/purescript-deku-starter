let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220507/packages.dhall
        sha256:cf54330f3bc1b25a093b69bff8489180c954b43668c81288901a2ec29a08cc64

let overrides = {=}

let additions =
      { fast-vect =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/sigma-andex/purescript-fast-vect.git"
        , version = "main"
        }
      , event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.6.6"
        }
      , variant =
        { dependencies =
          [ "assert"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "partial"
          , "monoid-extras"
          , "prelude"
          , "record"
          , "tuples"
          , "type-equality"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/natefaubion/purescript-variant.git"
        , version = "v8.0.0"
        }
      , monoid-extras =
        { dependencies = [ "profunctor-lenses" ]
        , repo = "https://github.com/mikesol/purescript-monoid-extras.git"
        , version = "v0.0.1"
        }
      , everythings-better-with-variants =
        { dependencies =
          [ "control"
          , "foldable-traversable"
          , "invariant"
          , "newtype"
          , "prelude"
          , "variant"
          ]
        , repo =
            "https://github.com/mikesol/purescript-everythings-better-with-variants.git"
        , version = "v0.0.0"
        }
      , typelevel-eval =
        { dependencies =
          [ "effect"
          , "leibniz"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mikesol/purescript-typelevel-eval.git"
        , version = "mikesol"
        }
      , behaviors =
        { dependencies =
          [ "effect"
          , "ordered-collections"
          , "filterable"
          , "nullable"
          , "event"
          , "web-html"
          , "web-events"
          , "web-uievents"
          ]
        , repo = "https://github.com/mikesol/purescript-behaviors.git"
        , version = "v8.2.1"
        }
      , row-options =
        { dependencies = [ "homogeneous", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-row-options.git"
        , version = "v0.0.2"
        }
      , convertable-options =
        { dependencies = [ "console", "effect", "maybe", "record" ]
        , repo =
            "https://github.com/natefaubion/purescript-convertable-options.git"
        , version = "v1.0.0"
        }
      , deku =
        { dependencies =
          [ "arrays"
          , "behaviors"
          , "bolson"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "event"
          , "exists"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "indexed-monad"
          , "lists"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "record"
          , "refs"
          , "simple-json"
          , "sized-vectors"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "unsafe-coerce"
          , "variant"
          , "canvas"
          , "web-dom"
          , "web-events"
          ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "v0.4.2"
        }
      , homogeneous =
        { dependencies =
          [ "arrays"
          , "assert"
          , "control"
          , "effect"
          , "enums"
          , "foldable-traversable"
          , "foreign-object"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/mikesol/purescript-homogeneous.git"
        , version = "0.15.0"
        }
      , simple-json =
        { dependencies =
          [ "arrays"
          , "assert"
          , "control"
          , "effect"
          , "enums"
          , "foldable-traversable"
          , "foreign-object"
          , "lists"
          , "maybe"
          , "partial"
          , "prelude"
          , "tuples"
          , "typelevel-prelude"
          , "unsafe-coerce"
          , "variant"
          ]
        , repo = "https://github.com/mikesol/purescript-simple-json.git"
        , version = "0.15.0"
        }
      , wags =
        { dependencies =
          [ "arrays"
          , "behaviors"
          , "control"
          , "avar"
          , "aff-promise"
          , "homogeneous"
          , "everythings-better-with-variants"
          , "datetime"
          , "effect"
          , "either"
          , "event"
          , "exists"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "indexed-monad"
          , "lists"
          , "maybe"
          , "newtype"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "record"
          , "refs"
          , "simple-json"
          , "sized-vectors"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-peano"
          , "unsafe-coerce"
          , "variant"
          , "canvas"
          , "web-dom"
          , "web-events"
          ]
        , repo = "https://github.com/mikesol/purescript-wags.git"
        , version = "v1.1.6"
        }
      , bolson =
        { dependencies = [ "heterogeneous", "fast-vect" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "v0.0.2"
        }
      }

in  upstream // overrides // additions
