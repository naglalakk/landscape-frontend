{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "blog"
, dependencies =
    [ "affjax"
    , "aff-bus"
    , "argonaut"
    , "b64"
    , "css"
    , "console"
    , "datetime"
    , "dotenv"
    , "effect"
    , "elasticsearch"
    , "encoding"
    , "express"
    , "formatters"
    , "halogen"
    , "halogen-css"
    , "halogen-formless"
    , "halogen-hooks"
    , "halogen-hooks-extra"
    , "halogen-media"
    , "halogen-rawhtml"
    , "halogen-select"
    , "js-timers"
    , "precise-datetime"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    , "slug"
    , "strings"
    , "stringutils"
    , "spec"
    , "timestamp"
    , "quill"
    ]
, packages =
    ./packages.dhall
}
