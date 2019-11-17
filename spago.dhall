{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [
        "effect",
        "console",
        "psci-support",
        "aff",
        "canvas",
        "web-html",
        "foreign-object",
        "random",
        "generics-rep",
        "memoize",
        "foreign",
        "foreign-generic",
        "argonaut",
        "argonaut-codecs",
        "exceptions",
        "debug",
        "record",
        "refs",
        "coroutines",
        "aff-coroutines"
    ]
, packages =
    ./packages.dhall
, sources =
    [
        "src/**/Main.purs",
        "src/**/Util.purs",
        "src/**/Paper/**.purs",   
        "src/**/GameGraphics.purs",
        "src/**/State.purs",
        "src/**/Menus.purs",
        "src/**/DataTypes.purs",
        "src/**/GameLogic.purs",
        "src/**/Actions.purs",
        "src/**/Animation.purs",
        "src/**/Game.purs",
        "src/**/GameLoop.purs",
        "src/**/AI.purs",
        "src/**/UI.purs",
        "src/**/Demo.purs",
        "src/**/Shapes.purs"
    ]
}
