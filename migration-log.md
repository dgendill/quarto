Migrating project from PureScript 0.12 to PureScript 0.13

The secret to migrating an old codebase is to do it gradually piece by piece. To do that, you'll specify the specific modules you want to compile starting with the base and working your way up the dependency tree. Using spago this is accomplished by updating the "sources" attribute.

---

In the past PureScript dependencies were installed using bower packages with names like "purescript-aff", "purescript-canvas", "purescript-web-html", "purescript-dom-events", etc... Using spago, I've had success using only the part after "purescript-", e.g. in spago I can install these dependencies sans "purescript-"

dependencies =
[
    "aff",
    "canvas",
    "web-html"
]

----

purescript-maps was depreciated and contained Data.StrMap. https://github.com/purescript/purescript-ordered-collections contains a Map data type, but not StrMap. It's now recommended to use https://github.com/purescript/purescript-foreign-object for native objects.  

---

purescript-generics has been replaced with purescript-generics-rep.

---

I installed a base package set using [spago](https://github.com/spacchetti/spago).

I create a blank spago project and pulled out the spago.dhall and package.dhall files into my Quarto project.

When I ran spago build I had a ton of errors. I updated the "sources" property of spago.dhall to only include Main.purs so the other files would not be compiled.

```
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
        "aff"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/Main.purs" ]
}
```

I managed to get Main.purs working with a barebones aff setup.

```
module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)

main :: Effect Unit
main = runAff_ logShow $ do
    liftEffect $ log "Hello World"
```

I was now ready to migrate some of my graphic functions.

```
import GameGraphics (GRAPHICS, createMainMenu, drawAvailablePieces, drawAvailablePiecesLayout, drawBoard, init, loadAssets, pieceAssets)

main :: Effect Unit
main = runAff_ logShow $ do
    liftEffect $ log "Hello World"
    assets <- loadAssets pieceAssets (map pieceId makePieces)
    pure unit

```

I updated spago.dhall with the next module I needed to update.

```
, sources =
    [ "src/**/Main.purs", "src/**/GameGraphics.purs" ]
```

I continued to get each module working and then moved on to the next.