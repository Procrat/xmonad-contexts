# XMonad.Actions.Contexts

This is an [XMonad](http://xmonad.org) extension that allows to **switch the
current set of workspaces with other named sets**. This is quite useful if you
use multiple workspaces to work on a single project. Then you can create a
context per project and temporarily make sure you don't need to see irrelevant
workspaces.


## Usage

### Hold my hand

Clone this repo into your `~/.xmonad/lib` directory or make it available to you
in another way.
```sh
git clone git@github.com:Procrat/xmonad-contexts ~/.xmonad/lib/
```

I find this extension particularly enjoyable in combination with a menu, like
[`dmenu`](http://tools.suckless.org/dmenu/). So I am going to assume you have it
(or something similar) installed.

Add the following to the imports of your `~/.xmonad/xmonad.hs`.
```haskell
import qualified XMonad.Util.Dmenu       as D
import qualified XMonad.Actions.Contexts as C
```

Then add a key binding that switches contexts and creates it if it did not
exist, and a key binding that deletes a context.
This example uses
[XMonad.Util.EZConfig](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html)-style
key bindings, but this is not necessary.
```haskell
    ...
    , ("M-s", C.listContextNames >>= D.dmenu >>= C.createAndSwitchContext)
    , ("M-S-s", C.listContextNames >>= D.dmenu >>= C.deleteContext >> return ())
    ...
```

As a final step, which is required to make your layouts in non-visible contexts
persistent across restarts of XMonad, you have to make the following `instance`
where `myLayout` is the layout you defined in the `layoutHook` field of your
XMonad configuration.
```haskell
{-# LANGUAGE FlexibleInstances #-}
import XMonad (readsLayout)

instance Read (Layout Window) where
    readsPrec _ = readsLayout (Layout myLayout)
```


### Just give me the API

I think the function names are pretty self-explanatory. If they are not, please
tell me so. Usage of them requires an orphan instance of `Read (Layout Window)`
in your `xmonad.hs` to be able to make layouts persistents across restarts of
XMonad. If there is a way to work around this hack, I would love to know!
```haskell
type ContextName = String

createContext          :: Read (Layout Window) => ContextName -> X ()
switchContext          :: Read (Layout Window) => ContextName -> X Bool
createAndSwitchContext :: Read (Layout Window) => ContextName -> X ()
deleteContext          :: Read (Layout Window) => ContextName -> X Bool
showCurrentContextName :: Read (Layout Window) => X String
listContextNames       :: Read (Layout Window) => X [ContextName]
defaultContextName     :: ContextName
```


## Similar extensions

As far as I know, there isn't really another extension like this. It may be
vaguely related to
[X.A.Plane](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-Plane.html),
[X.A.DynamicWorkspaceGroups](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-DynamicWorkspaceGroups.html)
and
[X.L.IndependentScreens](http://hackage.haskell.org/package/xmonad-contrib-0.12/docs/XMonad-Layout-IndependentScreens.html).

The most important difference between those extensions and this one is how the
workspaces are internally handled. Most extensions manipulate the workspaces
that XMonad keeps track of internally. This extension leaves those workspace
sets completely intact, but just switches to completely other ones. One of the
advantages is that this means that **all your key bindings and extensions that
manage workspaces can remain intact!**


## Troubleshooting

### Nothing happens when I try to switch. I am not using dmenu, but <X>.

I encountered a similar issue when using dmwit's
[`yeganesh`](http://dmwit.com/yeganesh/). The problem is that XMonad cleans up
child processes. In this particular case, it cleaned up `yeganesh`'s call to
`dmenu`. To fix this, temporarily disable XMonad's signal handlers in the call
to the menu.
```haskell
import XMonad

safeMenu :: [String] -> X String
safeMenu options = do
    uninstallSignalHandlers
    choice <- D.menu "yeganesh" options
    installSignalHandlers
    return choice
```
Then replace the `dmenu` call by a call to this function.
```haskell
    ...
    , ("M-s", C.listContextNames >>= safeMenu >>= C.createAndSwitchContext)
    , ("M-S-s", C.listContextNames >>= safeMenu >>= C.deleteContext >> return ())
    ...
```
