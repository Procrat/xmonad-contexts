# XMonad.Actions.Contexts

This is an [XMonad](http://xmonad.org) extension that allows to **switch the
current set of workspaces with other named sets**. This is quite useful if you
use multiple workspaces to work on a single project. Then you can create a
context per project and temporarily make sure you don't need to see irrelevant
workspaces.

## Usage

You can use this extension by editing your `~/.xmonad/xmonad.hs`.

```haskell
import qualified XMonad.Actions.Contexts as C
```

Then add key bindings like the following (this example uses
[XMonad.Util.EZConfig](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html)-style
key bindings, but this is not necessary):

```haskell
    , ("M-n", C.createContext "context2")
    , ("M-s", C.switchContext C.defaultContextName)
    , ("M-S-s", C.switchContext "context2")
```

**Tip**: Use something like [`dmenu`](http://tools.suckless.org/dmenu/) to
dynamically choose a context name.


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

## Disclaimer

Although the creation and switching of context is fully functional, this
extension does not make the contexts persistent yet, meaning that when
restarting XMonad, the non-visible contexts are lost. I mean to make it
persistent sometime soon.
