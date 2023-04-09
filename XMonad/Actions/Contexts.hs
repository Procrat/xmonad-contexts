{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Actions.Contexts (
    createContext,
    switchContext,
    switchContextFixedWs,
    createAndSwitchContext,
    createAndSwitchContextFixedWs,
    deleteContext,
    showCurrentContextName,
    listContextNames,
    moveWindowToContext,
    moveWindowToContextFixedWs,
    defaultContextName,
    showContextStorage
) where

import System.IO

import           Control.Monad               (when)
import           Data.Foldable               (for_)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.List                   as L

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.WorkspaceNames         (setWorkspaceName, getWorkspaceName)


type ContextName = String
type ContextMap = Map.Map ContextName Context

type WorkspaceNames = [(WorkspaceId, String)]

data Context = Context
    { windowSet      :: WindowSet
    , workspaceNames :: WorkspaceNames
    } deriving Show

deriving instance Read (Layout Window) => Read Context

data ContextStorage = ContextStorage
    { currentCtxName :: !ContextName
    , contextMap     :: !ContextMap
    } deriving Show

deriving instance Read (Layout Window) => Read ContextStorage

instance Read (Layout Window) => ExtensionClass ContextStorage where
    initialValue = ContextStorage defaultContextName Map.empty
    extensionType = PersistentExtension

defaultContextName :: ContextName
defaultContextName = "Main"

-------------------------------------------------------------------------------
switchContext :: Read (Layout Window) => ContextName -> X Bool
switchContext = switchContextFixedWs []

switchContextFixedWs :: Read (Layout Window) => [WorkspaceId] -> ContextName -> X Bool
switchContextFixedWs fixedWs newContextName = do
    ctxStorage <- XS.get :: X ContextStorage
    let (maybeNewCtx, ctxMap) = findAndDelete newContextName (contextMap ctxStorage) -- get new
    case maybeNewCtx of
        Nothing     -> return False
        Just newCtx -> do
            xstate <- get
            wsMap <- currentWorkspaceMap -- list of current workspaces

            let oldContext = Context (windowset xstate) wsMap -- current Context, including current names of workspaces

            let ctxMap' = Map.insert oldContextName oldContext ctxMap -- store current context in map
                    where oldContextName = currentCtxName ctxStorage

            XS.put $ ContextStorage newContextName ctxMap' -- store context

            setWindowsAndWorkspaces fixedWs oldContext newCtx

            return True

createAndSwitchContext :: Read (Layout Window) => ContextName -> X ()
createAndSwitchContext = createAndSwitchContextFixedWs []

createAndSwitchContextFixedWs :: Read (Layout Window) => [WorkspaceId] -> ContextName -> X ()
createAndSwitchContextFixedWs fixedWs name = do
    createContext name
    _ <- switchContextFixedWs fixedWs name
    return ()


-- merge windows from second set into first set if the
-- corresponding workspace id is in the list
-- First Contxt is old; Second context is new
-- Note: visible, hidden, and current workspace(s) have to be handled separately
mergeContexts :: [WorkspaceId] -> Context -> Context -> Context
mergeContexts ids ctxOld ctxNew = do

    let stackNew = windowSet ctxNew -- Workspaces of new context

    -- first, merge hidden workspaces
    let newHidden = map selectWorkspace (W.hidden stackNew)

    -- secondly, we merge visible workspaces
    let newVisible = map selectScreen (W.visible stackNew)

    -- finally, we handle the currently focused workspace
    let newFocused = selectScreen (W.current stackNew)

    -- update workspaces
    let mergedStack = stackNew  {
        W.hidden = newHidden,
        W.visible = newVisible,
        W.current = newFocused
    }

    Context mergedStack (workspaceNames ctxNew)

        where
            selectScreen screen = do
                let ws = W.workspace screen
                screen { W.workspace = selectWorkspace ws }

            selectWorkspace ws = if W.tag ws `elem` ids then oldWs ws else ws

            workspacesOld = W.workspaces (windowSet ctxOld)

            {- oldWs :: W.Workspace i l a -> W.Workspace i l a -}
            oldWs ws = fromMaybe ws (find (\x -> W.tag ws == W.tag x) workspacesOld) -- if tag is not found in workspacesOld, return new ws

-- set the window set and apply the workspaceNames
setWindowsAndWorkspaces :: [WorkspaceId] -> Context -> Context -> X ()
setWindowsAndWorkspaces fixedWs oldContext newContext = do

    -- copy fixed workspaces from curren context
    let mergedContext = mergeContexts fixedWs oldContext newContext

    -- let Context windowSet workspaceNames = mergeContexts
    let Context windowSet workspaceNames = mergedContext

    windows $ const windowSet -- hide old windows and show windows from new context
    mapM_ (uncurry setWorkspaceName) workspaceNames


-- Returns a map that contains all workspaces
currentWorkspaceMap :: X WorkspaceNames
currentWorkspaceMap = do
    ws <- asks (workspaces . config) -- get list of Workspace tags

    -- helper function to load the current name of the workspace
    let f :: WorkspaceId -> X (WorkspaceId, String)
        f tag = do
            name <- getWorkspaceName tag :: X (Maybe String)
            return (tag, fromMaybe "" name)

    traverse f ws :: X WorkspaceNames -- traverse: Applies the functions and converts from [X (WorkspaceId, String)] to X [ (WorkspaceId, String) ]

-- return the default workspace map
defaultWorkspaces :: X WorkspaceNames
defaultWorkspaces = do
    ws <- asks (workspaces . config)
    return $ map (,"") ws -- set every name to ""

moveWindowToContext :: Read (Layout Window) => ContextName -> X Bool
moveWindowToContext = moveWindowToContextFixedWs []

-- switch to new context while taking the current active window with you
moveWindowToContextFixedWs :: Read (Layout Window) => [WorkspaceId] -> ContextName -> X Bool
moveWindowToContextFixedWs fixedWs name = do
    ctxStorage <- XS.get :: X ContextStorage
    let (maybeNewCtx, ctxMap) = findAndDelete name (contextMap ctxStorage)
    case maybeNewCtx of
        Nothing     -> return False -- context not found
        Just newCtx -> do
            maybeWindow <- W.peek <$> gets windowset -- get current active window
            case maybeWindow of
                Nothing -> return False -- no active window found
                Just window -> do
                    xstate <- get
                    wsMap <- currentWorkspaceMap -- list of current workspaces

                    let newWindowSet = W.delete  window (windowset xstate)
                    let oldContext = Context newWindowSet wsMap -- current Context, including current names of workspaces
                    let ctxMap' = Map.insert oldContextName oldContext ctxMap -- store current context in map
                            where oldContextName = currentCtxName ctxStorage

                    XS.put $ ContextStorage name ctxMap' -- store changes

                    let newCtx' = Context newWindowSet newWorkspaceNames -- insert focused window in new context
                            where newWindowSet = W.insertUp window (windowSet newCtx)
                                  newWorkspaceNames = workspaceNames newCtx

                    setWindowsAndWorkspaces fixedWs oldContext newCtx' -- load new context

                    return True

-- Creates a new context if not already existant
createContext :: Read (Layout Window) => ContextName -> X ()
createContext name = do
    ctxStorage <- XS.get :: X ContextStorage
    when (not (null name)
          && name /= currentCtxName ctxStorage
          && name `Map.notMember` contextMap ctxStorage) $ do
        newWS' <- newWS
        defWs <- defaultWorkspaces
        let newCtx = Context newWS' defWs -- create new context with new workspace names
            newCtxMap = Map.insert name newCtx (contextMap ctxStorage)
        XS.put $ ctxStorage { contextMap = newCtxMap }

deleteContext :: Read (Layout Window) => ContextName -> X Bool
deleteContext name = do
    ctxStorage <- XS.get :: X ContextStorage
    let (maybeCtx, newCtxMap) = findAndDelete name (contextMap ctxStorage)
    case maybeCtx of
      Nothing  -> return False
      Just ctx -> do
          -- Kill all windows in that context
          let windows' = W.allWindows $ windowSet ctx
          for_ windows' killWindow
          XS.put $ ctxStorage { contextMap = newCtxMap }
          return True

showCurrentContextName :: Read (Layout Window) => X String
showCurrentContextName = do
    ctxStorage <- XS.get :: X ContextStorage
    return $ currentCtxName ctxStorage

listContextNames :: Read (Layout Window) => X [ContextName]
listContextNames = do
    ctxStorage <- XS.get :: X ContextStorage
    return $ Map.keys (contextMap ctxStorage)

newWS :: X WindowSet
newWS = withDisplay $ \dpy -> do
    xinesc <- getCleanedScreenInfo dpy
    xconf <- ask
    let conf = config xconf
        layout = layoutHook conf
        padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
        workspaces' = padToLen (length xinesc) (workspaces conf)
        sds = map SD xinesc
    return $ W.new layout workspaces' sds


findAndDelete :: ContextName -> ContextMap -> (Maybe Context, ContextMap)
findAndDelete = Map.updateLookupWithKey (\_ _ -> Nothing)

showContextStorage :: Read (Layout Window) => X ()
showContextStorage = do
    ctxStorage <- XS.get :: X ContextStorage
    io $ print ctxStorage
