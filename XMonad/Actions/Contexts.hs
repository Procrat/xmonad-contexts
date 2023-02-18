{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Actions.Contexts (
    createContext,
    switchContext,
    createAndSwitchContext,
    deleteContext,
    showCurrentContextName,
    listContextNames,
    moveWindowToContext,
    defaultContextName,
    showContextStorage
) where

import System.IO

import           Control.Monad               (when)
import           Data.Foldable               (for_)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.WorkspaceNames         (setWorkspaceName, getWorkspaceName)


type ContextName = String
type ContextMap = Map.Map ContextName Context

type WorkspaceNames = [(WorkspaceId, String)]

data Context = Context
    { windowSet          :: WindowSet
    , workspaceNames :: WorkspaceNames
    } deriving Show

deriving instance Read (Layout Window) => Read Context

data ContextStorage = ContextStorage
    { currentCtxName :: !ContextName
    , contextMap         :: !ContextMap
    } deriving Show

deriving instance Read (Layout Window) => Read ContextStorage

instance Read (Layout Window) => ExtensionClass ContextStorage where
    initialValue = ContextStorage defaultContextName Map.empty
    extensionType = PersistentExtension

defaultContextName :: ContextName
defaultContextName = "Main"

-------------------------------------------------------------------------------
switchContext :: Read (Layout Window) => ContextName -> X Bool
switchContext newContextName = do
    ctxStorage <- XS.get :: X ContextStorage
    let (maybeNewCtx, ctxMap) = findAndDelete newContextName (contextMap ctxStorage) -- get new
    case maybeNewCtx of
        Nothing     -> return False
        Just newCtx -> do
            xstate <- get
            wsMap <- currentWorkspaceMap -- list of current workspaces

            let ctxMap' = Map.insert oldContextName oldContext ctxMap -- store current context in map
                    where oldContext = Context (windowset xstate) wsMap -- current Context, including current names of workspaces
                          oldContextName = currentCtxName ctxStorage

            XS.put $ ContextStorage newContextName ctxMap' -- store context

            setWindowsAndWorkspaces newCtx

            return True

createAndSwitchContext :: Read (Layout Window) => ContextName -> X ()
createAndSwitchContext name = do
    createContext name
    _ <- switchContext name
    return ()


-- set the window set and apply the workspaceNames
setWindowsAndWorkspaces :: Context -> X ()
setWindowsAndWorkspaces ctx = do
    let Context windowSet workspaceNames = ctx
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


-- switch to new context while taking the current active window with you
moveWindowToContext :: Read (Layout Window) => ContextName -> X Bool
moveWindowToContext name = do
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

                    let ctxMap' = Map.insert oldContextName oldContext ctxMap -- store current context in map
                            where newWindowSet = W.delete  window (windowset xstate)
                                  oldContext = Context newWindowSet wsMap -- current Context, including current names of workspaces
                                  oldContextName = currentCtxName ctxStorage

                    XS.put $ ContextStorage name ctxMap' -- store changes

                    let newCtx' = Context newWindowSet newWorkspaceNames -- insert focused window in new context
                            where newWindowSet = W.insertUp window (windowSet newCtx)
                                  newWorkspaceNames = workspaceNames newCtx

                    setWindowsAndWorkspaces newCtx' -- load new context

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
