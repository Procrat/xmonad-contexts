module XMonad.Actions.Contexts (
    createContext,
    switchContext,
    createAndSwitchContext,
    deleteContext,
    listContextNames,
    defaultContextName,
    showContextStorage
) where


import           Control.Monad               (when)
import qualified Data.Map.Strict             as Map
import           Data.Traversable            (for)

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS


type ContextName = String
type ContextMap = Map.Map ContextName Context

newtype Context = Context
    { ctxWS :: WindowSet
    } deriving Show

data ContextStorage = ContextStorage
    { currentCtxName :: !ContextName
    , ctxMap         :: !ContextMap
    } deriving Show

instance ExtensionClass ContextStorage where
  initialValue = ContextStorage defaultContextName Map.empty

defaultContextName :: ContextName
defaultContextName = "default"

-------------------------------------------------------------------------------
switchContext :: ContextName -> X Bool
switchContext name = do
    ctxStorage <- XS.get :: X ContextStorage
    let (maybeNewCtx, newCtxMap) = findAndDelete name (ctxMap ctxStorage)
    case maybeNewCtx of
        Nothing     -> return False
        Just newCtx -> do
            xstate <- get
            let currentCtx = Context (windowset xstate)
                newCtxMap' = Map.insert (currentCtxName ctxStorage) currentCtx newCtxMap
            XS.put $ ContextStorage name newCtxMap'
            windows (const $ ctxWS newCtx)
            return True

createAndSwitchContext :: ContextName -> X ()
createAndSwitchContext name = do
    createContext name
    switchContext name
    return ()

createContext :: ContextName -> X ()
createContext name = do
    ctxStorage <- XS.get :: X ContextStorage
    when (name `Map.notMember` ctxMap ctxStorage) $ do
        newWS' <- newWS
        let newCtx = Context newWS'
            newCtxMap = Map.insert name newCtx (ctxMap ctxStorage)
        XS.put $ ctxStorage { ctxMap = newCtxMap }

deleteContext :: ContextName -> X Bool
deleteContext name = do
    ctxStorage <- XS.get :: X ContextStorage
    let (maybeCtx, newCtxMap) = findAndDelete name (ctxMap ctxStorage)
    case maybeCtx of
      Nothing  -> return False
      Just ctx -> do
          -- Kill all windows in that context
          let windows' = W.allWindows $ ctxWS ctx
          withDisplay $ \dpy -> for windows' (io . killClient dpy)
          XS.put $ ctxStorage { ctxMap = newCtxMap }
          return True

listContextNames :: X [ContextName]
listContextNames = do
    ctxStorage <- XS.get :: X ContextStorage
    return $ Map.keys (ctxMap ctxStorage)

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

showContextStorage :: X ()
showContextStorage = do
    ctxStorage <- XS.get :: X ContextStorage
    io $ print ctxStorage
