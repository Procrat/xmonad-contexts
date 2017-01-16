module XMonad.Actions.Contexts (
    createContext,
    switchContext,
    createAndSwitchContext,
    listContextNames,
    defaultContextName,
    showContextStorage
) where


import           Control.Monad
import qualified Data.Map.Strict             as Map

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS


type ContextName = String
type ContextMap = Map.Map ContextName Context

newtype Context = Context { contextWS :: WindowSet }
  deriving Show

data ContextStorage = ContextStorage { currentContextName :: ContextName
                                     , contextMap         :: ContextMap
                                     }
  deriving Show

instance ExtensionClass ContextStorage where
  initialValue = ContextStorage defaultContextName Map.empty

defaultContextName :: ContextName
defaultContextName = "default"

-------------------------------------------------------------------------------
switchContext :: ContextName -> X Bool
switchContext name = do
  contextStorage <- XS.get :: X ContextStorage
  let findAndDelete = Map.updateLookupWithKey (\_ _ -> Nothing)
  let (maybeNewContext, newContextMap) = findAndDelete name (contextMap contextStorage)
  case maybeNewContext of
    Just newContext -> do
      xstate <- get
      let currentContext = Context (windowset xstate)
          newContextMap' :: ContextMap
          newContextMap' = Map.insert (currentContextName contextStorage) currentContext newContextMap
      XS.put $ ContextStorage name newContextMap'
      windows (const $ contextWS newContext)
      return True
    Nothing -> return False

createAndSwitchContext :: ContextName -> X ()
createAndSwitchContext name = do
  createContext name
  switchContext name
  return ()

createContext :: ContextName -> X ()
createContext name = do
  contextStorage <- XS.get :: X ContextStorage
  when (name `Map.notMember` contextMap contextStorage) $ do
    newWS' <- newWS
    let newContext = Context newWS'
        newContextMap = Map.insert name newContext (contextMap contextStorage)
    XS.put $ contextStorage { contextMap = newContextMap }

listContextNames :: X [ContextName]
listContextNames = do
  contextStorage <- XS.get :: X ContextStorage
  return $ Map.keys (contextMap contextStorage)

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

showContextStorage :: X ()
showContextStorage = do
  contextStorage <- XS.get :: X ContextStorage
  liftIO $ print contextStorage
