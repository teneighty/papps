{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}
module Papps.State where

import Control.Monad.Reader 
import Control.Monad.State (modify)
import Data.List
import qualified Data.Map as M
import Happstack.Data
import Happstack.Server
import Happstack.State
import Happstack.State.ClockTime
import Text.Highlighting.Kate

data Comment = Comment {
    commentComment     :: String
  , commentCreatedDate :: ClockTime
  }
  deriving (Show, Ord, Eq, Typeable)
instance Version Comment
$(deriveSerialize ''Comment)


type PasteKey = String
data PasteEntry = PasteEntry { 
    pasteRaw         :: String
  , pasteLines       :: [SourceLine]
  , pasteLang        :: String
  , pasteURL         :: PasteKey
  , pasteComments    :: [Comment]
  , pasteCreatedDate :: ClockTime
  }
  deriving (Show, Ord, Eq, Typeable)
instance Version PasteEntry
$(deriveSerialize ''PasteEntry)

data AppState = AppState { 
    appPastes :: M.Map PasteKey PasteEntry  
  }
  deriving (Show, Ord, Eq, Typeable)
instance Version AppState
$(deriveSerialize ''AppState)

readPastes :: Query AppState [PasteEntry]
readPastes = liftM M.elems $ fmap appPastes ask
  
addPasteEntry :: PasteEntry -> Update AppState ()
addPasteEntry e = 
  modify $ \(AppState p) -> 
              (AppState {appPastes = M.insert (pasteURL e) e p})

getPasteEntry :: String -> Query AppState (Maybe PasteEntry)
getPasteEntry url = liftM (M.lookup url) $ fmap appPastes ask

addPasteComment :: String -> Comment -> Update AppState ()
addPasteComment k c = 
  modify $ 
    \(AppState p) -> 
      do let Just m = M.lookup k p
         AppState $ 
          M.update (\_ -> Just m { pasteComments = (c:(pasteComments m))}) k p

-- |top-level application component
-- we depend on the Pastes component
instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState { appPastes = M.empty } 
  
-- create types for event serialization
$(mkMethods ''AppState 
  [ 'readPastes
  , 'addPasteEntry
  , 'getPasteEntry
  , 'addPasteComment
  ])

instance FromData Comment where
  fromData = do
    c <- look "comments"
    return $ Comment { 
              commentComment = c
            , commentCreatedDate = defaultValue
            }

instance FromData PasteEntry where
    fromData = do
      vlang  <- look "lang"
      vcode  <- look "code"
      let code = if null vcode then "" else vcode
          l    = if null vlang then "" else vlang
          c    = filter ((/=) '\r') code
          lns = entryToLines l c
      return $ PasteEntry {
                  pasteRaw = c
                , pasteLines = lns
                , pasteLang = l 
                , pasteURL = defaultValue 
                , pasteComments = []
                , pasteCreatedDate = defaultValue 
                }

entryToLines :: String -> String -> [SourceLine]
entryToLines vlang vcode = 
  do case highlightAs vlang vcode of
       Right res -> res
       Left _err -> []

stateProxy :: Proxy AppState
stateProxy = Proxy

