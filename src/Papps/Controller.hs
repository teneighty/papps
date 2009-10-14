module Papps.Controller where

import Control.Monad.Reader
import Data.List (find, sortBy)
import Happstack.Server
import Happstack.State
import System.FilePath
import System.Time (getClockTime)
import System.Random

import Papps.Views
import Papps.State
import Papps.Utils

controller :: FilePath -> ServerPartT IO Response
controller styleDir = 
    msum [
      dir "styles"      (fileServe [] styleDir)
    , dir "addPaste"    paste
    , dir "recent"      recent
    , dir "paste"       $ homePage Nothing
    , catchOthers
    ]

catchOthers :: ServerPartT IO Response
catchOthers = do
    rq <- askRq
    handlePaste $ rqPaths rq

homePage :: Maybe PasteEntry -> ServerPartT IO Response
homePage f = do
  rq <- askRq
  let l = prefLang rq
  homeView l f

-- | TODO: figure out why Happstack.Server readCookieValue
--         did not work.
prefLang :: Request -> String
prefLang rq = 
  case (find (\(x, _) -> x == langCookieName) (rqCookies rq)) of
    Just (_, c) -> cookieValue c
    Nothing     -> "nada"

handlePaste :: [String] -> ServerPartT IO Response
handlePaste [] = homePage Nothing
handlePaste (r : rs) = 
  do if r `elem` ["", "/"] 
       then homePage Nothing
       else do resp <- query $ GetPasteEntry r
               case resp of
                 Just f  -> handlePaste' f rs
                 Nothing -> pasteError badUrl 
  where
    handlePaste' f [] = pasteView f
    handlePaste' f (u : _rs) = do
      case u of 
        "addComment" -> do Just c <- getData
                           n <- liftIO getClockTime
                           update $ AddPasteComment 
                                     (pasteURL f) c {commentCreatedDate = n}
                           seeOther (pasteURL f) $ toResponse ()
        "raw"        -> ok $ plainText $ raw2resp f
        "dl"         -> ok $ plainText $ (attachment f) $ raw2resp f
        "fork"       -> homePage $ Just f
        _            -> pasteView f
    mkFileName f = concat [ "papps-"
                          , (pasteURL f)
                          , (languageFirstExt (pasteLang f))]
    plainText    = setHeader "Content-Type" "text/plain"
    raw2resp     = (toResponse . pasteRaw)
    attachment f = setHeader 
                    "Content-Disposition" 
                    ("attachment; filename=\"" ++ mkFileName f ++ "\" ")

paste :: ServerPartT IO Response
paste = do
  Just pdata <- getData
  now <- liftIO getClockTime
  url <- liftIO generateURL
  addCookie (-1) $ mkCookie langCookieName $ pasteLang pdata
  if length (pasteRaw pdata) <= 0 
    then pasteError noCode 
    else do update $ 
              AddPasteEntry pdata { pasteCreatedDate = now, pasteURL = url }
            seeOther url $ toResponse ()

comment :: ServerPartT IO Response
comment = do
  rq <- askRq
  let _url = rqURL rq
  pasteError fixComments

-- TODO: this is pretty weak. create a better url
generateURL :: IO String
generateURL = liftM format (randomIO :: IO Integer)
  where format = (show . abs)

recent :: ServerPartT IO Response
recent = do
     pastes  <- query ReadPastes
     recentView $ take 20 $ reverse $ sortBy s pastes
  where s a b = if pasteCreatedDate a < pasteCreatedDate b 
                  then LT
                  else GT
    

langCookieName :: String
langCookieName = "lang"
