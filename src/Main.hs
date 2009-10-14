module Main where

import Control.Concurrent (forkIO, killThread)
import Data.IORef
import Data.Version (showVersion)
import Happstack.Server
import Happstack.State
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.Exit

import Papps.Controller
import Papps.State
import Papps.Types
import Paths_Papps

main :: IO ()
main = do
  args  <- getArgs
  (o,_) <- getOpts args
  civ   <- newIORef defaultConfig
  doOpts civ o
  conf  <- readIORef civ

  styleDir <- getDataFileName $ "web" </> "styles"
  control <- startSystemState stateProxy
  httpTid <- 
    forkIO $ simpleHTTP (nullConf { port = (configPort conf) }) 
                $ controller styleDir

  waitForTermination
  
  killThread httpTid
  createCheckpoint control
  shutdownSystem control 

data Opts = Help
          | Version
          | Port   Int
       deriving Show

options :: [OptDescr Opts]
options = [ 
      Option ['h'] ["help"   ] (NoArg  Help           ) "This help"
    , Option ['v'] ["version"] (NoArg  Version        ) "Show version"
    , Option ['p'] ["port"   ] (ReqArg strToInt "PORT") "Port"
    ]
  where strToInt s = Port (read s :: Int)

getOpts :: [String] -> IO ([Opts], [String])
getOpts argv =
    case getOpt Permute options argv of
      (o,n,[])   -> return (o,n)
      (_,_,errs) -> error (concat errs ++ usage)

usage :: String
usage = (usageInfo header options)
    where header = "Usage: papps [OPTION...] \nOptions:"

info :: String
info = "papps " ++ (showVersion version) ++ " (C) 2009 Tim Horton "

mail :: String
mail = "<tmhorton@gmail.com>\n"

doOpts :: IORef Config -> [Opts] -> IO ()
doOpts _  [] = return ()
doOpts conf (o:oo) =
    case o of
      Help    -> putStr   usage >> exitWith ExitSuccess
      Version -> putStrLn info  >> exitWith ExitSuccess
      Port p  -> modifyIORef conf (\c -> c { configPort = p }) >> go
    where go = doOpts conf oo

