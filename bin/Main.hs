{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe (fromMaybe)
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Dhall hiding (void)
import Hakyll
import Network.HTTP.Req
import Options.Applicative hiding (auto)
import Text.Pandoc.Options

data Commands
  = New !String (Maybe String)
  | Serve
  | Update
  | Export !String

newtype DeckCfg = Cfg
  { cfgTitle :: String
  }
  deriving (Generic)

instance FromDhall DeckCfg

newParser :: Parser Commands
newParser =
  New
    <$> argument str (metavar "PATH")
    <*> optional (strOption (long "title" <> short 't' <> metavar "TITLE"))

serveParser :: Parser Commands
serveParser = pure Serve

updateParser :: Parser Commands
updateParser = pure Update

exportParser :: Parser Commands
exportParser = Export <$> argument str (metavar "PATH")

mainParser :: Parser Commands
mainParser =
  subparser $
    command "new" (info newParser (progDesc "generate a new Trex Deck at the given path"))
      <> command
        "serve"
        (info serveParser (progDesc "run the server and watch for changes"))
      <> command
        "update"
        (info updateParser (progDesc "Sync Trex's static files with the latest updtream versions"))
      <> command "export" (info exportParser (progDesc "export the presentation to the given location"))

pandocWithMath :: Compiler (Item String)
pandocWithMath =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          { writerExtensions = extents
          , writerHTMLMathMethod = MathJax ""
          }
   in pandocCompilerWith defaultHakyllReaderOptions wopts

site :: DeckCfg -> Rules ()
site c = do
  match "css/*.css" $ do
    route idRoute
    compile copyFileCompiler
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler
  match "slides/*.md" $ do
    compile pandocWithMath
  match "deck.html" $ do
    route idRoute
    compile $ do
      let context = listField "slides" defaultContext (loadAll "slides/*.md")
      getResourceBody >>= applyAsTemplate (context <> constField "title" (cfgTitle c))

getFile :: FilePath -> [T.Text] -> IO ()
getFile path rfile =
  runReq defaultHttpConfig $ do
    let baseUrl =
          https "raw.githubusercontent.com"
            /: "pittma"
            /: "trex"
            /: "refs"
            /: "heads"
            /: "main"
        url = foldl' (/:) baseUrl rfile
    r <- req GET url NoReqBody lbsResponse mempty
    liftIO $ LBS.writeFile path (responseBody r)

genNewDeck :: String -> String -> IO ()
genNewDeck ps title = do
  basep <- makeAbsolute ps
  createDirectoryIfMissing True basep
  createDirectoryIfMissing True (basep </> "css")
  createDirectoryIfMissing True (basep </> "images")
  createDirectoryIfMissing True (basep </> "js")
  createDirectoryIfMissing True (basep </> "slides")
  withFile (basep </> "cfg.dhall") WriteMode $ \h -> do
    hPutStrLn h ("{cfgTitle = \"" ++ title ++ "\"}")
  getFile (basep </> "css" </> "trex.css") ["css", "trex.css"]
  getFile (basep </> "css" </> "solarized.css") ["css", "solarized.css"]
  getFile (basep </> "css" </> "overrides.css") ["css", "overrides.css"]
  getFile (basep </> "deck.html") ["deck.html"]

syncUpstream :: IO ()
syncUpstream = do
  getFile ("css" </> "trex.css") ["css", "trex.css"]
  getFile ("css" </> "solarized.css") ["css", "solarized.css"]
  getFile "deck.html" ["deck.html"]

main :: IO ()
main = do
  cmd <- execParser (info (mainParser <**> helper) fullDesc)
  case cmd of
    New path title -> genNewDeck path (fromMaybe path title)
    Update -> syncUpstream
    Serve -> void $ do
      cfg <- input auto "./cfg.dhall" :: IO DeckCfg
      hakyllWithExitCodeAndArgs
        defaultConfiguration
        (Options False (Watch "localhost" 8000 False))
        (site cfg)
    Export _ -> putStrLn "not implemented yet (not sure it makes sense at all tbh)"
