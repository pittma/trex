{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as LBS
import Hakyll
import Network.HTTP.Req
import Options.Applicative
import Text.Pandoc.Options

data Commands
  = New !String
  | Serve

newParser :: Parser Commands
newParser = New <$> argument str (metavar "PATH")

serveParser :: Parser Commands
serveParser = pure Serve

mainParser :: Parser Commands
mainParser =
  subparser
    $ command "new" (info newParser (progDesc "generate a new Trex Deck at the given path"))
        <> command
             "serve"
             (info serveParser (progDesc "run the server and watch for changes"))

pandocWithMath :: Compiler (Item String)
pandocWithMath =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          {writerExtensions = extents, writerHTMLMathMethod = MathJax ""}
   in pandocCompilerWith defaultHakyllReaderOptions wopts 

site :: Rules ()
site = do
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
      getResourceBody >>= applyAsTemplate context

genNewDeck :: String -> IO ()
genNewDeck ps = do
  basep <- makeAbsolute ps
  createDirectoryIfMissing True basep
  createDirectoryIfMissing True (basep </> "css")
  createDirectoryIfMissing True (basep </> "images")
  createDirectoryIfMissing True (basep </> "js")
  createDirectoryIfMissing True (basep </> "slides")
  getFile (basep </> "css" </> "trex.css") ["css", "trex.css"]
  getFile (basep </> "css" </> "solarized.css") ["css", "solarized.css"]
  getFile (basep </> "css" </> "overrides.css") ["css", "overrides.css"]
  getFile (basep </> "deck.html") ["deck.html"]
  where
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

main :: IO ()
main = do
  cmd <- execParser (info (mainParser <**> helper) fullDesc)
  case cmd of
    New path -> genNewDeck path
    Serve -> void $ do
      hakyllWithExitCodeAndArgs
        defaultConfiguration
        (Options False (Watch "localhost" 8000 False))
        site
