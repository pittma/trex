{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import Text.Pandoc.Options

pandocWithMath :: Compiler (Item String)
pandocWithMath =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          {writerExtensions = extents, writerHTMLMathMethod = MathJax ""}
   in pandocCompilerWith defaultHakyllReaderOptions wopts 

main :: IO ()
main =
  hakyll $ do
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
    match "index.html" $ do
      route idRoute
      compile $ do
        let context = listField "slides" defaultContext (loadAll "slides/*.md")
        getResourceBody >>= applyAsTemplate context
