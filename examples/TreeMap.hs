{-# LANGUAGE OverloadedStrings
  , NoMonomorphismRestriction
  , FlexibleContexts
  , TypeFamilies
  , GADTs
#-}
module Main where


import           Control.Monad
import           Control.Monad.Random

import qualified Graphics.UI.FLTK.LowLevel.FL  as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                                ( Widget )
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL

import           Data.Colour.Palette.ColorSet
import           Data.List                      ( zipWith
                                                , zipWith3
                                                )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Tree                      ( unfoldTree )

import           Diagrams.Core.Compile
import           Diagrams.TwoD
import           Diagrams.TwoD.Sunburst
import           Diagrams.Prelude
import           Diagrams.Backend.FLTKHS
import           Diagrams.TwoD.Layout.Grid


aTree = unfoldTree (\n -> (0, replicate n (n - 1))) 6

example :: QDiagram SVG V2 Double Any
example = sunburst aTree # centerXY # pad 1.1


drawScene :: Ref Widget -> IO ()
drawScene widget = renderFltkhs widget example

main :: IO ()
main = do
    let width  = 800
        height = 600

    window' <- FL.doubleWindowNew (Size (Width width) (Height height))
                                  Nothing
                                  Nothing
    FL.begin window'
    widget' <- FL.widgetCustom
        (FL.Rectangle (Position (X 0) (Y 0))
                      (Size (Width width) (Height height))
        )
        Nothing
        drawScene
        FL.defaultCustomWidgetFuncs
    FL.end window'
    FL.showWidget window'
    void FL.run

