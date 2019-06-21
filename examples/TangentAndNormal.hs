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
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations
                                               as FL
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


pts = map p2 [(0, 0), (1, 1), (2, 1), (3, 0), (3.5, 0)]

spline :: Located (Trail V2 Double)
spline = cubicSpline False pts

param = 0.45 -- parameter on the curve where the tangent and normal are drawn
pt = atParam spline param
tangentVector = tangentAtParam spline param
normalVector = normalAtParam spline param


symmetricLine v = fromOffsets [2 *^ v] # center
tangentLine = symmetricLine tangentVector
normalLine = symmetricLine normalVector

rightAngleSquare =
    square 0.1 # alignBL # rotate (signedAngleBetween tangentVector unitX)


example :: QDiagram SVG V2 Double Any
example =
    frame 0.5
        $  strokeLocTrail spline
        <> mconcat
               [ tangentLine
               , baselineText "tangent" # translate tangentVector
               , normalLine
               , topLeftText "normal" # translate normalVector
               , rightAngleSquare
               ]
        #  moveTo pt
        #  fontSize large


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

