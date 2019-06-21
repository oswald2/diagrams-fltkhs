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


grad = defaultRG & _RG . rGradStops .~ mkStops [(blue,0,1), (crimson,1,1)]
                 & _RG . rGradRadius1 .~ 50
pentaflake' 0 = regPoly 5 1 # lw none

pentaflake' n = appends
                  pCenter
                  (zip vs (repeat (rotateBy (1/2) pOutside)))
  where vs = iterateN 5 (rotateBy (1/5))
           . (if odd n then negated else id)
           $ unitY
        pCenter  = pentaflake' (n-1)
        pOutside = pCenter # opacity (1.7 / fromIntegral n)

pentaflake n = pentaflake' n # fillTexture grad # bgFrame 4 silver

example :: QDiagram SVG V2 Double Any
example = pentaflake 4


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

