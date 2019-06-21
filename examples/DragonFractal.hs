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
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                                ( Widget )
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations
                                               as FL

import           Data.Colour.Palette.ColorSet
import           Data.List                      ( zipWith
                                                , zipWith3
                                                )
import           Data.ByteString.Lazy           ( toStrict )

import           Diagrams.Core.Compile
import           Diagrams.TwoD
import           Diagrams.Prelude
import           Diagrams.Backend.FLTKHS
import           Diagrams.TwoD.Layout.Grid
import           Graphics.Svg.Core              ( renderBS )


nextDragon trail =
    (trail # rotateBy (-1 / 8) <> trail # rotateBy (5 / 8) # reverseTrail)
        # scale (1 / sqrt 2)

dragonCurves = map (trailLike . (`at` origin))
                   (iterate nextDragon initialTrail)
    where initialTrail = hrule 1

withPrevious diagrams = zipWith (<>) diagrams (mempty : diagrams # opacity 0.2)


rememberOrder = zipWith named [0 :: Int ..]

showOrder diagram = diagram
    # applyAll (map addArrow [0 .. length (names diagram)])
  where
    addArrow n = connectOutside' opts n (n + 1)
    opts = with & gaps .~ normalized 0.005 & headLength .~ tiny


example :: QDiagram SVG V2 Double Any
example =
    dragonCurves
        # withPrevious
        # take 12
        # sameBoundingRect
        # rememberOrder
        # map (frame 0.1)
        # gridSnake
        # showOrder
        # lw ultraThin


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

