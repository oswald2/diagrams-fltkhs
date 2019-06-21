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
import           Diagrams.Backend.SVG
import           Diagrams.TwoD.Layout.Grid
import           Graphics.Svg.Core              ( renderBS )


aTree = unfoldTree (\n -> (0, replicate n (n - 1))) 6

example :: QDiagram SVG V2 Double Any
example = sunburst aTree # centerXY # pad 1.1




{-# INLINABLE withFlClip #-}
withFlClip :: FL.Rectangle -> IO a -> IO a
withFlClip rect action = do
    FL.flcPushClip rect
    a <- action
    FL.flcPopClip
    pure a

drawScene :: Ref Widget -> IO ()
drawScene widget = do
    rectangle' <- FL.getRectangle widget
    let coords@(x', y', w', h') = fromRectangle rectangle'
    withFlClip rectangle' $ do
        FL.flcSetColor whiteColor
        FL.flcRectf rectangle'
        let dia = renderDia
                SVG
                (SVGOptions (dims2D (fromIntegral w') (fromIntegral h'))
                            Nothing
                            ""
                            []
                            True
                )
                example
            bs = toStrict $ renderBS dia
        im <- FL.svgImageNew bs
        case im of
            Left  _     -> putStrLn ("drawScene: the generated SVG is invalid")
            Right image -> do
                FL.draw image (Position (X x') (Y y'))
                FL.destroy image


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

