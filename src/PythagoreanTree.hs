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

import           Diagrams.Core.Compile
import           Diagrams.TwoD
import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Diagrams.TwoD.Layout.Grid
import           Graphics.Svg.Core              ( renderBS )


stops = mkStops [(saddlebrown, 0, 1), (peru, 0.5, 1), (saddlebrown, 1, 1)]
b = mkLinearGradient stops ((-0.5) ^& 0) (0.5 ^& 0) GradPad
stops' = mkStops [(green, 0, 1), (lightgreen, 1, 1)]
g = mkRadialGradient stops' (0.0 ^& 0) 0.5 (0.0 ^& 0.0) 1 GradPad
sky = mkLinearGradient (mkStops [(darkgreen,0,1), (white,0.1,1), (skyblue,1,1)])
                       (0 ^& (-2.5)) (0 ^& 3) GradPad
tree 1 = circle 1.25 # fillTexture g
                    # translate (r2 (0, 1/2)) # lwG 0
tree n =
  square 1          # translate (r2 (0, 1/2)) # fillTexture b
                    # lineTexture b # lw thin
  `atop` triangle   # translate (r2 (0,1))    # fillTexture b # lwG 0
  `atop` tree (n-1) # rotate (-asin 0.8 @@ rad)
                    # scale 0.6 # translate (r2 ( 0.32,1.24)) # fade
  `atop` tree (n-1) # rotate ( asin 0.6 @@ rad)
                    # scale 0.8 # translate (r2 (-0.18,1.24)) # fade
  where
    triangle = translate (r2 (-0.5,0)) . strokeLoop . closeLine
                 . fromVertices . map p2 $ [(0,0), (1,0), (0.8*0.8,0.8*0.6)]
    fade = opacity 0.95

colourise c = fc c . lc (blend 0.5 black c)

example :: QDiagram SVG V2 Double Any
example = (tree 10 === square 1 # fillTexture b
                     # lineTexture b # lw thin) # center
                    <> (square 6.25 # fillTexture sky # lw none )


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

