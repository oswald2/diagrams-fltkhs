{-|
Module      : Diagrams.Backend.FLTKHS
Description : Provides a backend for diagrams library going via SVG
Copyright   : (c) Michael Oswald, 2019
License     : BSD-3
Maintainer  : michael.oswald@onikudaki.net
Stability   : experimental
Portability : POSIX

This is a backend to the diagrams library. Uses the SVG backend of diagrams to
create a SVG, which is then rendered with FLTK's SVG Image class.

Note that not all drawing operations are supported by FLTK. Notably text is not rendered
as well as some effects from SVG.

An example:

@
widget' <- widgetCustom
    (FL.Rectangle (Position (X 0) (Y 0)) (Size (Width width) (Height height)))
    Nothing
    drawScene
    defaultCustomWidgetFuncs
@

Here, 'drawScene' is the provided draw method for the widget. A possible implementation
could be this:

@
import Diagrams.TwoD.Sunburst
import Data.Tree (unfoldTree)

-- unfolding the tree
aTree = unfoldTree (\n -> (0, replicate n (n - 1))) 6

-- The diagram itself
example :: QDiagram SVG V2 Double Any
example = sunburst aTree # centerXY # pad 1.1

-- draw the diagram.
drawScene :: Ref Widget -> IO ()
drawScene widget = renderFltkhs widget example
@
-}
{-# LANGUAGE OverloadedStrings
#-}
module Diagrams.Backend.FLTKHS
    ( renderFltkhs
    , SVG
    -- | re-export from the SVG backend
    , B
    -- | re-export from the SVG backend
    )
where


import           Graphics.UI.FLTK.LowLevel.Fl_Types
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                                ( Widget )
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL
import           Data.ByteString.Lazy           ( toStrict )

import           Diagrams.Core.Compile
import           Diagrams.TwoD
import           Diagrams.Prelude
import           Diagrams.Backend.SVG
import           Graphics.Svg.Core              ( renderBS )



-- | Renders a SVG diagram to the given FLTK widget. Note that error handling
-- is quite rudimentary now, in case the SVG is invalid (which should not really
-- be the case when rendered from diagrams), nothing is rendered and a message
-- is put to console. This should probably be done better in the future
renderFltkhs :: (SVGFloat n) => Ref Widget -> QDiagram SVG V2 n Any -> IO ()
renderFltkhs widget diagram = do
    rectangle' <- FL.getRectangle widget
    let (x', y', w', h') = fromRectangle rectangle'
    FL.flcPushClip rectangle'

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
            diagram
        bs = toStrict $ renderBS dia
    im <- FL.svgImageNew bs
    case im of
        Left  _   -> putStrLn "renderFltkhs: the generated SVG is invalid"
        Right img -> do
            FL.draw img (Position (X x') (Y y'))
            FL.destroy img

    FL.flcPopClip
