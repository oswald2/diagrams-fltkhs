{-# LANGUAGE OverloadedStrings
#-}
module Diagrams.Backend.FLTKHS
(
    renderFltkhs
    , SVG
    , B
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
        Left  _     -> putStrLn "renderFltkhs: the generated SVG is invalid"
        Right img -> do
            FL.draw img (Position (X x') (Y y'))
            FL.destroy img

    FL.flcPopClip