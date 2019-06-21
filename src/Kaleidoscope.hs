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
import           Graphics.Svg.Core



iterateIdx :: (Int -> a -> a) -> a -> [a]
iterateIdx f t = go f t 0
    where go f t i = let t' = f i t in t' : go f t' (i + 1)



kaleidoscope d = appends hex hexs
  where
    hexs = zip dirs (replicate 6 hex)
    dirs = iterate (rotateBy (1 / 6)) (rotateBy (1 / 12) unitX)
    hex  = mconcat . take 6 $ iterateIdx next tri
    tri  = alignBR $ cutTriangle d
    next i = reflectAbout (0 ^& 0) (rotateBy (-fromIntegral i / 6) xDir)


--cutTriangle :: Diagram B -> Diagram B
cutTriangle = clipped (triangle 1) # lw none


--confettiScope :: Int -> Int -> Diagram B
confettiScope n r =
    kaleidoscope (mkConfetti n (mkStdGen r))
        #  centerXY
        <> (circle 2.75 # fc black)
        #  pad 1.1


sizeValue :: (RandomGen g) => Rand g Double
sizeValue = getRandomR (0.05, 0.25)


coordValue :: (RandomGen g) => Rand g Double
coordValue = getRandomR (-0.5, 0.5)


-- confetti :: Int -> Rand StdGen (Diagram B)
confetti n = do
    ss <- replicateM n sizeValue   -- radius
    cs <- replicateM n getRandom   -- color index
    as <- replicateM n getRandom   -- opacity
    xs <- replicateM n coordValue  -- x coordinate
    ys <- replicateM n coordValue  -- y coordinate
    let --mkCirc :: Double -> Int -> Double -> Diagram B
        mkCirc s c a = circle s # fc (webColors c) # lw none # opacity a
        pos  = zipWith mkP2 xs ys
        conf = zipWith3 mkCirc ss cs as
    return $ position (zip pos conf)

--mkConfetti :: Int -> (StdGen -> Diagram B)
mkConfetti n = evalRand $ confetti n


--example :: QDiagram SVG V2 Int Any
example = confettiScope 39 0


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

