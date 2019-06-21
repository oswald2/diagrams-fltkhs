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
--import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                                ( Widget )
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL

import           Data.Colour.Palette.ColorSet
import           Data.Colour.SRGB               ( sRGB24read )
import           Data.List                      ( zipWith
                                                , zipWith3
                                                )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Tree                      ( unfoldTree )

import           Diagrams.Core.Compile
import           Diagrams.Prelude        hiding ( tri )
import           Diagrams.Backend.FLTKHS
import           Diagrams.TwoD.Layout.Grid


fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

grid :: Int -> Int -> Diagram B
grid x y = frame <> lattice
  where
    s       = unitSquare # lw thin
    frame   = rect (fromIntegral x) (fromIntegral y) # lw thick
    lattice = centerXY . vcat . map hcat . replicate y . replicate x $ s

trap, tri :: Double -> Double -> Diagram B
trap s1 s2 = lw none . strokeLoop . closeLine
           . fromOffsets . map r2 $ [(0,-s2), (s2,0), (0,s1)]
tri s1 s2  = lw none .  strokeLoop . closeLine
           . fromOffsets . map r2 $ [(s1,0), (0,s1+s2)]

paradox :: Int -> Bool -> Diagram B
paradox n drawDiags = (sq # rotateBy (1/4)
                   ||| strutX (s2 / 2)
                   ||| rect # rotateBy (1/4)) # centerXY
  where f1 = fibs !! n
        f2 = fibs !! (n+1)
        s1 = fromIntegral f1
        s2 = fromIntegral f2

        trap1 = trap s1 s2 # fc (sRGB24read "#BEC3C7")
        trap2 = trap s1 s2 # fc (sRGB24read "#1ABC9C")
                           # rotateBy (1/2)

        tri1  = tri s1 s2  # fc (sRGB24read "#FF6666")
        tri2  = tri s1 s2  # fc (sRGB24read "#37485D")
        sq = (if drawDiags then sqDiags else mempty)
             <> grid (f1+f2) (f1+f2)
             <> sqShapes
        sqDiags = (fromVertices [p2 (0,s2), p2 (s2,s1)] <>
                   fromVertices [p2 (s2,0), p2 (s2,s1+s2)] <>
                   fromVertices [p2 (s2,0), p2 (s1+s2,s1+s2)])
                # strokeP
                # lw thick
                # centerXY

        sqShapes = (traps # centerY ||| tris # centerY)
                 # centerXY
        traps = trap2 # alignL
                      # translateY (s1 - s2)
             <> trap1
        tris  = tri1 # alignBL
             <> tri2 # rotateBy (1/2)
                     # alignBL
        rect = (if drawDiags then rDiags else mempty)
               <> grid (2*f2 + f1) f2
               <> rShapes

        rShapes = (bot # alignTL <> top # alignTL) # centerXY
        bot = trap1 # alignB ||| rotateBy (-1/4) tri1 # alignB
        top = rotateBy (1/4) tri2 # alignT ||| trap2 # alignT

        rDiags = (fromVertices [p2 (0,s2), p2 (2*s2+s1, 0)] <>
                  fromVertices [p2 (s2,0), p2 (s2,s1)] <>
                  fromVertices [p2 (s1+s2,s2-s1), p2 (s1+s2,s2)]
                  )
                 # strokeP
                 # lw thick
                 # lineCap LineCapRound
                 # centerXY



example :: QDiagram SVG V2 Double Any
example = paradox 4 True # frame 0.5

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

