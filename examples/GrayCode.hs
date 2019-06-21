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
                                               as FL
import           Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import           Graphics.UI.FLTK.LowLevel.FLTKHS
                                                ( Widget )
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS
                                               as FL

import           Data.Colour.Palette.ColorSet

import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( catMaybes )
import           Control.Applicative
import           Data.Monoid                    ( mconcat )
import           Data.List                      ( transpose )

import           Diagrams.Core.Compile
import           Diagrams.TwoD
import           Diagrams.TwoD.Sunburst
import           Diagrams.Prelude        hiding ( gray )
import           Diagrams.Backend.FLTKHS
import           Diagrams.TwoD.Layout.Grid


gray 0 = [[]]
gray n = map (False :) g ++ map (True :) (reverse g) where g = gray (n - 1)

rings n = mkRingsDia . map ringOffsets . transpose . gray $ n
  where
    ringOffsets :: [Bool] -> [(Direction V2 Double, Diagrams.Prelude.Angle Double)]
    ringOffsets =
        map l2t
            . chunksOf 2
            . findEdges
            . zip
                  [ rotate α xDir
                  | α <- [0 @@ turn, 1 / (2 ^ n) @@ turn .. fullTurn]
                  ]
    l2t [x, y] = (x, angleBetweenDirs x y)
    l2t [x]    = (x, angleBetweenDirs x xDir) -- arc angle will never be > fullturn ^/ 2

findEdges :: Eq a => [(Direction V2 Double, a)] -> [Direction V2 Double]
findEdges = catMaybes . (zipWith edge <*> tail)
  where
    edge (_, c1) (a, c2) | c1 /= c2  = Just a
                         | otherwise = Nothing


mkRingsDia = mconcat . zipWith mkRingDia [2, 3 ..]
  where
    mkRingDia r = lwL 1.05 . mconcat . map (strokeP . scale r . uncurry arc)

example :: QDiagram SVG V2 Double Any
example = pad 1.1 (rings 10)



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

