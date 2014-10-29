{-# LANGUAGE DeriveDataTypeable #-}

module Graphics(Map (MakeMap), generateMap) where

import Codec.Picture
import Data.Word
import Data.Maybe
import qualified Data.Map.Strict as M
import Foreign.C.Types

data Map = MakeMap { mapWidth :: Int, mapHeight :: Int, mapCells :: M.Map (Int,Int) CFloat }

getElevation :: Map -> Int -> Int -> CFloat
getElevation map x y = fromJust $ M.lookup (x,y) (mapCells map)

altitudeColor :: CFloat -> PixelRGB8
altitudeColor elev = let f = elev
                         isSea = elev < 0.5
                         comp = round( f*255.0 )
                     in if isSea then PixelRGB8 0 0 comp else PixelRGB8 0 comp 0

mix :: Word8 -> Word8 -> CFloat -> CFloat -> Word8
mix c1 c2 f1 f2 = let comp1 =  (fromIntegral c1) * f1
                      comp2 =  (fromIntegral c2) * f2
                  in round( comp1 + comp2 )

mixColors :: PixelRGB8 -> PixelRGB8 ->  CFloat -> PixelRGB8
mixColors c1 c2 f = let PixelRGB8 r1 g1 b1 = c1
                        PixelRGB8 r2 g2 b2 = c2
                        fi = 1.0 - f
                        r = mix r1 r2 f fi
                        g = mix g1 g2 f fi                        
                        b = mix b1 b2 f fi
                    in PixelRGB8 r g b

generateMap :: Map -> Image PixelRGB8
generateMap map = generateImage f w h
                    where w = mapWidth map
                          h = mapHeight map
                          f x y = altColor
                                  where altColor = altitudeColor $ getElevation map x y