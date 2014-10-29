{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Graphics(Map (MakeMap), generatePngMap, generateGifMap) where

import Codec.Picture
import Data.Word
import Data.Maybe
import qualified Data.Map.Strict as M
import Foreign.C.Types

data Map = MakeMap { mapWidth :: Int, mapHeight :: Int, mapCells :: M.Map (Int,Int) CFloat }

getElevation :: Map -> Int -> Int -> CFloat
getElevation map x y = fromJust $ M.lookup (x,y) (mapCells map)

-- np = no palette
-- p  = palette

npAltitudeColor :: CFloat -> PixelRGB8
npAltitudeColor elev = let f = elev
                           isSea = elev < 0.5
                           comp = round( f*255.0 )
                       in if isSea then PixelRGB8 0 0 comp else PixelRGB8 0 comp 0

pAltitudeColor :: CFloat -> Pixel8
pAltitudeColor elev = let f = elev
                          isSea = elev < 0.5
                          comp = round( (f/5.0)*127.0 )
                       in if isSea then min comp 127 else comp+127

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

generateMap :: forall p . Pixel p => Map -> (CFloat -> p) -> Image p
generateMap map pixelFun = generateImage f w h
                           where w = mapWidth map
                                 h = mapHeight map
                                 f x y = altColor
                                         where altColor = pixelFun $ getElevation map x y

generatePngMap map = generateMap map npAltitudeColor

generateGifMap map = generateMap map pAltitudeColor