{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Foreign
import Foreign.C.Types
import Data.Either
import qualified Foreign.Marshal.Array as FMA
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Graphics as G
import Codec.Picture
import Codec.Picture.Gif

---------------------------------------------------
-- Bindings
---------------------------------------------------

foreign import ccall "srand"
    c_srand :: CUInt -> IO ()

-- | size_t map_side
-- | float sea_level
-- | size_t erosion_period
-- | float folding_ratio
-- | size_t aggr_overlap_abs
-- | float aggr_overlap_rel
-- | size_t cycle_count
-- | size_t num_plates
foreign import ccall "platec_api_create"
     c_platec_create :: CSize -> CFloat -> CSize -> CFloat -> CSize -> CFloat -> CSize -> CSize -> IO (Ptr ())

foreign import ccall "platec_api_get_heightmap"
    c_platec_api_get_heightmap :: Ptr () -> Ptr CFloat;

foreign import ccall "platec_api_step"
    c_platec_api_step :: Ptr () -> IO ()

foreign import ccall "platec_api_is_finished"
    c_platec_api_is_finished :: Ptr () -> CSize

---------------------------------------------------
-- Haskell functions
---------------------------------------------------

toMap :: Int -> [CFloat] -> M.Map (Int,Int) CFloat
toMap width list = helper list (fromIntegral 0) (fromIntegral 0) M.empty
                   where helper :: [CFloat] -> Int -> Int -> M.Map (Int,Int) CFloat -> M.Map (Int,Int) CFloat
                         helper [] x y map = map
                         helper (e:es) x y map = helper es x' y' (M.insert (x,y) e map)
                                                 where x' = (x + 1) `mod` width
                                                       y' = if x'==0 then y+1 else y

saveMap p filename = do let heightmap = c_platec_api_get_heightmap p
                        heightmap' :: [CFloat] <- FMA.peekArray (512*512) heightmap
                        let heightmap'' = G.MakeMap 512 512 (toMap 512 heightmap')
                        let img = G.generateGifMap heightmap''
                        writeGifImage filename img

getFrame p = do
    let heightmap = c_platec_api_get_heightmap p
    heightmap' :: [CFloat] <- FMA.peekArray (512*512) heightmap
    let heightmap'' = G.MakeMap 512 512 (toMap 512 heightmap')
    let img = G.generateGifMap heightmap''
    return img

writeAnimation filename frames = do writeGifImages filename LoopingForever frames'
                                    where frames' = L.map (\f -> (greyPalette, 20, f)) frames

getFrames p = helper p [] 0
              where helper :: Ptr () -> [Image Pixel8] -> Int -> IO [Image Pixel8]
                    helper p acc i = if (c_platec_api_is_finished p) /= 0 || (length acc > 200)
                                     then return acc
                                     else do
                                        c_platec_api_step p
                                        acc' <- if i == 0
                                                then do
                                                    frame :: Image Pixel8 <- getFrame p
                                                    putStrLn $ "Getting frame " ++ (show $ length acc)
                                                    return $ acc ++ [frame]
                                                else return acc
                                        helper p acc' ((i+1) `mod` 10)

processRes (Data.Either.Left msg) = do
    putStrLn "Error while producing the animation:"
    putStrLn msg

processRes (Right res) = do
    res
    putStrLn "DONE"

main = do
    c_srand 1
    p <- c_platec_create 512 0.65 60 0.02 1000000 0.33 2 10
    frames <- getFrames p
    putStrLn "Frames produced, saving animation (it could take a while)"
    let res = writeAnimation "animation4.gif" frames
    processRes res
    return ()