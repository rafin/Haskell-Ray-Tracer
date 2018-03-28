{-# LANGUAGE FlexibleContexts #-}

module Renderer where

import RayTracer
import Vec3

import Control.Applicative
import qualified Graphics.Image as G
import Data.Vector.Storable (fromList)


data Rendering = Rendering {rImage :: [[Color]], rWidth :: Int, rHeight :: Int}
    deriving (Show, Eq)


doRender :: World -> View -> Rendering
doRender w (View vw vh vz vx) = Rendering ri vw vh
    where
        xi = take vw [vx..]
        zi = take vh [vz..]
        xzi = map (\x -> (,) <$> pure x <*> zi) xi -- all coordinates in a two-d array
        xyzi = map2 toRay xzi
        ri = map2 (traceRay w) xyzi
        toRay (x,z) = Ray o d
            where
                o = Vec3 (fromIntegral x) 0 (fromIntegral z)
                d = Vec3 0 1 0


scaleRender :: Int -> World -> View -> Rendering
scaleRender n (World wc l ac os) (View w h z x) = doRender w' v'
    where
        n' = fromIntegral n
        l' = l * (scalar n')
        os' = map (objScale n') os
        w' = World wc l' ac os'
        v' = View (w*n) (h*n) (z*n) (x*n)


renderingToImage :: Rendering -> G.Image G.VU G.RGB Double
renderingToImage (Rendering ri rw rh) = G.fromLists i 
    where
        i = map2 toPixel ri
        toPixel (Vec3 r g b) = G.PixelRGB (toRGB r) (toRGB g) (toRGB b)
        toRGB x = fromIntegral (round x) / 255 -- RGB values must exist in range [0,1] as a Double
    

-- helper
map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f x = map (map f) x
        

-- test Rendering
-- identical that in Main.hs
testRender x = do
    G.writeImage "test.png" i
    where 
        ref = Just 0.1
        s1 = Sphere (Vec3 100 50 50) 20 ref (color 90 30 120)
        s2 = Sphere (Vec3 150 50 150) 15 ref (color 90 90 0)
        s3 = Sphere (Vec3 175 50 50) 30 ref (color 0 190 150)
        s4 = Sphere (Vec3 200 250 50) 25 ref (color 255 200 200)
        brColor = white
        w = World brColor (Vec3 0 0 0) 0.1 [s1, s2, s3, s4]
        v = View {vWidth=200
                , vHeight=200
                , vZ=0
                , vX=50}
        n = x
        i = renderingToImage $ scaleRender n w v
    