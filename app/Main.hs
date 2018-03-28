module Main where

import RayTracer
import Renderer
import Vec3
import Foreign.C.Types
import qualified Graphics.Image as G

main :: IO ()
main = do
    G.writeImage "test.png" i
    where 
        ref = Just 0.5
        s1 = Sphere (Vec3 50 50 50) 3 ref (color 170 0 0)
        s2 = Sphere (Vec3 100 100 100) 20 ref (color 0 170 0)
        s3 = Sphere (Vec3 150 150 150) 50 ref (color 0 0 170)
        s4 = Sphere (Vec3 100 100 50) 20 ref (color 170 170 0)
        brColor = color 100 100 100
        w = World brColor (Vec3 0 0 0) 0.2 [s1, s2, s3, s4]
        v = View {vWidth=200
                , vHeight=200
                , vZ=0
                , vX=0}
        n = 8
        i = renderingToImage $ scaleRender n w v