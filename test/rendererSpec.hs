{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Test.Tasty
    import Test.Tasty.HUnit
    
    import RayTracer
    import Vec3
    import Renderer

    
    
    
    main :: IO ()
    main = do
        defaultMain (testGroup "Testing Renderer Functionality"
            [])

    -- bad test
    doRenderTest :: TestTree
    doRenderTest = testCase "Testing doRender"
        (assertEqual "doRender should produce a rendering based off the args"
        (Rendering [[Vec3 1 1 1]] 1 1)
        (let
            s1 = Sphere (Vec3 5 5 5) 5 Nothing (color 120 120 120)
            wc = Vec3 0 0 0
            w = World wc (Vec3 0 0 0) 0.1 [s1]
            v = View {vWidth=5
                    , vHeight=5
                    , vZ=3
                    , vX=3}
            in doRender w v
        )
        )

    -- bad test
    renderingToImageTest :: TestTree
    renderingToImageTest = testCase "Testing renderingToImage"
        (assertEqual "renderingToImage should produce an image given a rendering"
        (Nothing)
        (let
            r = Rendering [[Vec3 45 45 45]] 1 1
            in (\x -> Just x) $ renderingToImage r
        )
        )