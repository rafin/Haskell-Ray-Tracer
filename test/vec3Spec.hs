module Main where

    import Test.Tasty
    import Test.Tasty.HUnit
    
    import Vec3
    
    main :: IO ()
    main = do
        defaultMain (testGroup "Testing Vec3 Functionality"
            [addTest
            , subTest
            , multTest
            , divTest
            , scalarTest
            , cDotTest
            , normalizeTest])
    
    addTest :: TestTree
    addTest = testCase "Testing Vec3 addition"
        (assertEqual "adding two Vec3's does element wise addition" 
            ((Vec3 2 3 (-1)) + Vec3 2 2 7)    
            (Vec3 4 5 6))
    
    subTest :: TestTree
    subTest = testCase "Testing Vec3 subtraction"
        (assertEqual "subtracting two Vec3's does element wise subtraction"
            ((Vec3 2 3 (-1)) - Vec3 (-2) (-2) (-7))    
            (Vec3 4 5 6))
    
    multTest :: TestTree
    multTest = testCase "Testing Vec3 multiplication"
        (assertEqual "multiplying two Vec3's does element wise multiplication"
            ((Vec3 2 3 (-1)) * Vec3 2 2 7)    
            (Vec3 4 6 (-7)))
    
    divTest :: TestTree
    divTest = testCase "Testing Vec3 division"
        (assertEqual "adding two Vec3's does element wise division"
            ((Vec3 2 3 (-1)) / Vec3 2 2 4)
            (Vec3 (1) (1.5) (-0.25)) )
    
    scalarTest :: TestTree
    scalarTest = testCase "Testing Vec3 scalar generation"
        (assertEqual "'scalar' converts a CFloat to a vector where all elements are the CFloat value"
            (Vec3 4 4 4)
            (scalar 4))
    
    cDotTest :: TestTree
    cDotTest = testCase "Testing Vec3 dot product implemenation"
        (assertEqual "`cdot` returns the dot product between two vectors"
            4.5
            (cDot (Vec3 3.3 2 (-4)) (Vec3 5 (-1) 2.5)) )

    normalizeTest :: TestTree
    normalizeTest = testCase "Testing Vec3 normalize implementation"
        (assertEqual "`normalize` should result in a vector with a magnitude of 1"
            (Vec3 (1/(sqrt 14)) (2/(sqrt 14)) (3/(sqrt 14)))
            (normalize (Vec3 1 2 3)))