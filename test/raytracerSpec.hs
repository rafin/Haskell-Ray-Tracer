module Main where

import Test.Tasty
import Test.Tasty.HUnit

import RayTracer
import Vec3


main :: IO ()
main = do
    defaultMain (testGroup "Testing RayTracer Functionality"
        [reflectTest
        , sphereIntersectTest_hit
        , sphereIntersectTest_miss
        , sphereIntersectTest_negdist
        , firstContactTest_success
        , firstContactTest_miss
        , cosineShadeTest_direct
        , cosineShadeTest_opposite
        , combineColorTest_2colors
        , combineColorTest_nocolors
        , traceRayTest_noreflect
        , traceRayTest_reflect])

reflectTest :: TestTree
reflectTest = testCase "Testing reflect"
    (assertEqual "reflecting two vectors returns the reflection" 
        (Vec3 (-0.80178374) (-0.53452253) (-0.2672613)) -- close enough
        (reflect (Vec3 1 2 3) (Vec3 1 1 1))
    )

sphereIntersectTest_hit :: TestTree
sphereIntersectTest_hit = testCase "Testing `intersect :: sphere` - hit intersection"
    (assertEqual "intersection between a ray and a sphere returns Just (sphere, normal, distance)"
        (let 
            s = Sphere (Vec3 6 6 6) 5 Nothing (color 1 1 1) 
            n = Ray (Vec3 1.7445832 3.4891665 5.2337494) (Vec3 (-0.85108346) (-0.50216675) (-0.15325014))
            in Just (s, n, 6.5276327)
        )
        (let 
            r = Ray (Vec3 0 0 0) (Vec3 1 2 3)
            s = Sphere (Vec3 6 6 6) 5 Nothing (color 1 1 1)
            in intersect r s
        )
    )

sphereIntersectTest_miss :: TestTree
sphereIntersectTest_miss = testCase "Testing `intersect :: sphere` - missed intersection"
    (assertEqual "intersection between a ray and a sphere returns Nothing if they don't actually intersect"
        (Nothing)
        (let 
            r = Ray (Vec3 0 0 0) (Vec3 1 0 1)
            s = Sphere (Vec3 0 0 (-60)) 1 Nothing (color 1 1 1)
            in intersect r s
        )
    )

sphereIntersectTest_negdist :: TestTree
sphereIntersectTest_negdist = testCase "Testing `intersect :: sphere` - intersects, but in wrong direction"
    (assertEqual "intersection between a ray and a sphere returns Nothing if they intersect, but in wrong direction"
        (Nothing)
        (let 
            r = Ray (Vec3 0 0 1) (Vec3 0 0 (-1))
            s = Sphere (Vec3 0 0 2) 1 (Just 1) (color 5 5 5)
            in intersect r s
        )
    )

firstContactTest_success :: TestTree
firstContactTest_success = testCase "Testing firstContact - 2 intersecting spheres"
    (assertEqual "if 1 or more objects are intersected by a ray, return the object closest to the rays origin"
        (let
            s = Sphere (Vec3 0 0 2) 1 Nothing (color 1 1 1)
            n = Ray (Vec3 0 0 1) (Vec3 0 0 (-1))
            in Just (s,n)    
        )
        (let
            r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
            s1 = Sphere (Vec3 0 0 3) 1 Nothing (color 1 1 1)
            s2 = Sphere (Vec3 0 0 2) 1 Nothing (color 1 1 1)
            wc = color 1 1 1
            w = World wc (Vec3 0 0 0) 0.1 [s1, s2] 
            in firstContact w r
        )
    )

firstContactTest_miss :: TestTree
firstContactTest_miss = testCase "Testing firstContact - no intersection"
    (assertEqual "if no objects are intersected by a ray, return the worlds background color"
        (Nothing)
        (let
            r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
            s1 = Sphere (Vec3 2 0 0) 1 Nothing (color 1 1 1)
            s2 = Sphere (Vec3 (-2) 0 0) 1 Nothing (color 1 1 1)
            wc = color 1 1 1
            w = World wc (Vec3 3 3 3) 0.1 [s1, s2] 
            in firstContact w r
        )
    )

cosineShadeTest_direct :: TestTree
cosineShadeTest_direct = testCase "Testing cosineShade - facing light"
    (assertEqual "if a surface point faces a light source head on, the point will be at full brightness"
    (color 0.99999994 0.99999994 0.99999994) -- close enough
    (let
        l = Vec3 1 1 1 -- light direction vector (from surface towards light)
        n = Vec3 1 1 1 -- normal vector
        ocol = color 1 1 1
        in cosineShade 0.2 ocol l n
    )   
    )

cosineShadeTest_opposite :: TestTree
cosineShadeTest_opposite = testCase "Testing cosineShade - opposite of light"
    (assertEqual "if a surface point is opposite a light source, the point only experience ambient light"
    (color 0.2 0.2 0.2) -- close enough
    (let
        l = Vec3 1 1 1 -- light direction vector (from surface towards light)
        n = Vec3 (-1) (-1) (-1) -- normal vector
        ocol = color 1 1 1
        in cosineShade 0.2 ocol l n
    )   
    )

combineColorTest_2colors :: TestTree
combineColorTest_2colors = testCase "Testing combineColor - 2 valid colors"
    (assertEqual "Compute the colors, bounding output into the range [0-255]"
    (color 0 255 8)
    (let
        c1 = color (-10) 0 0
        c2 = color 1 253 1
        c3 = color 3 5 7
        in combineColor [c1, c2, c3]
    )    
    )

combineColorTest_nocolors :: TestTree
combineColorTest_nocolors = testCase "Testing combineColor - no valid colors"
    (assertEqual "if all input colors are `Nothing`, return Black"
    (color 0 0 0)
    (combineColor [(color 0 0 0),(color 0 0 0),(color 0 0 0)])    
    )

traceRayTest_noreflect :: TestTree
traceRayTest_noreflect = testCase "Testing traceRay - no reflection"
    (assertEqual "if an object isn't reflective, the objects color is simple it's base color"
    (color 5 5 5)
    (let
        r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
        s1 = Sphere (Vec3 0 0 2) 1 Nothing (color 5 5 5)
        wc = color 1 1 1
        w = World wc (Vec3 0 0 0) 0 [s1] 
        in traceRay w r
    )
    )

traceRayTest_reflect :: TestTree
traceRayTest_reflect = testCase "Testing traceRay - with reflection"
    (assertEqual "if an object is reflective, add the objects color to the reflective color"
    (color 5.5 5.5 5.5) -- (5 5 5) + 0.5 * 0.5 * (2 2 2)
    (let
        r = Ray (Vec3 0 0 0) (Vec3 0 0 1)
        s1 = Sphere (Vec3 0 0 2) 1 (Just 0.5) (color 5 5 5)
        wc = color 2 2 2
        w = World wc (Vec3 0 0 0) 0.5 [s1] 
        in traceRay w r
    )
    )
