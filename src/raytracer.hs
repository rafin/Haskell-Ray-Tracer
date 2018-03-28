module RayTracer where

import Vec3
import Foreign.C.Types
import Data.Maybe (catMaybes)


type Point = Vec3
type Distance = CFloat
type AmbientCoeff = CFloat

data Ray = Ray {rOrigin :: Point
    , rDir :: Vec3}
    deriving (Show, Eq)

type Normal = Ray

type Color = Vec3
color a b c = Vec3 a b c
black = (Vec3 0 0 0)
white = (Vec3 255 255 255)

data Sphere = Sphere {sOrigin :: Point
    , sRadius :: CFloat
    , sReflective :: Maybe CFloat
    , sColor :: Color}
    deriving (Show, Eq)

-- 'View' represents the field of view window into a renderable world.
-- 'vWidth' is the width in pixels of the fov.
-- 'vWidth' is the height in pixels of the fov.
-- 'vZ' is the bottom right z coord of the fov.
-- 'vX' is the bottom right x coord of the fov.
data View = View {vWidth :: Int
    , vHeight :: Int
    , vZ :: Int
    , vX :: Int}
    deriving (Show, Eq)

-- currently the only object type in the world is a 'Sphere'
type Object = Sphere

-- a 'World' represents the 3D environment to be rendered. 
-- The 'backgroundColor' is simply the color seen in the wolrd infinite. It's used 
-- in the case of a ray that intersects no objects.
-- The 'ambientCoeff' represents the porportion of light that is always present, whether
-- or not an object's surface faces a light source.
-- 'objects' is a list of the objects in the environment.
data World = World {backgroundColor :: Color
    , lightSrc :: Vec3
    , ambientCoeff :: CFloat
    , objects :: [Object]}
    deriving Show

ambientColor w = scalar (ambientCoeff w) * (backgroundColor w)

-- all objects should be intersectable and contain attributes such as
-- origin/center, isReflective, and a color
-- they can also be scaled
class Intersectable a where
    intersect :: Ray -> a -> Maybe (Object, Normal, Distance)  
        -- returns distance to point of intersection and
        -- the normal vector of the point of intersection.
    origin :: a -> Point
    isReflective :: a -> Maybe CFloat
    objColor :: a -> Color
    objScale :: CFloat -> a -> a

instance Intersectable Sphere where
    intersect r s = checkD disc
        where
            eo = (origin s) - (rOrigin r)
            rd' = normalize (rDir r)
            v = cDot eo rd'
            disc = ((sRadius s) ^ 2) - ((cDot eo eo) - v ^ 2)
            checkD d
                | (d < 0 || v - (sqrt d) <= 0) = Nothing
                | otherwise = Just (s, (Ray np nd), dist)
                    where
                        np = (rOrigin r) + (scalar (v - sqrt d) * rd') -- point of intersection
                        dist = v - (sqrt d) -- distance between ray and point of intersection
                        nd = normalize (np - (origin s)) -- normal direction             
    origin = sOrigin
    isReflective = sReflective
    objColor = sColor
    objScale n s@(Sphere so sr sref sc) = Sphere (so * n') (sr * n) sref sc
        where n' = scalar n

-- 'traceRay' will compute a coordinate's Color by tracing the input 'Ray' onto the input 'World'.
traceRay :: World -> Ray -> Color
traceRay w r@(Ray ro rd) = case firstContact w r of-- returns Maybe (Object, Normal)
    Just (o, (Ray no nd)) -> combineColor [pointColor, reflectColor]
        where
            pointColor = case firstContact w (Ray no ld) of
                Just (o', _) -> if (o' == o) then cs else scalar (ambientCoeff w) * (objColor o)
                Nothing -> cs
            cs = cosineShade (ambientCoeff w) (objColor o) ld nd
            ld = (lightSrc w) - no -- vector direction between obj and light src
            reflectColor = case (isReflective o) of
                Just s -> (scalar s) * (traceRay w (Ray no d)) -- s = strength of reflection
                    where d = reflect rd nd
                Nothing -> black
    Nothing -> ambientColor w

-- 'reflect' returns the reflection of the first vector towards the second vector (which is
-- the normal vector of the surface of intersection)
reflect :: Vec3 -> Vec3 -> Vec3
reflect r n = normalize $ r' + (scalar (2 * c1) * n')
    where
        r' = normalize r
        n' = normalize n
        c1 = (-1) * cDot r' n'

-- 'firstContact' will return the nearest object in the input 'World' that the input 'Ray' intersects
--  If no intersections occur, return Nothing
firstContact :: World -> Ray -> Maybe (Object, Normal)
firstContact w r = getNearest os
    where
        os = catMaybes $ map (intersect r) (objects w) -- list (obj, dist) of intersected objects
        getNearest xs = case length xs of
            0 -> Nothing
            _ -> Just (oMin, nMin)
                where (oMin, nMin, _) = foldr (isCloser) (head xs) (tail xs)
        isCloser (o, n, d) (o', n', d')
            | (d < d') = (o, n, d)
            | otherwise = (o', n', d')

-- 'cosineShade' returns The libertian/cosine shade
--      at the surface point of interest wrt the light source.
-- Args:
--      The 'AmbientCoeff' (-icient) is the porportion of light that is ambient light.
--      The 'Color' is the color of the object being shaded.
--      The first 'Vec3' is the direction between the point of interest and the light source.
--      The second 'Vec3' is the surface normal at the point of interest.
cosineShade :: AmbientCoeff -> Color -> Vec3 -> Vec3 -> Color
cosineShade acof ocol l n = cApp (*) ocol ( scalar (acof + (1-acof) * shade') )
    where
        shade = cDot (normalize l) (normalize n) 
        shade'
            | shade < 0 = 0
            | otherwise = shade

-- 'combineColor' will will generate "combined" color based off the input list of colors.
-- If no colors are present, black will be implicitly returned.
combineColor :: [Color] -> Color
combineColor = getColor . (filter ((/=) black))
        where
            getColor cs = case (length cs) of
                0 -> black
                n -> ((cMap coFix) (coAdd cs)) -- / (scalar (fromIntegral n))
            coAdd = foldr (+) black
            -- coMul = (foldr (*) (Vec3 1 1 1))
            coFix x
                | (x > 255) = 255
                | (x < 0) = 0
                | otherwise = x