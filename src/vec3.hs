-- optimized vector with length = 3
-- https://wiki.haskell.org/Numeric_Haskell:_A_Vector_Tutorial#Generating_Vectors

{-# LANGUAGE BangPatterns #-}

module Vec3 where

import qualified Data.Vector.Storable as V 
import Foreign
import Foreign.C.Types

data Vec3 = Vec3 { x :: {-# UNPACK #-} !CFloat
                 , y :: {-# UNPACK #-} !CFloat
                 , z :: {-# UNPACK #-} !CFloat}
    deriving Eq

instance V.Storable Vec3 where
    sizeOf _ = sizeOf (undefined :: CFloat) * 4
    alignment _ = alignment (undefined :: CFloat)

    {-# INLINE peek #-}
    peek p = do
        x <- peekElemOff q 0
        y <- peekElemOff q 1
        z <- peekElemOff q 2
        return (Vec3 x y z)
        where 
            q = castPtr p

    {-# INLINE poke #-}
    poke p (Vec3 x y z) = do
        pokeElemOff q 0 x
        pokeElemOff q 1 y
        pokeElemOff q 2 z
        where 
            q = castPtr p

instance Show Vec3 where
    show (Vec3 x y z) = "Vec3 " ++ show x ++ " " ++ show y ++ " " ++ show z

instance Num Vec3 where
    (+) = cApp (+)
    (-) = cApp (-)
    (*) = cApp (*)
    abs = undefined
    fromInteger = undefined
    signum = undefined

instance Fractional Vec3 where
    (/) = cApp (/)
    recip = undefined
    fromRational = undefined

-- `cmap` and `capp` are a bit hacky for Haskell.
-- ideally the functionality would be satified with a Functor
-- and Applicative instance, but Vec3 is *-kinded, so that isn't
-- possible without external dependancies.
cMap :: (CFloat -> CFloat) -> Vec3 -> Vec3
cMap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

cApp :: (CFloat -> CFloat -> CFloat) -> Vec3 -> Vec3 -> Vec3
cApp f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')

-- dot product
cDot :: Vec3 -> Vec3 -> CFloat
cDot v1 v2 = x + y + z
    where (Vec3 x y z) = (v1 * v2)

scalar :: CFloat -> Vec3
scalar n = Vec3 n n n

normV :: CFloat -> CFloat -> CFloat -> Vec3
normV x y z = normalize (Vec3 x y z)

normalize :: Vec3 -> Vec3
normalize (Vec3 x y z) = (Vec3 x y z) / (scalar m)
    where m = sqrt (x^2 + y^2 + z^2)
