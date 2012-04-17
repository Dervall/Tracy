module Vector where

type Scalar = Float
type Vector = [Scalar]

vsub :: Vector -> Vector -> Vector
vsub = zipWith (-)

vadd :: Vector -> Vector -> Vector
vadd = zipWith (+)

vscale :: Scalar -> Vector -> Vector
vscale s = map (*s)

vnorm :: Vector -> Vector
vnorm v = map (/vlength v) v

vlength :: Vector -> Scalar
vlength = sqrt . vlengthsq

vlengthsq :: Vector -> Scalar
vlengthsq a = vdot a a

vdot :: Vector -> Vector -> Scalar
vdot a b = foldr1 (+) (zipWith (*) a b)

-- Promotes a vector adding an extra dimension
vpromote :: Vector -> Vector
vpromote a = a ++ [0]
