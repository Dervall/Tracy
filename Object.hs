module Object where

import Ray
import Vector
import Debug.Trace
import Material

data IntersectionPoint = IntersectionPoint { point :: Vector,
                                             normal :: Vector, 
                                             material :: Material } deriving Show

data Object = Sphere Vector Scalar Material | Plane Vector Scalar Material deriving Show

rayIntersect :: Object -> Ray -> Maybe IntersectionPoint
rayIntersect (Sphere c r m) (Ray p d)
        | vpcDot < 0 = if vpcLen > r        -- Sphere is behind the point
            then Nothing                    -- Sphere is behind and outside of our radius
            else             
                if vpcLen == r               
                    then intersectionPoint p                            -- Exactly on the sphere!
                    else intersectionPoint (p `vadd` (vscale di1 d))  -- Somewhere inside it
        | otherwise = if cpcLen > r
            then Nothing
            else intersectionPoint (p `vadd` (vscale ((if vpcLen > r then (-) else (+)) (vlength pcp) dist) d)) 
    where vpc = c `vsub` p                  -- Vector from ray origin to center of sphere
          vpcDot = vpc `vdot` d             -- Dot product of vector to sphere with ray direction
          pc = p `vadd` (vpcDot `vscale` d) -- Projection of the ray to the closest intersection point
          vpcLen = vlength vpc
          cpc = pc `vsub` c
          cpcLen = vlength cpc
          dist = sqrt ((r*r) - (cpcLen*cpcLen))
          di1 = dist - (vlength pcp)
          pcp = pc `vsub` p
          intersectionPoint a = Just (IntersectionPoint a (vnorm (a `vsub` c)) m)
rayIntersect (Plane pn d m) (Ray rp rd)
    | vd == 0 = Nothing
    | otherwise = let t = v0 / vd
                      hitpoint = rp `vadd` (vscale t rd)
                  in if t > 0.000001 then Just (IntersectionPoint hitpoint pn m)
                                   else Nothing
  where vd = pn `vdot` rd
        v0 = negate ((pn `vdot` rp) + d)
