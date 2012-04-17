module Main where
import Codec.Picture
import Vector
import Object
import Ray
import Scene
import Data.List
import Data.Maybe
import Light
import Material

main :: IO ()
main = do
    writePng "my_fancy_image.png" $ generateImage (\x y -> tracePixel x y) (screenSize !! 0) (screenSize !! 1)

tracePixel :: Int -> Int -> PixelRGBA8
tracePixel x y = toPixel ( rayTrace 0 (pixelRay x y) )

toPixel :: Colour -> PixelRGBA8
toPixel (r:g:b:[]) = PixelRGBA8 (f r) (f b) (f g) 255
    where f a = floor(clamp(a*255))
          clamp a = if a > 255 then 255 else a

rayTrace :: Int -> Ray -> Colour
rayTrace 2 _ = [0,0,0]
rayTrace depth ray@(Ray o d) = case i of
        (Just ip@(IntersectionPoint p n _)) -> zipWith (+) (map (*(1-k)) (localIllumination ip scene lights)) (map (*k) (rayTrace (depth+1) reflectedRay))
            where k = shinyness $ material ip
                  nk = 2 * (n `vdot` (vscale (-1) d))
                  outRayDir = (vscale nk n) `vsub` (vscale (-1) d)
                  reflectedRay = Ray (p `vadd` (vscale 0.1 outRayDir)) outRayDir --(vnorm reflectedNormal)
        _ -> [0,0,0]
    where i = closestIntersection o (sceneIntersections ray scene)




localIllumination :: IntersectionPoint -> [Object] -> [Light] -> Colour
localIllumination ip objects [] = [0,0,0] 
localIllumination ip objects (light:lights) = zipWith (+) (illuminate ip objects light) (localIllumination ip objects lights)

illuminate :: IntersectionPoint -> [Object] -> Light -> Colour
illuminate ip@(IntersectionPoint p n m) o (PointLight lightPos lightColour) = if null i 
        then [0,0,0] -- Nominally impossible, but hey, its floating point stuff
        else if vlengthsq ( occlusionPoint `vsub` p ) < epsilon 
            then zipWith (+) diffuseColour specularColour
            else [0,0,0]
    where lightDirection = vnorm (p `vsub` lightPos)
          ray = Ray lightPos lightDirection
          i = sceneIntersections ray o
          occlusionPoint = point (head i)
          diffuseColour = map (*(negate (n `vdot` lightDirection))) (zipWith (*) lightColour (materialColour m))  -- Successful lightning
          specularColour = [0,0,0] -- phong ray ip
illuminate (IntersectionPoint _ _ m) _ (Ambient lightColour) = (zipWith (*) lightColour (materialColour m))

phong :: Ray -> IntersectionPoint -> Colour
phong lightRay@(Ray _ lightRayDir) (IntersectionPoint p n m) = [finalBlinnTerm, finalBlinnTerm, finalBlinnTerm]
    where h = (vscale (-1) viewRayDir) `vadd` lightRayDir
          finalBlinnTerm = ((specularity m) * ( ((n) `vdot` h)**(1.8) ))
       {-- blinnDir = lightRayDir `vsub` viewRayDir
          temp = sqrt(blinnDir `vdot` blinnDir)
          invBlinnDir = vscale (-1/temp)  blinnDir
          blinnTerm = max (invBlinnDir `vdot` n) 0
          finalBlinnTerm = (specularity m) * (blinnTerm ** 1.4) * 1 --}
          viewRayDir = vnorm( p `vsub` eyePosition ) 



epsilon :: Scalar
epsilon = 0.00000001

closestIntersection :: Vector -> [IntersectionPoint] -> Maybe IntersectionPoint
closestIntersection _ [] = Nothing
closestIntersection v a = Just (head (sortBy (\a b -> compare (distToV a) (distToV b) ) a))
    where distToV (IntersectionPoint x _ _) = vlengthsq(x `vsub` v)

sceneIntersections :: Ray -> [Object] -> [IntersectionPoint]
sceneIntersections r = catMaybes . map ((flip rayIntersect) r)

-- Creates the initial ray for a given pixel
pixelRay :: Int -> Int -> Ray
pixelRay x y = Ray screenIntersection (vnorm (screenIntersection `vsub` eyePosition))
    where screenIntersection = screen [x,y]

-- Converts a screen coordinate into a normalized coordinate in the x y 0.0 plane
screen :: [Int] -> [Scalar]
screen a = vpromote ( zipWith (*) normalizedScreenSize (zipWith (\i j -> (fromIntegral i)/(fromIntegral j)) (zipWith (-) a halfScreenSize) halfScreenSize))
        
halfScreenSize :: [Int]
halfScreenSize = map (`div` 2) screenSize

-- Screen size in pixels
screenSize :: [Int]
screenSize = [1024, 786]

-- The normalized screen size, where the largest component goes from 0-1
normalizedScreenSize :: [Scalar]
normalizedScreenSize = map (/ maxOrientation) (map fromIntegral screenSize)
    where maxOrientation = fromIntegral $ maximum screenSize
          
-- Position of the eye
eyePosition :: Vector
eyePosition = [0.0, 0.0, -1.0]
