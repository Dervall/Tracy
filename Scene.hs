module Scene where

import Object
import Vector
import Light
import Material

scene :: [Object]
scene = [Sphere [-6,0,20] 3 (DiffuseMaterial [1, 0, 0]   0.0 0.2),
         Sphere [6,0,20]  3 (DiffuseMaterial [1, 1, 1]   0.0 0.1),
         Sphere [6,3,30]  8 (DiffuseMaterial [0, 0.5, 1] 0.7 0.2),
         Plane  [0,-1,0]  5 (DiffuseMaterial [0, 0, 1]   0.5 0.2)]

lights :: [Light]
lights = [PointLight [0, (-50), 0] [1,1,1],
          Ambient [0.3,0.3,0.3]]
