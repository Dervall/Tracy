module Material where
import Vector

type Colour = [Scalar]

data Material = DiffuseMaterial Colour Scalar Scalar deriving Show

materialColour :: Material -> Colour
materialColour (DiffuseMaterial c _ _) = c

shinyness :: Material -> Scalar
shinyness (DiffuseMaterial _ s _) = s

specularity :: Material -> Scalar
specularity (DiffuseMaterial _ _ s) = s
