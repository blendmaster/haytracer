{-
A simple ray-tracable world of spheres and triangles + related functions.
Author: Steven Ruppert
For CSCI441: Computer Graphics, Fall 2012
-}

import Vector

data Material = Material { diffuse_red :: Scalar
                         , diffuse_green :: Scalar
                         , diffuse_blue :: Scalar
                         , ambient_red :: Scalar
                         , ambient_green :: Scalar
                         , ambient_blue :: Scalar
                         , specular_coef :: Scalar
                         , specular_exp :: Scalar
                         }
                         deriving (Show, Eq, Ord)

data Primitive = Sphere { center :: Point
                        , radius :: Scalar
                        , material :: Material
                        }
               | Triangle { v1 :: Point
                          , v2 :: Point
                          , v3 :: Point
                          , material :: Material
                          }
               deriving (Show, Eq, Ord)

data World = World { viewpoint :: Point
                   -- screen origin is at the bottom-left corner, and the
                   -- horizontal and vertical vectors extend from that point
                   , screen_origin :: Point
                   , screen_horizontal :: Vector
                   , screen_vertical :: Vector

                   , light_source :: Point
                   , light_intensity :: Scalar

                   , ambient_intensity :: Scalar

                   , primitives :: [Primitive]
                   }
                   deriving (Show)

data Ray = Ray { origin :: Point
               , direction :: Vector
               }
               deriving (Show, Eq, Ord)

-- intersection :: Ray -> Primitive -> Maybe Point
