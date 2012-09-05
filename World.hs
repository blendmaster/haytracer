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

-- The closest intersection point between the ray and the primitive
intersection :: Ray -> Primitive -> Maybe Point
intersection (Ray o d) (Sphere center r _) =
  let a = d `dot` d
      b = (d ^* 2) `dot` (o - center)
      c = (o - center) `dot` (o - center) - r ** 2

      det = b ** 2 - 4 * a * c

      point_from t = Just (o + d ^* t)
      choose_from t1 t2
        | t1 < 0 && t2 < 0 = Nothing
        | t1 > 0 && t2 < 0 = point_from t1
        | t2 < 0 && t2 > 0 = point_from t2
        | otherwise        = if t1 < t2 then point_from t1
                                        else point_from t2
  in if det < 0 then Nothing
                else choose_from (((-b) + sqrt det) / (2 * a))
                                 (((-b) - sqrt det) / (2 * a))

intersection (Ray o d) (Triangle v1 v2 v3 _) = Nothing -- TODO

-- the closest intersection (to the origin) between a primitive in the world
-- and a ray, or Nothing if there is no such intersection
--
-- oh gosh so bad
{-first_intersection :: Ray -> World -> Maybe Primitive-}
{-first_intersection ray world =-}
  {-let intersections = filter (\p -> (intersection ray p) /= Nothing)-}
                             {-(primitives world)-}
  {-in case intersections of [] -> Nothing-}
                           {-xs -> closest ray xs-}
  {-where closest ray xs = first (sort (\p -> (magnitude (intersection ray p)))-}
