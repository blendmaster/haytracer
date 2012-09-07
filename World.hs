{-
A simple ray-tracable world of spheres and triangles + related functions.
Author: Steven Ruppert
For CSCI441: Computer Graphics, Fall 2012
-}

import Vector
import Data.List

data Color = Color { red, blue, green :: Scalar } deriving (Show, Eq, Ord)

data Material = Material { diffuse :: Color
                         , ambient :: Color
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

-- The closest intersection point between the ray and the primitive,
-- as well as the distance from the ray origin
intersection :: Ray -> Primitive -> Maybe (Point, Scalar)
intersection (Ray o d) (Sphere center r _) =
  let a = d `dot` d
      b = (d ^* 2) `dot` (o - center)
      c = (o - center) `dot` (o - center) - r ** 2

      det = b ** 2 - 4 * a * c

      point_from t = Just ((o + d ^* t), t)
      choose_from t1 t2
        | t1 < 0 && t2 < 0 = Nothing       -- sphere is 'behind' ray
        | t1 > 0 && t2 < 0 = point_from t1 -- ray starts inside sphere
        | t2 < 0 && t2 > 0 = point_from t2
        | otherwise        = if t1 < t2 then point_from t1 -- choose nearest
                                        else point_from t2
  in if det < 0 then Nothing -- no intersections at all
                else choose_from (((-b) + sqrt det) / (2 * a))
                                 (((-b) - sqrt det) / (2 * a))

intersection (Ray o d) (Triangle v1 v2 v3 _) =
  let n = normal $ (v2 - v1) `cross` (v3 - v1) -- normal to triangle's plane
      t = ((o - v1) `dot` n) / (d `dot` n)     -- intersection of ray/plane
      p = o + d ^* t                           -- point of intersection
  -- if all three crossproducts are in the same direction as n
  -- then p is inside the triangle on its plane
  in if ((v2 - v1) `cross` (p - v1)) `dot` n >= 0 &&
        ((v3 - v2) `cross` (p - v2)) `dot` n >= 0 &&
        ((v1 - v3) `cross` (p - v3)) `dot` n >= 0
     then Just (p, t)
     else Nothing

-- The first shape and the point at which a given ray intersects a list of
-- primitives, or Nothing if there is no intersection
-- TODO I sure wish I understood monads better
first_intersection :: Ray -> [Primitive] -> Maybe (Primitive, Point)
first_intersection ray primitives =
  let intersections = map (\s -> (s, (intersection ray s))) primitives
      just = filter ((/= Nothing) . snd) intersections
  in case just of
     [] -> Nothing
     _  -> let dist (s, Just (p, t)) = t
               dist_of a b = (dist a) `compare` (dist b)
               (s, Just (p, t)) = (minimumBy dist_of just)
           in Just (s, p)

-- The ray point from the viewpoint to the center of the x,yth pixel of
-- the width x height pixels of the rendered view
pixel_ray :: World -> Int -> Int -> Int -> Int -> Ray
pixel_ray world width height x y =
  let e = viewpoint world
      l = screen_origin world
      v = screen_vertical world
      h = screen_horizontal world
      fi = fromIntegral
  in Ray { origin    = e
         , direction = ((l + h ^* (((fi x) + 0.5) / (fi width))) +
                        (l + v ^* (((fi y) + 0.5) / (fi height)))) - e
         }

-- The pixels of a World rendered into width x height pixels
-- Simply uses the ambient color of the material of the primitive
-- TODO lighting and shading and stuff
pixels :: World -> Int -> Int -> [[Color]]
pixels world width height =
  [[ let ray = pixel_ray world width height x y
     in case first_intersection ray (primitives world) of
       Nothing        -> Color 0 0 0 -- background
       Just (prim, _) -> ambient . material $ prim
  | y <- [0..(height - 1)]]
  | x <- [0..(width - 1)]]

