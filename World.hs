{-
A simple ray-tracable world of spheres and triangles + related functions.
Author: Steven Ruppert
For CSCI441: Computer Graphics, Fall 2012
-}

module World
( Color(..)
, Material(..)
, Primitive(..)
, World(..)
, Ray(..)
, pixels
) where

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
  in if n `dot` d == 0
     then Nothing
     else let t = (v1 `dot` n - n `dot` o) / (n `dot` d)
              p = o + d ^* t                             -- point of intersection
          -- if all three crossproducts are in the same direction as n
          -- then p is inside the triangle on its plane
          in if (((v2 - v1) `cross` (p - v1)) `dot` n) >= 0 &&
                (((v3 - v2) `cross` (p - v2)) `dot` n) >= 0 &&
                (((v1 - v3) `cross` (p - v3)) `dot` n) >= 0
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
-- x,y are offset from the top-left (image pixel coordinates)
pixel_ray :: World -> Int -> Int -> Int -> Int -> Ray
pixel_ray world width height x y =
  let e = viewpoint world
      o = screen_origin world
      v = screen_vertical world
      h = screen_horizontal world
      fi = fromIntegral
      dx = h ^* (((fi x) + 0.5 ) / (fi width))
      -- must transform from top-left pixel coords to bottom-left origin
      dy = v ^* (((fi height - 1) - (fi y) + 0.5 ) / (fi height))
  in Ray { origin    = e
         , direction = normal $ o + dx + dy - e
         }

-- The color of a point on a primitive within the world
lighting :: World -> Primitive -> Point -> Color
lighting world primitive point =
  let m                   = material primitive
      (Color kdr kdg kdb) = diffuse m
      (Color adr adg adb) = ambient m
      ks                  = specular_coef m
      exp                 = specular_exp m
      ia                  = ambient_intensity world
      i                   = light_intensity world

      n = normal $ case primitive of
        (Sphere c _ _)        -> point - c
        (Triangle v1 v2 v3 _) -> let n = (v2 - v1) `cross` (v3 - v1)
                                 in if n `dot` ((viewpoint world) - v1) >= 0
                                    then n  -- use the normal pointing towards
                                    else -n -- the viewer (thin triangle)
      l = normal $ (light_source world) - point
      v = normal $ (viewpoint world) - point
      h = normal $ v + l

      shadow_ray = Ray { origin = point, direction = l }
      other_primitives = filter (/= primitive) (primitives world)
      in_shadow = (n `dot` l) < 0 ||
                  Nothing /= (first_intersection shadow_ray other_primitives)

      i_tot kd ka =
        if in_shadow then
          ka * ia
        else
          i * (kd * (n `dot` l) + ks * (h `dot` n) ** exp) + ka * ia
  in
    Color (i_tot kdr adr) (i_tot kdg adg) (i_tot kdb adb)

-- The pixels of a World rendered into width x height pixels
pixels :: World -> Int -> Int -> [[Color]]
pixels world width height =
  [[ let ray = pixel_ray world width height x y
     in case first_intersection ray (primitives world) of
       Nothing                 -> Color 0 0 0 -- background
       Just (primitive, point) -> lighting world primitive point
  | x <- [0..(width - 1)]]
  | y <- [0..(height - 1)]]

