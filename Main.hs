{-
A simple ray-tracer. Takes a simple 3d world input and writes an output PPM
image.

Author: Steven Ruppert
For CSCI441: Computer Graphics, Fall 2012
-}

import Vector
import World

test = World { viewpoint         = Vector 0 1 40
             , screen_origin     = Vector (-1) (-1) 1
             , screen_horizontal = Vector 2 0 0
             , screen_vertical   = Vector 0 2 0
             , light_source      = Vector 10 3 2
             , light_intensity   = 0.8
             , ambient_intensity = 0.2
             , primitives = [ Sphere { center = Vector 0.5 0.5 0
                                     , radius = 0.45
                                     , material =
                                         Material { diffuse = Color 1 0 0
                                                  , ambient = Color 1 0 0
                                                  , specular_coef = 0
                                                  , specular_exp = 1 }}
                            , Sphere { center = Vector 0.5 (-0.5) 0
                                     , radius = 0.45
                                     , material =
                                         Material { diffuse = Color 0 1 0
                                                  , ambient = Color 0 1 0
                                                  , specular_coef = 0.4
                                                  , specular_exp = 1 }}
                            , Sphere { center = Vector (-0.5) 0.5 0
                                     , radius = 2
                                     , material =
                                         Material { diffuse = Color 0 0 1
                                                  , ambient = Color 0 0 1
                                                  , specular_coef = 0
                                                  , specular_exp = 1 }}
                            , Sphere { center = Vector (-0.5) (-0.5) 0
                                     , radius = 2
                                     , material =
                                         Material { diffuse = Color 1 1 1
                                                  , ambient = Color 1 1 1
                                                  , specular_coef = 0
                                                  , specular_exp = 1 }}
                            ,  Sphere { center = Vector 0 0 0
                                     , radius = 2
                                     , material =
                                         Material { diffuse = Color 1 1 0
                                                  , ambient = Color 1 1 0
                                                  , specular_coef = 0
                                                  , specular_exp = 1 }}]}

-- the PPM file from a 2d array of color pixels
ppm :: [[Color]] -> String
ppm pixels =
  let len = show . length
      width = len $ head pixels -- length of first row
      height = len pixels

      color (Color r g b) = [channel r, channel g, channel b]
                            where channel = floor . (255*) . min 1 . max 0

      channels = unwords $ map show $ concatMap color (concat pixels)
  in
    "P3\n" ++ width ++ " " ++ height ++ " 255\n" ++ channels

main = do
  putStrLn "Writing output..."
  writeFile "output.ppm" (ppm (pixels test 512 512))
  putStrLn "done!"
