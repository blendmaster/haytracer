{-
A simple ray-tracer. Takes a simple 3d world input and writes an output PPM
image.

Author: Steven Ruppert
For CSCI441: Computer Graphics, Fall 2012
-}

import Vector
import World
import System.Environment

-- haskell doesn't accept a leading decimal point e.g. ".02425" with
-- vanilla Read, so this hacks around it. TODO look up better solution
parse_double :: String -> Scalar
parse_double num@('.':digits) = read $ '0':num
parse_double num = read num

parse_material :: [Scalar] -> Material
parse_material [kdr, kdg, kdb, kar, kag, kab, ks, nspec] =
  Material { diffuse       = Color kdr kdg kdb
           , ambient       = Color kar kag kab
           , specular_coef = ks
           , specular_exp  = nspec
           }

parse_primitives :: [String] -> [Primitive]
parse_primitives [] = []
parse_primitives ("S":v) =
  let (cx:cy:cz:r:m) = map parse_double $ take 12 v
  in Sphere { center   = Vector cx cy cz
            , radius   = r
            , material = parse_material m
            }
     : (parse_primitives $ drop 12 v)
parse_primitives ("T":v) =
  let (x1:y1:z1:x2:y2:z2:x3:y3:z3:m) = map parse_double $ take 17 v
  in Triangle { v1       = Vector x1 y1 z1
              , v2       = Vector x2 y2 z2
              , v3       = Vector x3 y3 z3
              , material = parse_material m
              }
     : (parse_primitives $ drop 17 v)

parse_world :: [String] -> World
parse_world input =
  let [ex,ey,ez
       ,lx,ly,lz
       ,hx,hy,hz
       ,vx,vy,vz
       ,bx,by,bz,i,a] = map parse_double $ take 17 input
      prims = drop 18 input -- drop useless "number of primatives" field
  in World { viewpoint         = Vector ex ey ez
           , screen_origin     = Vector lx ly lz
           , screen_horizontal = Vector hx hy hz
           , screen_vertical   = Vector vx vy vz

           , light_source      = Vector bx by bz
           , light_intensity   = i
           , ambient_intensity = a

           , primitives        = parse_primitives prims
           }

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
  (infile:_) <- getArgs
  putStrLn $ "Reading " ++ infile ++ "..."
  input <- readFile infile
  putStrLn "Rendering to output.ppm..."
  let (w:h:rest) = words input
      world = parse_world rest
  writeFile "output.ppm" (ppm (pixels world (read w) (read h)))
  putStrLn "done!"
