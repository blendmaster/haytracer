{-
3d vector functions
Author: Steven Ruppert
For CSCI441: Computer Graphics, Fall 2012
-}

module Vector
( Scalar(..)
, Vector(..)
, Point(..)
, (^*)
, dot
, cross
, magnitude
, normal
) where

type Scalar = Double -- should it be Num or something?

data Vector = Vector Scalar Scalar Scalar deriving (Show, Eq, Ord)
type Point = Vector -- points are just vectors from the origin

-- vector times scalar, think of ^ as a vector hat
(Vector a1 a2 a3) ^* b = Vector (a1 * b)
                                (a2 * b)
                                (a3 * b)

-- dot product
(Vector a1 a2 a3) `dot` (Vector b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3

-- cross product
(Vector a1 a2 a3) `cross` (Vector b1 b2 b3) = Vector (a2 * b3 - a3 * b2)
                                                     (a3 * b1 - a1 * b3)
                                                     (a1 * b2 - a2 * b1)

magnitude (Vector a1 a2 a3) = sqrt (a1 ** 2 + a2 ** 2 + a3 ** 2)

normal v@(Vector a1 a2 a3) = Vector (a1 / m) (a2 / m) (a3 / m)
                             where m = magnitude v

-- overload operators as necessary
-- is undefined bad?
instance Num Vector where
  (Vector a1 a2 a3) + (Vector b1 b2 b3) = Vector (a1 + b1)
                                                 (a2 + b2)
                                                 (a3 + b3)
  (*) = undefined -- `dot` and `cross` are just fine
  (Vector a1 a2 a3) - (Vector b1 b2 b3) = Vector (a1 - b1)
                                                 (a2 - b2)
                                                 (a3 - b3)

  negate (Vector a1 a2 a3) = Vector (-a1) (-a2) (-a3)
  abs = undefined
  signum = undefined
  fromInteger = undefined
