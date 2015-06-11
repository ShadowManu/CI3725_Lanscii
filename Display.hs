module Display
( Display(..)
, PDisplay(..)
, SDisplay(..)
) where

-- Custom typeclass to display tokens
class Display a where
  display :: a -> String

class PDisplay a where
  pDisplay :: a -> [String]

class SDisplay a where
  sDisplay :: a -> IO [String]
