module Display
( Display(..)
, PDisplay(..)
) where

-- Custom typeclass to display tokens
class Display a where
  display :: a -> String

class PDisplay a where
  pDisplay :: a -> [String]
