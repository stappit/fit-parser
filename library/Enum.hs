module Enum where

import Data.Word

class Enum' a where
  toEnum'   :: Word16 -> Maybe a
  fromEnum' :: a -> Word16
