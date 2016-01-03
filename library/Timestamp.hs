module Timestamp where

import Data.Word

type Timestamp = Int

newtype DateTime = 
    DateTime Word32
  deriving Show

newtype LocalDateTime = 
    LocalDateTime Word32
  deriving Show
 
