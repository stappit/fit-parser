{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Timestamp where

import Data.Word
import Data.Bits 

newtype Timestamp = Timestamp Word32
  deriving (Show, Num, Eq, Ord, Bits)

newtype DateTime = 
    DateTime Word32
  deriving Show

newtype LocalDateTime = 
    LocalDateTime Word32
  deriving Show
 
