module Fit where

import Data.Word
import Data.Int
import Data.Bits

import Timestamp
import BaseType
import CRC
import Profiles

data Fit = Fit FitHeader [Message] CRC
  deriving Show

data FitHeader = FitHdr HeaderSize ProtocollVersion ProfileVersion MessageSize (Maybe CRC)
  deriving Show

data Message = 
    DefnM DefinitionMessage 
  | DataM DataMessage
  deriving Show

data DefinitionMessage = Def LocalMsgNum Arch GlobalMsgNum [FieldDefinition]
  deriving Show

data Header = 
    DefnH LocalMsgNum
  | DataH LocalMsgNum 
  | CompH LocalMsgNum Offset
  deriving Show

makeHeader :: Word8 -> Header
makeHeader n
  | testBit n 7 =
      let offsetMask        = 31
          compressedMsgMask = 96
          msg               = fromIntegral $ (n .&. compressedMsgMask) `shiftR` 5
          offset            = n .&. offsetMask
      in  CompH msg offset
  | otherwise   = 
      let normalMsgMask = 15 
          msg           = fromIntegral $ n .&. normalMsgMask
      in  if testBit n 6 then DefnH msg else DataH msg 

data MessageType = DefnType | DataType
  deriving Show

data Arch = BigEndian | LittleEndian
  deriving Show

data FieldDefinition = FieldDef FieldNumber FieldSize BaseType
  deriving Show

data DataMessage = Dat LocalMsgNum (Maybe Timestamp) Profile
  deriving Show

data Field = Field String BaseTypeValue
  deriving Show

type Size        = Int64
type HeaderSize  = Int64
type MessageSize = Int64
type ProtocollVersion = Word8
type ProfileVersion   = Word16

type GlobalMsgNum = Word16

type LocalMsgNum = Word16
type Offset      = Word8

type FieldNumber = Word8
type FieldSize   = Int
type BaseType    = Word8
