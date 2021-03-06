{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Data.Int
import Data.Word 
import Data.Bits 
import Data.Binary.Get
import Control.Applicative (Applicative)
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Class ()

import qualified Data.Map as M

import Timestamp
import Fit
import BaseType
import CRC

newtype Parser a = Parser {runParser :: StateT ParseState (ExceptT ParseError Get) a}
  deriving (Functor, Applicative, Monad, MonadError ParseError, MonadState ParseState)

liftGet :: Get a -> Parser a
liftGet = Parser . lift . lift
  
bytesConsumed :: Parser Int64
bytesConsumed = liftGet bytesRead

data ParseError = CRCFail           !CRC
                | NoDefFound        !LocalMsgNum
                | WrongMsgType
                | InvalidBasetype   !BaseType
                | InvalidFieldNum   !GlobalMsgNum !FieldNumber
                | InvalidFieldNum'  !FieldNumber
                | NoGlobalMsgFound  !GlobalMsgNum !FieldNumber
                | NoTimestampFound
                | TypeMismatch      !GlobalMsgNum !FieldNumber !BaseTypeValue
                | NoEnum            !Word16
                | DotFIT
                | InvalidHeaderSize !Word8
                deriving Show

data ParseState = ParseState {
  totalBytesToparse :: !Size,
  definitions       :: M.Map LocalMsgNum DefinitionMessage,
  timestamp         :: Maybe Timestamp,
  crcState          :: !CRC,
  globalMsgNum      :: !GlobalMsgNum
}

type Size        = Int64

addDef :: LocalMsgNum -> DefinitionMessage -> ParseState -> ParseState
addDef lNum def (ParseState sz defs ts crc gNum) = ParseState sz (M.insert lNum def defs) ts crc gNum

setTimestamp :: Timestamp -> ParseState -> ParseState
setTimestamp ts (ParseState sz defs _ crc gNum) = ParseState sz defs (Just ts) crc gNum

setSize :: Size -> ParseState -> ParseState
setSize sz (ParseState _ defs ts crc gNum) = ParseState sz defs ts crc gNum

addOffset :: Offset -> Timestamp -> Timestamp
addOffset offset ts = (ts .&. tsMask) + offset' + rollover
  where
    tsMask   = Timestamp 0xFFFFFFE0
    roMask   = Timestamp 0x0000001F
    offset'  = Timestamp (fromIntegral offset)
    rollover = if offset' >= ts .&. roMask 
                 then 0 
                 else 0x20

setCRC :: CRC -> ParseState -> ParseState
setCRC crc (ParseState sz defs ts _ gNum) = ParseState sz defs ts crc gNum

setGlobalMsgNum :: GlobalMsgNum -> ParseState -> ParseState
setGlobalMsgNum gNum (ParseState sz defs ts crc _) = ParseState sz defs ts crc gNum
