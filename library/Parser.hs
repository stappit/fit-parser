{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import Data.Int
import Data.Word 
import Data.Bits 
import Data.Binary.Get -- binary parser combinators
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

data ParseError = CRCFail CRC
                | NoDefFound LocalMsgNum
                | WrongMsgType
                | InvalidBasetype BaseType
                | InvalidFieldNum GlobalMsgNum FieldNumber
                | NoGlobalMsgFound GlobalMsgNum FieldNumber
                | NoTimestampFound
                | TypeMismatch GlobalMsgNum FieldNumber BaseTypeValue
                | DotFIT
                deriving Show

data ParseState = ParseState {
  size        :: Size,
  definitions :: M.Map LocalMsgNum Definition,
  timestamp   :: Maybe Timestamp,
  crc         :: CRC
}

addDef :: LocalMsgNum -> Definition -> ParseState -> ParseState
addDef lMsg def (ParseState sz defs ts crc) = ParseState sz (M.insert lMsg def defs) ts crc

setTimestamp :: Timestamp -> ParseState -> ParseState
setTimestamp ts (ParseState sz defs _ crc) = ParseState sz defs (Just ts) crc

setSize :: Size -> ParseState -> ParseState
setSize sz (ParseState _ defs ts crc) = ParseState sz defs ts crc

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
setCRC crc (ParseState sz defs ts _) = ParseState sz defs ts crc
