module Parser where

import Data.Int
import Data.Word 
import Data.Binary.Get -- binary parser combinators
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class ()

import qualified Data.Map as M

import Timestamp
import Fit
import BaseType
import CRC

type Parser a = StateT ParseState (ExceptT ParseError Get) a

data ParseError = CRCFail CRC
                | NoDefFound Int64 LocalMsgNum
                | WrongMsgType
                | InvalidBasetype BaseType
                | InvalidFieldNum GlobalMsgNum FieldNumber
                | NoGlobalMsgFound GlobalMsgNum FieldNumber
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
addOffset _ = id -- TODO

setCRC :: CRC -> ParseState -> ParseState
setCRC crc (ParseState sz defs ts _) = ParseState sz defs ts crc
