module Parser where

import Data.Int
import Data.Word ()
import Data.Binary.Get -- binary parser combinators
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class ()

import qualified Data.Map as M

import Timestamp
import Fit
import BaseType

type Parser a = StateT ParseState (ExceptT ParseError Get) a

data ParseError = NoDefFound Int64 LocalMsgNum
                | WrongMsgType
                | InvalidBasetype BaseType
                | InvalidFieldNum GlobalMsgNum FieldNumber
                | NoGlobalMsgFound GlobalMsgNum FieldNumber
                | TypeMismatch GlobalMsgNum FieldNumber BaseTypeValue
                deriving Show

data ParseState = ParseState {
  size        :: Size,
  definitions :: M.Map LocalMsgNum Definition,
  timestamp   :: Maybe Timestamp  
}

addDef :: LocalMsgNum -> Definition -> ParseState -> ParseState
addDef lMsg def (ParseState sz defs ts) = ParseState sz (M.insert lMsg def defs) ts

setTimestamp :: Timestamp -> ParseState -> ParseState
setTimestamp ts (ParseState sz defs _) = ParseState sz defs (Just ts)

setSize :: Size -> ParseState -> ParseState
setSize sz (ParseState _ defs ts) = ParseState sz defs ts

addOffset :: Offset -> Timestamp -> Timestamp
addOffset _ = id -- TODO
