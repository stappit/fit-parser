{-# LANGUAGE OverloadedStrings #-}

module FitParser where

import Data.Binary.Get -- binary parser combinators
import Data.Bits       -- bit operations
import Data.Word       -- unsigned ints
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Except
import Control.Monad.State.Lazy

import Control.Applicative (liftA3)
import qualified Data.Map as M

import Fit
import CRC
import Parser
import ProfileParser
import WordParsers

fitP :: Parser Fit
fitP = liftA3 Fit fitHeaderP messagesP crcP

parseSize :: FitHeader -> Size
parseSize (FitHdr hdrSize _ msgSize _) = hdrSize + msgSize

fitHeaderP :: Parser FitHeader
fitHeaderP = do
  hdrSize <- num8P
  version <- num8P  
  _       <- word16P LittleEndian
  msgSize <- num32P LittleEndian
  _       <- modify $ setSize (hdrSize + msgSize)
  _       <- dotFITP
  crc     <- if hdrSize > 12 
               then liftM Just crcP
               else return Nothing
  return $ FitHdr hdrSize version msgSize crc

dotFITP :: Parser ()
dotFITP = do
  cs <- replicateM 4 word8P
  unless (BL.pack cs == ".FIT") $ throwError DotFIT

crcP :: Parser CRC
crcP = do 
  crc1 <- word16P LittleEndian
  crc2 <- liftM crc get
  if crc2 == 0
    then return crc1
    else throwError $ CRCFail crc2

definitionP :: LocalMsgNum -> Parser DefinitionMessage
definitionP lMsg = do
  _         <- reservedP
  arch      <- archP
  gMsg      <- globalMsgNumP arch
  numFields <- num8P
  defs      <- replicateM numFields fieldDefP
  let def = Def lMsg arch gMsg defs
  _         <- modify $ addDef lMsg def
  return def

msgHeaderP :: Parser Header
msgHeaderP = liftM makeHeader word8P

archP :: Parser Arch
archP = do
  byte <- word8P
  if testBit byte 0
    then return BigEndian
    else return LittleEndian

fieldDefP :: Parser FieldDefinition
fieldDefP = liftA3 FieldDef word8P num8P baseTypeP

baseTypeP :: Parser Word8
baseTypeP = do
    n <- word8P 
    return $ 0xF .&. n

reservedP :: Parser ()
reservedP = void word8P

globalMsgNumP :: Arch -> Parser GlobalMsgNum
globalMsgNumP arch = do
    w <- word16P arch
    _ <- modify $ setGlobalMsgNum w
    return w

dataP :: Header -> Parser DataMessage
dataP hdr = do
   defs <- liftM definitions get
   case hdr of
     DefnH _ -> throwError WrongMsgType
     DataH lMsg -> do
         profile <- maybe (throwError $ NoDefFound lMsg) profileP (M.lookup lMsg defs) 
         return $ Dat lMsg Nothing profile
     CompH lMsg offset -> do
         ts     <- liftM timestamp get
         ts'    <- maybe (throwError NoTimestampFound) (return . addOffset offset) ts
         _      <- modify $ setTimestamp ts'
         profile <- maybe (throwError $ NoDefFound lMsg) profileP (M.lookup lMsg defs) 
         return $ Dat lMsg (Just ts') profile

messageP :: Parser Message
messageP = do
  hdr <- msgHeaderP
  case hdr of
    (DefnH     lMsg) -> liftM DefnM $ definitionP lMsg
    _        -> liftM DataM $ dataP hdr

messagesP :: Parser [Message]
messagesP = do
  size <- liftM size get
  consumed  <- liftGet bytesRead
  if size > consumed
    then do
      msg  <- messageP
      msgs <- messagesP
      return $ msg : msgs
    else return []

runFitParser :: Filename -> IO (Either ParseError Fit)
runFitParser fname = do
  bytestring <- BL.readFile fname
  return $ runGet (runExceptT $ evalStateT (runParser fitP) (ParseState 12 M.empty Nothing 0 0)) bytestring

type Filename = String

