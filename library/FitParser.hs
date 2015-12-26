{-# LANGUAGE OverloadedStrings #-}

module FitParser where

import Data.Binary.Get -- binary parser combinators
import Data.Bits       -- bit operations
import Data.Word       -- unsigned ints
import Data.Int        -- signed ints
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM, replicateM, void, unless)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

import Control.Applicative (liftA3)
import qualified Data.Map as M

import Fit
import CRC
import BaseType
import Parser

fitP :: Parser Fit
fitP = liftA3 Fit fitHeaderP messagesP crcP

parseSize :: FitHeader -> Size
parseSize (FitHdr hdrSize _ msgSize _) = hdrSize + msgSize

fitHeaderP :: Parser FitHeader
fitHeaderP = do
  hdrSize <- num8P
  version <- num8P  
  _       <- num16P LittleEndian
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
  unless (BL.pack cs == ".FIT") $ lift $ throwE DotFIT

crcP :: Parser CRC
crcP = do 
  crc1 <- word16P LittleEndian
  crc2 <- liftM crc get
  if crc2 == 0
    then return crc1
    else lift $ throwE (CRCFail crc2)

definitionP :: LocalMsgNum -> Parser Definition
definitionP lMsg = do
  _         <- reservedP
  arch      <- archP
  gMsg      <- globalMsgNumP arch
  numFields <- num8P
  defs      <- replicateM numFields fieldDefP
  let def = Defn lMsg arch gMsg defs
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

globalMsgNumP :: Arch -> Parser Word16
globalMsgNumP = word16P

dataP :: Header -> Parser Data
dataP hdr = do
   defs <- liftM definitions get
   consumed <- lift . lift $ bytesRead
   case hdr of
     DefnH _ -> lift $ throwE WrongMsgType
     DataH lMsg -> do
         let mDef = M.lookup lMsg defs
         gMsg   <- lift $ maybe (throwE $ NoDefFound consumed lMsg) (return . globalMsgNum) mDef
         fields <- maybe (lift . throwE $ NoDefFound consumed lMsg) fieldsP mDef 
         return $ Data lMsg gMsg Nothing fields
     CompH lMsg offset -> do
         ts <- liftM timestamp get
         let mDef = M.lookup lMsg defs
             ts' = addOffset offset (fromMaybe 0 ts)
         _  <- modify $ setTimestamp ts'
         gMsg   <- lift $ maybe (throwE $ NoDefFound consumed lMsg) (return . globalMsgNum) mDef
         fields <- maybe (lift . throwE $ NoDefFound consumed lMsg) fieldsP mDef 
         return $ Data lMsg gMsg (Just ts') fields

fieldP :: Arch -> GlobalMsgNum -> M.Map FieldNumber Modification -> FieldDefinition -> Parser Profile
fieldP arch gMsg profileMap fDef = case fDef of
  FieldDef fNum _    0  -> liftM Enum    word8P                   >>= mkFieldE fNum
  FieldDef fNum _    1  -> liftM SInt8   num8P                    >>= mkFieldE fNum 
  FieldDef fNum _    2  -> liftM UInt8   word8P                   >>= mkFieldE fNum 
  FieldDef fNum _    3  -> liftM SInt16  (num16P arch)            >>= mkFieldE fNum 
  FieldDef fNum _    4  -> liftM UInt16  (word16P arch)           >>= mkFieldE fNum 
  FieldDef fNum _    5  -> liftM SInt32  (num32P arch)            >>= mkFieldE fNum 
  FieldDef fNum _    6  -> liftM UInt32  (word32P arch)           >>= mkFieldE fNum 
  FieldDef fNum size 7  -> liftM String  (replicateM size word8P) >>= mkFieldE fNum 
  FieldDef fNum _    8  -> liftM Float32 (num32P arch)            >>= mkFieldE fNum 
  FieldDef fNum _    9  -> liftM Float64 (num64P arch)            >>= mkFieldE fNum 
  FieldDef fNum _    10 -> liftM UInt8z  word8P                   >>= mkFieldE fNum 
  FieldDef fNum _    11 -> liftM UInt16z (word16P arch)           >>= mkFieldE fNum 
  FieldDef fNum _    12 -> liftM UInt32z (word32P arch)           >>= mkFieldE fNum 
  FieldDef fNum size 13 -> liftM Byte    (replicateM size word8P) >>= mkFieldE fNum 
  FieldDef _    _    bt -> lift $ throwE (InvalidBasetype bt)

  where
    mkFieldE fNum bt = lift $ do
      modification <- maybe (return $ const (Just NoProfile)) return (M.lookup fNum profileMap) -- should throw nofieldnum error
      maybe (throwE $ TypeMismatch gMsg fNum bt) return (modification bt)

fieldsP :: Definition -> Parser [Profile]
fieldsP (Defn _ arch gMsg defs) = do
  profile <- lift $ maybe (return M.empty) return (M.lookup gMsg profiles) -- should throw noglobmsg error
  mapM (fieldP arch gMsg profile) defs

messageP :: Parser Message
messageP = do
  hdr <- msgHeaderP
  case hdr of
    (DefnH     lMsg) -> liftM DefnM $ definitionP lMsg
    _        -> liftM DataM $ dataP hdr

messagesP :: Parser [Message]
messagesP = do
  size <- liftM size get
  consumed  <- lift . lift $ bytesRead
  if size > consumed
    then do
      msg  <- messageP
      msgs <- messagesP
      return $ msg : msgs
    else return []

runFitParser :: Filename -> IO (Either ParseError Fit)
runFitParser fname = do
  bytestring <- BL.readFile fname
  return $ runGet (runExceptT $ evalStateT fitP (ParseState 12 M.empty Nothing 0)) bytestring

{-runMsgParser :: Filename -> IO (Either ParseError Message)-}
{-runMsgParser fname = do-}
  {-bytestring <- BL.readFile fname-}
  {-return $ runGet (runExceptT $ evalStateT (fitHeaderP >> messageP >> messageP >> messageP) (ParseState 12 M.empty Nothing)) bytestring-}

type Filename = String

combine :: Bits a => Int -> Arch -> a -> a -> a
combine offset arch w1 w2 = case arch of
  BigEndian -> (w1 `shiftL` offset) `xor` w2
  _         -> (w2 `shiftL` offset) `xor` w1

word8P :: Parser Word8
word8P = do
  w    <- lift . lift $ getWord8
  crc1 <- liftM crc get
  modify $ setCRC (checkByte crc1 w)
  return w

word16P :: Arch -> Parser Word16
word16P arch = do
  w1 <- num8P 
  w2 <- num8P
  return $ combine 8 arch w1 w2

word32P :: Arch -> Parser Word32
word32P arch = do
  w1 <- num16P arch
  w2 <- num16P arch
  return $ combine 16 arch w1 w2


word64P :: Arch -> Parser Word64
word64P arch = do
  w1 <- num32P arch
  w2 <- num32P arch
  return $ combine 32 arch w1 w2

num8P :: Num a => Parser a
num8P = liftM fromIntegral word8P

num16P :: Num a => Arch -> Parser a
num16P = liftM fromIntegral . word16P 

num32P :: Num a => Arch -> Parser a
num32P = liftM fromIntegral . word32P 

num64P :: Num a => Arch -> Parser a
num64P = liftM fromIntegral . word64P 
