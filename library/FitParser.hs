module FitParser where

import Data.Binary.Get -- binary parser combinators
import Data.Bits       -- bit operations
import Data.Word       -- unsigned ints
import Data.Int        -- signed ints
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)
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

fitHeaderG :: Get FitHeader
fitHeaderG = do
  hdrSize <- liftM fromIntegral getWord8
  version <- liftM fromIntegral getWord8
  _       <- getWord16le
  msgSize <- liftM fromIntegral getWord32le
  _       <- count 4 getWord8
  crc     <- if hdrSize > 12 
               then crcG >>= return . Just 
               else return Nothing
  return $ FitHdr hdrSize version msgSize crc

fitHeaderP :: Parser FitHeader
fitHeaderP = do
  hdr <- lift . lift $ fitHeaderG
  _   <- modify $ setSize (parseSize hdr)
  return hdr

crcG :: Get CRC
crcG = getWord16le

crcP :: Parser CRC
crcP = lift . lift $ crcG

definitionG :: LocalMsgNum -> Get Definition
definitionG lMsg = do
  _         <- reserved
  arch      <- archG
  gMsg      <- globalMsgNumG arch
  numFields <- fieldSizeG
  defs      <- count numFields fieldDefG
  return $ Defn lMsg arch gMsg defs

definitionP :: LocalMsgNum -> Parser Definition
definitionP lMsg = do
  def       <- lift . lift $ definitionG lMsg
  _         <- modify $ addDef lMsg def
  return def

msgHeaderG :: Get Header
msgHeaderG = getWord8 >>= return . makeHeader

msgHeaderP :: Parser Header
msgHeaderP = lift . lift $ msgHeaderG

archG :: Get Arch
archG = do
  byte <- getWord8
  if testBit byte 0
    then return BigEndian
    else return LittleEndian

fieldDefG :: Get FieldDefinition
fieldDefG = liftA3 FieldDef getWord8 fieldSizeG baseTypeG

fieldSizeG :: Get Int
fieldSizeG = getWord8 >>= return . fromIntegral

baseTypeG :: Get Word8
baseTypeG = do
    n <- getWord8 
    return $ 0xF .&. n

reserved :: Get ()
reserved = skip 1

globalMsgNumG :: Arch -> Get Word16
globalMsgNumG = getWord16

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
fieldP arch gMsg profileMap fDef = lift $ case fDef of
  FieldDef fNum _    0  -> liftM Enum    (lift getWord8)              >>= mkFieldE fNum
  FieldDef fNum _    1  -> liftM SInt8   (lift getInt8)               >>= mkFieldE fNum 
  FieldDef fNum _    2  -> liftM UInt8   (lift getWord8)              >>= mkFieldE fNum 
  FieldDef fNum _    3  -> liftM SInt16  (lift $ getInt16 arch)       >>= mkFieldE fNum 
  FieldDef fNum _    4  -> liftM UInt16  (lift $ getWord16 arch)      >>= mkFieldE fNum 
  FieldDef fNum _    5  -> liftM SInt32  (lift $ getInt32 arch)       >>= mkFieldE fNum 
  FieldDef fNum _    6  -> liftM UInt32  (lift $ getWord32 arch)      >>= mkFieldE fNum 
  FieldDef fNum size 7  -> liftM String  (lift $ count size getWord8) >>= mkFieldE fNum 
  FieldDef fNum _    8  -> liftM Float32 (lift $ getNum32 arch)       >>= mkFieldE fNum 
  FieldDef fNum _    9  -> liftM Float64 (lift $ getNum64 arch)       >>= mkFieldE fNum 
  FieldDef fNum _    10 -> liftM UInt8z  (lift getWord8)              >>= mkFieldE fNum 
  FieldDef fNum _    11 -> liftM UInt16z (lift $ getWord16 arch)      >>= mkFieldE fNum 
  FieldDef fNum _    12 -> liftM UInt32z (lift $ getWord32 arch)      >>= mkFieldE fNum 
  FieldDef fNum size 13 -> liftM Byte    (lift $ count size getWord8) >>= mkFieldE fNum 
  FieldDef _    _    bt -> throwE (InvalidBasetype bt)

  where
    mkFieldE fNum bt = do
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
    else do
      return []

runFitParser :: Filename -> IO (Either ParseError Fit)
runFitParser fname = do
  bytestring <- BL.readFile fname
  return $ runGet (runExceptT $ evalStateT fitP (ParseState 12 M.empty Nothing)) bytestring

runMsgParser :: Filename -> IO (Either ParseError Message)
runMsgParser fname = do
  bytestring <- BL.readFile fname
  return $ runGet (runExceptT $ evalStateT (fitHeaderP >> messageP >> messageP >> messageP) (ParseState 12 M.empty Nothing)) bytestring

type Filename = String

getInt8 :: Get Int8
getInt8 = liftM fromIntegral getWord8

getInt16 :: Arch -> Get Int16
getInt16 arch = liftM fromIntegral (getWord16 arch)

getInt32 :: Arch -> Get Int32
getInt32 arch = liftM fromIntegral (getWord32 arch)

getWord16 :: Arch -> Get Word16
getWord16 BigEndian = getWord16be
getWord16 _         = getWord16le

getWord32 :: Arch -> Get Word32
getWord32 BigEndian = getWord32be
getWord32 _         = getWord32le

getWord64 :: Arch -> Get Word64
getWord64 BigEndian = getWord64be
getWord64 _         = getWord64le

getNum8 :: Num a => Get a
getNum8 = liftM fromIntegral getWord8

getNum16 :: Num a => Arch -> Get a
getNum16 arch = liftM fromIntegral (getWord16 arch)

getNum32 :: Num a => Arch -> Get a
getNum32 arch = liftM fromIntegral (getWord32 arch)

getNum64 :: Num a => Arch -> Get a
getNum64 arch = liftM fromIntegral (getWord64 arch)

count :: Int -> Get a -> Get [a]
count n = sequence . take n . repeat

