module WordParsers where

import Data.Binary.Get
import Data.Word
import Data.Bits
import Control.Monad (liftM, liftM2, replicateM, void, unless)

import Fit
import Parser
import CRC
import Control.Monad.State.Lazy

combine :: Bits a => Int -> Arch -> a -> a -> a
combine offset arch w1 w2 = case arch of
  BigEndian -> (w1 `shiftL` offset) `xor` w2
  _         -> (w2 `shiftL` offset) `xor` w1

word8P :: Parser Word8
word8P = do
  w    <- liftGet getWord8
  crc1 <- gets crcState
  modify $ setCRC (checkByte crc1 w)
  return w

word16P :: Arch -> Parser Word16
word16P arch = liftM2 (combine 8 arch) num8P num8P

word32P :: Arch -> Parser Word32
word32P arch = liftM2 (combine 16 arch) (num16P arch) (num16P arch)

word64P :: Arch -> Parser Word64
word64P arch = liftM2 (combine 32 arch) (num32P arch) (num32P arch)

num8P :: Num a => Parser a
num8P = liftM fromIntegral word8P

num16P :: Num a => Arch -> Parser a
num16P = liftM fromIntegral . word16P 

num32P :: Num a => Arch -> Parser a
num32P = liftM fromIntegral . word32P 

num64P :: Num a => Arch -> Parser a
num64P = liftM fromIntegral . word64P 
