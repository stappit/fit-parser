module CRC where

import Data.Word
import Data.Int
import Data.Bits

type CRC = Word16

crcTable :: [Word16]
crcTable = 
  [ 0x0000, 0xCC01, 0xD801, 0x1400, 0xF001, 0x3C00, 0x2800, 0xE401
  , 0xA001, 0x6C00, 0x7800, 0xB401, 0x5000, 0x9C01, 0x8801, 0x4400
  ]

lowerCheck :: CRC -> Word8 -> CRC
lowerCheck crc byte =
    let tmp = crcTable !! fromIntegral (crc .&. 0xF)
        crc' = (crc `shiftR` 4) .&. 0x0FFF
    in  crc' `xor` tmp `xor` crcTable !! fromIntegral (byte .&. 0xF)

upperCheck :: CRC -> Word8 -> CRC
upperCheck crc byte = 
    let tmp = crcTable !! fromIntegral (crc .&. 0xF)
        crc' = (crc `shiftR` 4) .&. 0x0FFF
    in  crc' `xor` tmp `xor` crcTable !! fromIntegral (byte `shiftR` 4 .&. 0xF)

checkByte :: CRC -> Word8 -> CRC
checkByte crc byte = upperCheck (lowerCheck crc byte) byte
  
checkBytes :: CRC -> [Word8] -> CRC
checkBytes = foldl checkByte
