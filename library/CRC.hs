module CRC where

import Data.Word
import Data.Int
import Data.Bits

type CRC     = Word16

crcTable :: [Word16]
crcTable = [0, 52225, 55297, 5120, 61441, 15360, 10240, 58369, 40961, 27648, 30720, 46081, 20480, 39937, 34817, 17408]

lowerCheck :: CRC -> Word8 -> CRC
lowerCheck crc byte = 
    let tmp = crcTable !! (fromEnum $ crc .&. 15)
        crc' = (crc `shiftR` 4) .&. 4095
    in  crc' `xor` tmp `xor` crcTable !! (fromEnum $ byte .&. 15)

upperCheck :: CRC -> Word8 -> CRC
upperCheck crc byte =
    let tmp = crcTable !! (fromEnum $ crc .&. 15)
        crc' = (crc `shiftR` 4) .&. 4095
    in  crc' `xor` tmp `xor` crcTable !! (fromEnum $ byte `shiftR` 4 .&. 15)

check :: CRC -> Word8 -> CRC
check crc byte = upperCheck (lowerCheck crc byte) byte
