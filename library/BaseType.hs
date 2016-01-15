module BaseType where

import Data.Word
import Data.Int

data BaseTypeValue = Enum    !Word8
                   | SInt8   !Int8
                   | UInt8   !Word8
                   | SInt16  !Int16
                   | UInt16  !Word16
                   | SInt32  !Int32
                   | UInt32  !Word32
                   | String  ![Word8]
                   | Float32 !Float
                   | Float64 !Double
                   | UInt8z  !Word8
                   | UInt16z !Word16
                   | UInt32z !Word32
                   | Byte    ![Word8]
                   deriving Show

unEnum :: BaseTypeValue -> Maybe Word8
unEnum (Enum val) = Just val
unEnum _ = Nothing

unSInt8 :: BaseTypeValue -> Maybe Int8
unSInt8 (SInt8 val) = Just val
unSInt8 _          = Nothing

unSInt16 :: BaseTypeValue -> Maybe Int16
unSInt16 (SInt16 val) = Just val
unSInt16 _          = Nothing

unSInt32 :: BaseTypeValue -> Maybe Int32
unSInt32 (SInt32 val) = Just val
unSInt32 _          = Nothing

unUInt8 :: BaseTypeValue -> Maybe Word8
unUInt8 (UInt8 val) = Just val
unUInt8 _          = Nothing

unUInt16 :: BaseTypeValue -> Maybe Word16
unUInt16 (UInt16 val) = Just val
unUInt16 _          = Nothing

unUInt32 :: BaseTypeValue -> Maybe Word32
unUInt32 (UInt32 val) = Just val
unUInt32 _          = Nothing

unUInt8z :: BaseTypeValue -> Maybe Word8
unUInt8z (UInt8z val) = Just val
unUInt8z _          = Nothing

unUInt16z :: BaseTypeValue -> Maybe Word16
unUInt16z (UInt16z val) = Just val
unUInt16z _          = Nothing

unUInt32z :: BaseTypeValue -> Maybe Word32
unUInt32z (UInt32z val) = Just val
unUInt32z _          = Nothing

unString :: BaseTypeValue -> Maybe [Word8]
unString (String val) = Just val
unString _            = Nothing

unByte :: BaseTypeValue -> Maybe [Word8]
unByte (Byte val) = Just val
unByte _          = Nothing

invalidEnum    :: BaseTypeValue
invalidEnum    = Enum    0 

invalidSInt8   :: BaseTypeValue
invalidSInt8   = SInt8   127 

invalidUInt8   :: BaseTypeValue
invalidUInt8   = UInt8   255 

invalidSInt16  :: BaseTypeValue
invalidSInt16  = SInt16  32767 

invalidUInt16  :: BaseTypeValue
invalidUInt16  = UInt16  65535 

invalidSInt32  :: BaseTypeValue
invalidSInt32  = SInt32  2147483647 

invalidUInt32  :: BaseTypeValue
invalidUInt32  = UInt32  4294967295 

invalidString  :: BaseTypeValue
invalidString  = String  []

invalidFloat32  :: BaseTypeValue
invalidFloat32 = Float32 4294967295           

invalidFloat64  :: BaseTypeValue
invalidFloat64 = Float64 18446744073709551615 

invalidUInt8z  :: BaseTypeValue
invalidUInt8z  = UInt8z  0   

invalidUInt16z  :: BaseTypeValue
invalidUInt16z = UInt16z 0   

invalidUInt32z  :: BaseTypeValue
invalidUInt32z = UInt32z 0   

invalidByte  :: BaseTypeValue
invalidByte    = Byte    $ repeat 255

