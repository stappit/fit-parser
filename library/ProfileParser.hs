module ProfileParser where

import Data.Char (chr)
import Data.Bits       -- bit operations
import Data.Word       -- unsigned ints
import Data.Int
import Control.Monad.Except
import Control.Monad.State

import Fit
import Parser
import Profiles
import WordParsers
import Timestamp
import Enum
import BaseType

import qualified Types as T

profileP :: DefinitionMessage -> Parser Profile
profileP (Def _ arch gMsg defs)
  | gMsg == 0   = liftM FileIDProfile      $ mapM (fileIDP arch) defs
  {-| gMsg == 18  = liftM SessionProfile     $ mapM (sessionP arch) defs-}
  | gMsg == 19  = liftM LapProfile         $ mapM (lapP arch) defs
  | gMsg == 20  = liftM RecProfile         $ mapM (recordP arch) defs
  | gMsg == 21  = liftM EventProfile       $ mapM (eventP  arch) defs             
  | gMsg == 23  = liftM DeviceInfoProfile  $ mapM (deviceInfoP arch) defs         
  {-| gMsg == 34  = liftM ActivityProfile    $ mapM (activityP arch) defs         -}
  | gMsg == 49  = liftM FileCreatorProfile $ mapM (fileCreatorP arch) defs         
  {-| gMsg == 113 = liftM ???                $ mapM (             arch) defs         -}
  | otherwise   = mapM_ (baseTypeValueP arch) defs >> return NoProfile

fileIDP :: Arch -> FieldDefinition -> Parser FileID
fileIDP arch fDef@(FieldDef fNum sz _)
  | fNum == 0 = liftM (maybe NoFileID Type) enumP
  | fNum == 1 = liftM (maybe NoFileID Manufacturer) $ enum16P arch
  | fNum == 2 = liftM (maybe NoFileID Product) $ uInt16P arch
  | fNum == 3 = liftM (maybe NoFileID Serial) $ uInt32zP arch
  | fNum == 4 = liftM (maybe NoFileID $ TimeCreated . DateTime) $ uInt32P arch
  | fNum == 5 = liftM (maybe NoFileID Number) $ uInt16P arch
  | fNum == 8 = liftM (maybe NoFileID ProductName) $ stringP sz
  | otherwise = liftM (NoFileID' fNum) $ baseTypeValueP arch fDef

recordP :: Arch -> FieldDefinition -> Parser Record
recordP arch fDef@(FieldDef fNum sz _)
  | fNum == 253 = liftM (maybe NoRecord $ RecTimestamp . DateTime) $ uInt32P arch
  | fNum == 0   = liftM (maybe NoRecord RecPositionLat) $ sInt32P arch
  | fNum == 1   = liftM (maybe NoRecord RecPositionLong) $ sInt32P arch
  | fNum == 2   = liftM (maybe NoRecord $ RecAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
  | fNum == 3   = liftM (maybe NoRecord RecHeartRate) uInt8P
  | fNum == 4   = liftM (maybe NoRecord RecCadence) uInt8P
  | fNum == 5   = liftM (maybe NoRecord $ RecDistance . scaleBy 100) $ uInt32P arch
  | fNum == 6   = liftM (maybe NoRecord $ RecSpeed . scaleBy 1000) $ uInt16P arch
  | fNum == 7   = liftM (maybe NoRecord RecPower) $ uInt16P arch
  | fNum == 8   = liftM (maybe NoRecord $ uncurry RecCompressedSpeedDistance . byteToCSD) $ byteP sz
  | fNum == 9   = liftM (maybe NoRecord $ RecGrade . scaleBy 100) $ sInt16P arch
  | fNum == 10  = liftM (maybe NoRecord RecResistance) uInt8P
  | fNum == 11  = liftM (maybe NoRecord $ RecTimeFromCourse . scaleBy 1000) $ sInt32P arch
  | fNum == 12  = liftM (maybe NoRecord $ RecCycleLength . scaleBy 100) uInt8P
  | fNum == 13  = liftM (maybe NoRecord RecTemperature) sInt8P
  | fNum == 17  = liftM (maybe NoRecord $ RecSpeed1s . scaleBy 16) uInt8P
  | fNum == 18  = liftM (maybe NoRecord RecCycles) uInt8P
  | fNum == 19  = liftM (maybe NoRecord RecTotalCycles) $ uInt32P arch
  | fNum == 28  = liftM (maybe NoRecord RecCompressedAccumulatedPower) $ uInt16P arch
  | fNum == 29  = liftM (maybe NoRecord RecAccumulatedPower) $ uInt32P arch
  {-| fNum == 30  = liftM RecLeftRightBalance enumP-}
  | fNum == 31  = liftM (maybe NoRecord RecGpsAccuracy) uInt8P
  | fNum == 32  = liftM (maybe NoRecord $ RecVerticalSpeed . scaleBy 1000) $ sInt16P arch
  | fNum == 33  = liftM (maybe NoRecord RecCalories) $ uInt16P arch
  | fNum == 39  = liftM (maybe NoRecord $ RecVerticalOscillation . scaleBy 10) $ uInt16P arch
  | fNum == 40  = liftM (maybe NoRecord $ RecStanceTimePercent . scaleBy 100) $ uInt16P arch
  | fNum == 41  = liftM (maybe NoRecord $ RecStanceTime . scaleBy 10) $ uInt16P arch
  {-| fNum == 42  = liftM RecActivityType esIntP-}
  | fNum == 43  = liftM (maybe NoRecord $ RecLeftTorqueEffectiveness . scaleBy 2) uInt8P
  | fNum == 44  = liftM (maybe NoRecord $ RecRightTorqueEffectiveness . scaleBy 2) uInt8P
  | fNum == 45  = liftM (maybe NoRecord $ RecLeftPedalSmoothness     . scaleBy 2) uInt8P
  | fNum == 46  = liftM (maybe NoRecord $ RecRightPedalSmoothness    . scaleBy 2) uInt8P
  | fNum == 47  = liftM (maybe NoRecord $ RecCombinedPedalSmoothness . scaleBy 2) uInt8P
  | fNum == 48  = liftM (maybe NoRecord $ RecTime128                 . scaleBy 128) uInt8P
  {-| fNum == 49  = liftM RecStrokeType esIntP-}
  | fNum == 50  = liftM (maybe NoRecord RecZone) uInt8P
  | fNum == 51  = liftM (maybe NoRecord $ RecBallSpeed               . scaleBy 100) $ uInt16P arch
  | fNum == 52  = liftM (maybe NoRecord $ RecCadence256              . scaleBy 256) $ uInt16P arch
  | fNum == 53  = liftM (maybe NoRecord $ RecFractionalCadence       . scaleBy 128) uInt8P
  | fNum == 54  = liftM (maybe NoRecord $ RecTotalHemoglobinConc     . scaleBy 100) $ uInt16P arch
  | fNum == 55  = liftM (maybe NoRecord $ RecTotalHemoglobinConcMin  . scaleBy 100) $ uInt16P arch
  | fNum == 56  = liftM (maybe NoRecord $ RecTotalHemoglobinConcMax  . scaleBy 100) $ uInt16P arch
  | fNum == 57  = liftM (maybe NoRecord $ RecSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
  | fNum == 58  = liftM (maybe NoRecord $ RecSaturatedHemoglobinPercentMin. scaleBy 10) $ uInt16P arch
  | fNum == 59  = liftM (maybe NoRecord $ RecSaturatedHemoglobinPercentMax. scaleBy 10) $ uInt16P arch
  | fNum == 62  = liftM (maybe NoRecord $ RecDeviceIndex . T.Creator) uInt8P
  | fNum == 67  = liftM (maybe NoRecord RecLeftPco) sInt8P
  | fNum == 68  = liftM (maybe NoRecord RecRightPco) sInt8P
  | fNum == 69  = liftM (maybe NoRecord $ RecLeftPowerPhase . scaleBy 0.7111111) uInt8P
  | fNum == 70  = liftM (maybe NoRecord $ RecLeftPowerPhasePeak      . scaleBy 0.7111111) uInt8P
  | fNum == 71  = liftM (maybe NoRecord $ RecRightPowerPhase         . scaleBy 0.7111111) uInt8P
  | fNum == 72  = liftM (maybe NoRecord $ RecRightPowerPhasePeak     . scaleBy 0.7111111) uInt8P
  | fNum == 73  = liftM (maybe NoRecord $ RecEnhancedSpeed           . scaleBy 1000) $ uInt32P arch
  | fNum == 78  = liftM (maybe NoRecord $ RecEnhancedAltitude        . translateBy 500 . scaleBy 5) $ uInt32P arch
  | fNum == 81  = liftM (maybe NoRecord $ RecBatterySoc . scaleBy 2) uInt8P
  | fNum == 82  = liftM (maybe NoRecord RecMotorPower) $ uInt16P arch
  {-| otherwise = baseTypeValueP arch fDef >> return NoRecord-}
  | otherwise = liftM (NoRecord' fNum) $ baseTypeValueP arch fDef

eventP :: Arch -> FieldDefinition -> Parser Event
eventP arch fDef@(FieldDef fNum sz _)
  | fNum == 253 = liftM (maybe NoEvent $ EventTimestamp . DateTime) $ uInt32P arch
  | fNum == 0   = liftM (maybe NoEvent Event) enumP
  | fNum == 1   = liftM (maybe NoEvent EventType) enumP
  | fNum == 2   = liftM Data16 $ word16P arch
  {-| fNum == 3   = liftM Data $ word32P arch-}
  | fNum == 4   = liftM EventGroup word8P
  | fNum == 7   = liftM Score $ word16P arch
  | fNum == 8   = liftM OpponentScore $ word16P arch
  | fNum == 9   = liftM FrontGearNum word8P
  | fNum == 10  = liftM FrontGear word8P
  | fNum == 11  = liftM RearGearNum word8P
  | fNum == 12  = liftM RearGear word8P
  | fNum == 13  = liftM (DeviceIndex . T.Creator) word8P
  {-| otherwise = baseTypeValueP arch fDef >> return NoEvent-}
  | otherwise = liftM (NoEvent' fNum) $ baseTypeValueP arch fDef

deviceInfoP :: Arch -> FieldDefinition -> Parser DeviceInfo
deviceInfoP arch fDef@(FieldDef fNum sz _)
    | fNum == 253 = liftM (maybe NoDeviceInfo $ DITimestamp . DateTime) $ uInt32P arch
    | fNum == 0   = liftM (DIDeviceIndex . T.Creator) word8P
    {-[>| fNum == 1   = liftM DIDeviceType word8P<]-} -- dynamic
    | fNum == 2   = liftM (maybe NoDeviceInfo DIManufacturer) $ enum16P arch
    | fNum == 3   = liftM (maybe NoDeviceInfo DISerialNumber) $ uInt32zP arch
    | fNum == 4   = liftM (maybe NoDeviceInfo DIProduct) $ uInt16P arch -- dynamic
    | fNum == 5   = liftM (maybe NoDeviceInfo $ DISoftwareVersion . scaleBy 100) $ uInt16P arch
    | fNum == 6   = liftM (maybe NoDeviceInfo DIHardwareVersion) uInt8P
    | fNum == 7   = liftM (maybe NoDeviceInfo DICumOperatingTime) $ uInt32P arch
    | fNum == 10  = liftM (maybe NoDeviceInfo $ DIBatteryVoltage . scaleBy 256) $ uInt16P arch
    | fNum == 11  = liftM (maybe NoDeviceInfo DIBatteryStatus) enumP
    | fNum == 18  = liftM (maybe NoDeviceInfo DISensorPosition) enumP
    | fNum == 19  = liftM (maybe NoDeviceInfo DIDescriptor) $ stringP sz
    | fNum == 20  = liftM (maybe NoDeviceInfo DIAntTransmissionType) uInt8zP
    | fNum == 21  = liftM (maybe NoDeviceInfo DIAntDeviceNumber) $ uInt16zP arch
    | fNum == 22  = liftM (maybe NoDeviceInfo DIAntNetwork) enumP
    | fNum == 25  = liftM (maybe NoDeviceInfo DISourceType) enumP
    | fNum == 27  = liftM (maybe NoDeviceInfo DIProductName) $ stringP sz
    {-| otherwise   = baseTypeValueP arch fDef >> return NoDeviceInfo-}
    | otherwise = liftM (NoDeviceInfo' fNum) $ baseTypeValueP arch fDef

fileCreatorP :: Arch -> FieldDefinition -> Parser FileCreator
fileCreatorP arch (FieldDef fNum sz _)
    | fNum == 0 = liftM SoftwareVersion $ word16P arch
    | fNum == 1 = liftM HardwareVersion word8P
    | otherwise = throwError undefined

lapP :: Arch -> FieldDefinition -> Parser Lap
lapP arch fDef@(FieldDef fNum sz _)
    {-| fNum == 254 = liftM (maybe NoLap LapMessageIndex) $ enum16P arch-}
    | fNum == 253 = liftM (maybe NoLap $ LapTimestamp . DateTime) $ uInt32P arch
    {-| fNum == 0   = liftM (maybe NoLap LapEvent) enumP-}
    {-| fNum == 1   = liftM (maybe NoLap LapEventType) enumP-}
    | fNum == 2   = liftM (maybe NoLap $ LapStartTime . DateTime) $ uInt32P arch
    | fNum == 3   = liftM (maybe NoLap LapStartPositionLat) $ sInt32P arch
    | fNum == 4   = liftM (maybe NoLap LapStartPositionLong) $ sInt32P arch
    | fNum == 5   = liftM (maybe NoLap LapEndPositionLat) $ sInt32P arch
    | fNum == 6   = liftM (maybe NoLap LapEndPositionLong) $ sInt32P arch
    | fNum == 7   = liftM (maybe NoLap $ LapTotalElapsedTime . scaleBy 1000) $ uInt32P arch
    | fNum == 8   = liftM (maybe NoLap $ LapTotalTimerTime   . scaleBy 1000) $ uInt32P arch
    | fNum == 9   = liftM (maybe NoLap $ LapTotalDistance    . scaleBy 100)  $ uInt32P arch
    {-| fNum == 10  = liftM LapTotalCycles                   -}  -- dynamic
    | fNum == 11  = liftM (maybe NoLap LapTotalCalories) $ uInt16P arch
    | fNum == 12  = liftM (maybe NoLap LapTotalFatCalories) $ uInt16P arch
    | fNum == 13  = liftM (maybe NoLap $ LapAvgSpeed . scaleBy 1000) $ uInt16P arch
    | fNum == 14  = liftM (maybe NoLap $ LapMaxSpeed . scaleBy 1000) $ uInt16P arch
    | fNum == 15  = liftM (maybe NoLap LapAvgHeartRate) uInt8P
    | fNum == 16  = liftM (maybe NoLap LapMaxHeartRate) uInt8P
    {-| fNum == 17  = liftM LapAvgCadence                     dynamic-}
    {-| fNum == 18  = liftM LapMaxCadence                     dynamic-}
    | fNum == 19  = liftM (maybe NoLap LapAvgPower) $ uInt16P arch
    | fNum == 20  = liftM (maybe NoLap LapMaxPower) $ uInt16P arch
    | fNum == 21  = liftM (maybe NoLap LapTotalAscent) $ uInt16P arch
    | fNum == 22  = liftM (maybe NoLap LapTotalDescent) $ uInt16P arch
    {-| fNum == 23  = liftM (maybe NoLap LapIntensity) enumP-}
    {-| fNum == 24  = liftM (maybe NoLap LapLapTrigger) enumP-}
    {-| fNum == 25  = liftM (maybe NoLap LapSport) enumP-}
    | fNum == 26  = liftM (maybe NoLap LapEventGroup) uInt8P
    | fNum == 32  = liftM (maybe NoLap LapNumLengths) $ uInt16P arch
    | fNum == 33  = liftM (maybe NoLap LapNormalizedPower) $ uInt16P arch
    {-| fNum == 34  = liftM (maybe NoLap LapLeftRightBalance) $ enum16P arch-}
    | fNum == 35  = liftM (maybe NoLap LapFirstLengthIndex) $ uInt16P arch
    | fNum == 37  = liftM (maybe NoLap $ LapAvgStrokeDistance . scaleBy 100) $ uInt16P arch
    {-| fNum == 38  = liftM (maybe NoLap LapSwimStroke) enumP-}
    {-| fNum == 39  = liftM (maybe NoLap LapSubSport) enumP-}
    | fNum == 40  = liftM (maybe NoLap LapNumActiveLengths) $ uInt16P arch
    | fNum == 41  = liftM (maybe NoLap LapTotalWork) $ uInt32P arch
    | fNum == 42  = liftM (maybe NoLap $ LapAvgAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    | fNum == 43  = liftM (maybe NoLap $ LapMaxAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    | fNum == 44  = liftM (maybe NoLap LapGpsAccuracy) uInt8P
    | fNum == 45  = liftM (maybe NoLap $ LapAvgGrade . scaleBy 100) $ sInt16P arch
    | fNum == 46  = liftM (maybe NoLap $ LapAvgPosGrade . scaleBy 100) $ sInt16P arch
    | fNum == 47  = liftM (maybe NoLap $ LapAvgNegGrade . scaleBy 100) $ sInt16P arch
    | fNum == 48  = liftM (maybe NoLap $ LapMaxPosGrade . scaleBy 100) $ sInt16P arch
    | fNum == 49  = liftM (maybe NoLap $ LapMaxNegGrade . scaleBy 100) $ sInt16P arch
    | fNum == 50  = liftM (maybe NoLap LapAvgTemperature) sInt8P
    | fNum == 51  = liftM (maybe NoLap LapMaxTemperature) sInt8P
    | fNum == 52  = liftM (maybe NoLap $ LapTotalMovingTime . scaleBy 1000) $ uInt16P arch
    | fNum == 53  = liftM (maybe NoLap $ LapAvgPosVerticalSpeed . scaleBy 1000) $ sInt16P arch
    | fNum == 54  = liftM (maybe NoLap $ LapAvgNegVerticalSpeed . scaleBy 1000) $ sInt16P arch
    | fNum == 55  = liftM (maybe NoLap $ LapMaxPosVerticalSpeed . scaleBy 1000) $ sInt16P arch
    | fNum == 56  = liftM (maybe NoLap $ LapMaxNegVerticalSpeed . scaleBy 1000) $ sInt16P arch
    | fNum == 57  = liftM (maybe NoLap $ LapTimeInHRZone      . scaleBy 1000) $ uInt32P arch
    | fNum == 58  = liftM (maybe NoLap $ LapTimeInSpeedZone   . scaleBy 1000) $ uInt32P arch
    | fNum == 59  = liftM (maybe NoLap $ LapTimeInCadenceZone . scaleBy 1000) $ uInt32P arch
    | fNum == 60  = liftM (maybe NoLap $ LapTimeInPowerZone   . scaleBy 1000) $ uInt32P arch
    | fNum == 61  = liftM (maybe NoLap LapRepetitionNum) $ uInt16P arch
    | fNum == 62  = liftM (maybe NoLap $ LapMinAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    | fNum == 63  = liftM (maybe NoLap LapMinHeartRate) uInt8P
    {-| fNum == 71  = liftM (maybe NoLap LapWktStepIndex) $ enum16P arch-}
    | fNum == 74  = liftM (maybe NoLap LapOpponentScore) $ uInt16P arch
    | fNum == 75  = liftM (maybe NoLap LapStrokeCount) $ uInt16P arch
    | fNum == 76  = liftM (maybe NoLap LapZoneCount) $ uInt16P arch
    | fNum == 77  = liftM (maybe NoLap $ LapAvgVerticalOscillation . scaleBy 10) $ uInt16P arch
    | fNum == 78  = liftM (maybe NoLap $ LapAvgStanceTimePercent . scaleBy 100) $ uInt16P arch
    | fNum == 79  = liftM (maybe NoLap $ LapAvgStanceTime . scaleBy 10) $ uInt16P arch
    | fNum == 80  = liftM (maybe NoLap $ LapAvgFractionalCadence . scaleBy 128) uInt8P
    | fNum == 81  = liftM (maybe NoLap $ LapMaxFractionalCadence . scaleBy 128) uInt8P
    | fNum == 82  = liftM (maybe NoLap $ LapTotalFractionalCycles . scaleBy 128) uInt8P
    | fNum == 83  = liftM (maybe NoLap LapPlayerScore) $ uInt16P arch
    | fNum == 84  = liftM (maybe NoLap $ LapAvgTotalHemoglobinConc . scaleBy 100) $ uInt16P arch
    | fNum == 85  = liftM (maybe NoLap $ LapMinTotalHemoglobinConc . scaleBy 100) $ uInt16P arch
    | fNum == 86  = liftM (maybe NoLap $ LapMaxTotalHemoglobinConc . scaleBy 100) $ uInt16P arch
    | fNum == 87  = liftM (maybe NoLap $ LapAvgSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    | fNum == 88  = liftM (maybe NoLap $ LapMinSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    | fNum == 89  = liftM (maybe NoLap $ LapMaxSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    | fNum == 91  = liftM (maybe NoLap $ LapAvgLeftTorqueEffectiveness  . scaleBy 2) uInt8P
    | fNum == 92  = liftM (maybe NoLap $ LapAvgRightTorqueEffectiveness . scaleBy 2) uInt8P
    | fNum == 93  = liftM (maybe NoLap $ LapAvgLeftPedalSmoothness      . scaleBy 2) uInt8P
    | fNum == 94  = liftM (maybe NoLap $ LapAvgRightPedalSmoothness     . scaleBy 2) uInt8P
    | fNum == 95  = liftM (maybe NoLap $ LapAvgCombinedPedalSmoothness  . scaleBy 2) uInt8P
    | fNum == 98  = liftM (maybe NoLap $ LapTimeStanding . scaleBy 1000) $ uInt32P arch
    | fNum == 99  = liftM (maybe NoLap LapStandCount) $ uInt16P arch
    | fNum == 100 = liftM (maybe NoLap LapAvgLeftPco)  sInt8P
    | fNum == 101 = liftM (maybe NoLap LapAvgRightPco) sInt8P
    | fNum == 102 = liftM (maybe NoLap $ LapAvgLeftPowerPhase      . scaleBy 0.7111111) uInt8P
    | fNum == 103 = liftM (maybe NoLap $ LapAvgLeftPowerPhasePeak  . scaleBy 0.7111111) uInt8P
    | fNum == 104 = liftM (maybe NoLap $ LapAvgRightPowerPhase     . scaleBy 0.7111111) uInt8P
    | fNum == 105 = liftM (maybe NoLap $ LapAvgRightPowerPhasePeak . scaleBy 0.7111111) uInt8P
    | fNum == 106 = liftM (maybe NoLap LapAvgPowerPosition) $ uInt16P arch
    | fNum == 107 = liftM (maybe NoLap LapMaxPowerPosition) $ uInt16P arch
    | fNum == 108 = liftM (maybe NoLap LapAvgCadencePosition) uInt8P
    | fNum == 109 = liftM (maybe NoLap LapMaxCadencePosition) uInt8P
    | fNum == 110 = liftM (maybe NoLap $ LapEnhancedAvgSpeed . scaleBy 1000) $ uInt32P arch
    | fNum == 111 = liftM (maybe NoLap $ LapEnhancedMaxSpeed . scaleBy 1000) $ uInt32P arch
    | fNum == 112 = liftM (maybe NoLap $ LapEnhancedAvgAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    | fNum == 113 = liftM (maybe NoLap $ LapEnhancedMinAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    | fNum == 114 = liftM (maybe NoLap $ LapEnhancedMaxAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    | fNum == 115 = liftM (maybe NoLap LapAvgLevMotorPower) $ uInt16P arch
    | fNum == 116 = liftM (maybe NoLap LapMaxLevMotorPower) $ uInt16P arch
    | fNum == 117 = liftM (maybe NoLap $ LapLevBatteryConsumption . scaleBy 2) uInt8P
    {-| otherwise   = baseTypeValueP arch fDef >> return NoLap-}
    | otherwise = liftM (NoLap' fNum) $ baseTypeValueP arch fDef

baseTypeValueP :: Arch -> FieldDefinition -> Parser BaseTypeValue
baseTypeValueP arch fDef = case fDef of
  FieldDef fNum _    0  -> liftM Enum    word8P
  FieldDef fNum _    1  -> liftM SInt8   num8P
  FieldDef fNum _    2  -> liftM UInt8   word8P
  FieldDef fNum _    3  -> liftM SInt16  $ num16P arch
  FieldDef fNum _    4  -> liftM UInt16  $ word16P arch
  FieldDef fNum _    5  -> liftM SInt32  $ num32P arch
  FieldDef fNum _    6  -> do w <- word32P arch
                              _ <- when (fNum == 253) $ modify $ setTimestamp $ Timestamp w
                              return $ UInt32 w
  FieldDef fNum size 7  -> liftM String  $ replicateM size word8P
  FieldDef fNum _    8  -> liftM Float32 $ num32P arch
  FieldDef fNum _    9  -> liftM Float64 $ num64P arch
  FieldDef fNum _    10 -> liftM UInt8z  word8P
  FieldDef fNum _    11 -> liftM UInt16z $ word16P arch
  FieldDef fNum _    12 -> liftM UInt32z $ word32P arch
  FieldDef fNum size 13 -> liftM Byte    $ replicateM size word8P
  FieldDef _    _    bt -> throwError $ InvalidBasetype bt

bytesToCSD :: Word8 -> Word8 -> Word8 -> (Float, Float)
bytesToCSD b1 b2 b3 = (speed, distance)
  where
    speed    = scaleBy 100 $ (b1' `shiftL` 4) `xor` b2Speed
    distance = scaleBy 16  $ (b2Dist `shiftL` 8) `xor` b3'
    b2Speed = b2' .&. speedMask
    b2Dist  = b2' .&. distanceMask
    speedMask    = 0x000F
    distanceMask = 0xFFF0
    b1' = fromIntegral b1 :: Word16
    b2' = fromIntegral b2 :: Word16
    b3' = fromIntegral b3 :: Word16

byteToCSD :: [Word8] -> (Float, Float)
byteToCSD bs = let invalid = (-1, -1) in case bs of
  (b1:b2:b3:_) -> bytesToCSD b1 b2 b3
  _            -> invalid

scaleBy :: Integral a => Float -> a -> Float
scaleBy s x = fromIntegral x / s

translateBy :: Num a => a -> a -> a
translateBy = flip (-)

stringP :: Int -> Parser (Maybe String)
stringP n = do
    ws <- replicateM n word8P
    if 0 `elem` ws
      then return Nothing
      else return . Just $ map (chr . fromIntegral) ws

byteP :: Int -> Parser (Maybe [Word8])
byteP n = do
    bs <- replicateM n word8P
    if 0xFF `elem` bs
      then return Nothing
      else return $ Just bs

enumP :: Enum' a => Parser (Maybe a)
enumP = do
    w <- word8P
    let w' = fromIntegral w
    if w == 0xFF
      then return Nothing
      else maybe (throwError $ NoEnum w') (return . Just) (toEnum' w')

enum16P :: Enum' a => Arch -> Parser (Maybe a)
enum16P arch = do
    w <- word16P arch
    if w == 0xFF
      then return Nothing
      else maybe (throwError $ NoEnum w) (return . Just) (toEnum' w)

uInt8P :: Parser (Maybe Word8)
uInt8P = do
    w <- word8P
    if w == 0xFF
      then return Nothing
      else return $ Just w

uInt8zP :: Parser (Maybe Word8)
uInt8zP = do
    w <- word8P
    if w == 0
      then return Nothing
      else return $ Just w

sInt8P :: Parser (Maybe Int8)
sInt8P = do
    w <- num8P
    if w == 0x7F
      then return Nothing
      else return $ Just w

uInt16P :: Arch -> Parser (Maybe Word16)
uInt16P arch = do
    w <- word16P arch
    if w == 0xFFFF
      then return Nothing
      else return $ Just w

uInt16zP :: Arch -> Parser (Maybe Word16)
uInt16zP arch = do
    w <- word16P arch
    if w == 0
      then return Nothing
      else return $ Just w

sInt16P :: Arch -> Parser (Maybe Int16)
sInt16P arch = do
    w <- num16P arch
    if w == 0x7FFF
      then return Nothing
      else return $ Just w

uInt32P :: Arch -> Parser (Maybe Word32)
uInt32P arch = do
    w <- word32P arch
    if w == 0xFFFFFFFF
      then return Nothing
      else return $ Just w

uInt32zP :: Arch -> Parser (Maybe Word32)
uInt32zP arch = do
    w <- word32P arch
    if w == 0
      then return Nothing
      else return $ Just w

sInt32P :: Arch -> Parser (Maybe Int32)
sInt32P arch = do
    w <- num32P arch
    if w == 0x7FFFFFFF
      then return Nothing
      else return $ Just w

dateTimeP :: Arch -> Parser (Maybe DateTime)
dateTimeP arch = do
    w <- uInt32P arch
    return $ fmap DateTime w
