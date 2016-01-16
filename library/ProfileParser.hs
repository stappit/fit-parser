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
profileP (Def _ arch gNum defs) = profP `catchError` handler
  where
    handler (InvalidFieldNum' fNum) = throwError $ InvalidFieldNum gNum fNum
    handler e                       = throwError e
    profP = case gNum of
        GNum 0   -> liftM FileIDProfile      $ mapM (fileIDP arch) defs
        GNum 18  -> liftM SessionProfile     $ mapM (sessionP arch) defs
        GNum 19  -> liftM LapProfile         $ mapM (lapP arch) defs
        GNum 20  -> liftM RecProfile         $ mapM (recordP arch) defs
        GNum 21  -> liftM EventProfile       $ mapM (eventP  arch) defs             
        GNum 23  -> liftM DeviceInfoProfile  $ mapM (deviceInfoP arch) defs         
        GNum 34  -> liftM ActivityProfile    $ mapM (activityP arch) defs         
        GNum 49  -> liftM FileCreatorProfile $ mapM (fileCreatorP arch) defs         
        {-GNum 113 -> liftM ???                $ mapM (             arch) defs         -}
        _       -> mapM_ (baseTypeValueP arch) defs >> return NoProfile

fileIDP :: Arch -> FieldDefinition -> Parser FileID
fileIDP arch fDef@(FieldDef fNum sz _) = case fNum of
    FNum 0 -> liftM (maybe NoFileID Type) enumP
    FNum 1 -> liftM (maybe NoFileID Manufacturer) $ enum16P arch
    FNum 2 -> liftM (maybe NoFileID Product) $ uInt16P arch
    FNum 3 -> liftM (maybe NoFileID Serial) $ uInt32zP arch
    FNum 4 -> liftM (maybe NoFileID $ TimeCreated . DateTime) $ uInt32P arch
    FNum 5 -> liftM (maybe NoFileID Number) $ uInt16P arch
    FNum 8 -> liftM (maybe NoFileID ProductName) $ stringP sz
    _      -> throwError $ InvalidFieldNum' fNum

recordP :: Arch -> FieldDefinition -> Parser Record
recordP arch fDef@(FieldDef fNum sz _) = case fNum of
    FNum 253 -> liftM (maybe NoRecord $ RecTimestamp . DateTime) $ uInt32P arch
    FNum 0   -> liftM (maybe NoRecord RecPositionLat) $ sInt32P arch
    FNum 1   -> liftM (maybe NoRecord RecPositionLong) $ sInt32P arch
    FNum 2   -> liftM (maybe NoRecord $ RecAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 3   -> liftM (maybe NoRecord RecHeartRate) uInt8P
    FNum 4   -> liftM (maybe NoRecord RecCadence) uInt8P
    FNum 5   -> liftM (maybe NoRecord $ RecDistance . scaleBy 100) $ uInt32P arch
    FNum 6   -> liftM (maybe NoRecord $ RecSpeed . scaleBy 1000) $ uInt16P arch
    FNum 7   -> liftM (maybe NoRecord RecPower) $ uInt16P arch
    FNum 8   -> liftM (maybe NoRecord $ uncurry RecCompressedSpeedDistance . byteToCSD) $ byteP sz
    FNum 9   -> liftM (maybe NoRecord $ RecGrade . scaleBy 100) $ sInt16P arch
    FNum 10  -> liftM (maybe NoRecord RecResistance) uInt8P
    FNum 11  -> liftM (maybe NoRecord $ RecTimeFromCourse . scaleBy 1000) $ sInt32P arch
    FNum 12  -> liftM (maybe NoRecord $ RecCycleLength . scaleBy 100) uInt8P
    FNum 13  -> liftM (maybe NoRecord RecTemperature) sInt8P
    FNum 17  -> liftM (maybe NoRecord $ RecSpeed1s . scaleBy 16) uInt8P
    FNum 18  -> liftM (maybe NoRecord RecCycles) uInt8P
    FNum 19  -> liftM (maybe NoRecord RecTotalCycles) $ uInt32P arch
    FNum 28  -> liftM (maybe NoRecord RecCompressedAccumulatedPower) $ uInt16P arch
    FNum 29  -> liftM (maybe NoRecord RecAccumulatedPower) $ uInt32P arch
    FNum 30  -> liftM (maybe NoRecord $ RecLeftRightBalance . T.mkLRB) uInt8P 
    FNum 31  -> liftM (maybe NoRecord RecGpsAccuracy) uInt8P
    FNum 32  -> liftM (maybe NoRecord $ RecVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 33  -> liftM (maybe NoRecord RecCalories) $ uInt16P arch
    FNum 39  -> liftM (maybe NoRecord $ RecVerticalOscillation . scaleBy 10) $ uInt16P arch
    FNum 40  -> liftM (maybe NoRecord $ RecStanceTimePercent . scaleBy 100) $ uInt16P arch
    FNum 41  -> liftM (maybe NoRecord $ RecStanceTime . scaleBy 10) $ uInt16P arch
    FNum 42  -> liftM (maybe NoRecord RecActivityType) enumP
    FNum 43  -> liftM (maybe NoRecord $ RecLeftTorqueEffectiveness . scaleBy 2) uInt8P
    FNum 44  -> liftM (maybe NoRecord $ RecRightTorqueEffectiveness . scaleBy 2) uInt8P
    FNum 45  -> liftM (maybe NoRecord $ RecLeftPedalSmoothness     . scaleBy 2) uInt8P
    FNum 46  -> liftM (maybe NoRecord $ RecRightPedalSmoothness    . scaleBy 2) uInt8P
    FNum 47  -> liftM (maybe NoRecord $ RecCombinedPedalSmoothness . scaleBy 2) uInt8P
    FNum 48  -> liftM (maybe NoRecord $ RecTime128                 . scaleBy 128) uInt8P
    FNum 49  -> liftM (maybe NoRecord RecStrokeType) enumP
    FNum 50  -> liftM (maybe NoRecord RecZone) uInt8P
    FNum 51  -> liftM (maybe NoRecord $ RecBallSpeed               . scaleBy 100) $ uInt16P arch
    FNum 52  -> liftM (maybe NoRecord $ RecCadence256              . scaleBy 256) $ uInt16P arch
    FNum 53  -> liftM (maybe NoRecord $ RecFractionalCadence       . scaleBy 128) uInt8P
    FNum 54  -> liftM (maybe NoRecord $ RecTotalHemoglobinConc     . scaleBy 100) $ uInt16P arch
    FNum 55  -> liftM (maybe NoRecord $ RecTotalHemoglobinConcMin  . scaleBy 100) $ uInt16P arch
    FNum 56  -> liftM (maybe NoRecord $ RecTotalHemoglobinConcMax  . scaleBy 100) $ uInt16P arch
    FNum 57  -> liftM (maybe NoRecord $ RecSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    FNum 58  -> liftM (maybe NoRecord $ RecSaturatedHemoglobinPercentMin . scaleBy 10) $ uInt16P arch
    FNum 59  -> liftM (maybe NoRecord $ RecSaturatedHemoglobinPercentMax . scaleBy 10) $ uInt16P arch
    FNum 62  -> liftM (maybe NoRecord $ RecDeviceIndex . T.Creator) uInt8P
    FNum 67  -> liftM (maybe NoRecord RecLeftPco) sInt8P
    FNum 68  -> liftM (maybe NoRecord RecRightPco) sInt8P
    FNum 69  -> liftM (maybe NoRecord $ RecLeftPowerPhase      . scaleBy 0.7111111) uInt8P
    FNum 70  -> liftM (maybe NoRecord $ RecLeftPowerPhasePeak  . scaleBy 0.7111111) uInt8P
    FNum 71  -> liftM (maybe NoRecord $ RecRightPowerPhase     . scaleBy 0.7111111) uInt8P
    FNum 72  -> liftM (maybe NoRecord $ RecRightPowerPhasePeak . scaleBy 0.7111111) uInt8P
    FNum 73  -> liftM (maybe NoRecord $ RecEnhancedSpeed . scaleBy 1000) $ uInt32P arch
    FNum 78  -> liftM (maybe NoRecord $ RecEnhancedAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 81  -> liftM (maybe NoRecord $ RecBatterySoc . scaleBy 2) uInt8P
    FNum 82  -> liftM (maybe NoRecord RecMotorPower) $ uInt16P arch
    _        -> throwError $ InvalidFieldNum' fNum

eventP :: Arch -> FieldDefinition -> Parser Event
eventP arch fDef@(FieldDef fNum _ _) = case fNum of
    FNum 253 -> liftM (maybe NoEvent $ EventTimestamp . DateTime) $ uInt32P arch
    FNum 0   -> liftM (maybe NoEvent Event) enumP
    FNum 1   -> liftM (maybe NoEvent EventType) enumP
    FNum 2   -> liftM Data16 $ word16P arch
    {-FNum 3   -> liftM Data $ word32P arch-}
    FNum 4   -> liftM EventGroup word8P
    FNum 7   -> liftM Score $ word16P arch
    FNum 8   -> liftM OpponentScore $ word16P arch
    FNum 9   -> liftM FrontGearNum word8P
    FNum 10  -> liftM FrontGear word8P
    FNum 11  -> liftM RearGearNum word8P
    FNum 12  -> liftM RearGear word8P
    FNum 13  -> liftM (DeviceIndex . T.Creator) word8P
    {-| otherwise = baseTypeValueP arch fDef >> return NoEvent-}
    FNum fNum' -> liftM (NoEvent' fNum') $ baseTypeValueP arch fDef

deviceInfoP :: Arch -> FieldDefinition -> Parser DeviceInfo
deviceInfoP arch fDef@(FieldDef fNum sz _) = case fNum of
    FNum 253 -> liftM (maybe NoDeviceInfo $ DITimestamp . DateTime) $ uInt32P arch
    FNum 0   -> liftM (DIDeviceIndex . T.Creator) word8P
    {-[>FNum 1   -> liftM DIDeviceType word8P<]-} -- dynamic
    FNum 2   -> liftM (maybe NoDeviceInfo DIManufacturer) $ enum16P arch
    FNum 3   -> liftM (maybe NoDeviceInfo DISerialNumber) $ uInt32zP arch
    FNum 4   -> liftM (maybe NoDeviceInfo DIProduct) $ uInt16P arch -- dynamic
    FNum 5   -> liftM (maybe NoDeviceInfo $ DISoftwareVersion . scaleBy 100) $ uInt16P arch
    FNum 6   -> liftM (maybe NoDeviceInfo DIHardwareVersion) uInt8P
    FNum 7   -> liftM (maybe NoDeviceInfo DICumOperatingTime) $ uInt32P arch
    FNum 10  -> liftM (maybe NoDeviceInfo $ DIBatteryVoltage . scaleBy 256) $ uInt16P arch
    FNum 11  -> liftM (maybe NoDeviceInfo DIBatteryStatus) enumP
    FNum 18  -> liftM (maybe NoDeviceInfo DISensorPosition) enumP
    FNum 19  -> liftM (maybe NoDeviceInfo DIDescriptor) $ stringP sz
    FNum 20  -> liftM (maybe NoDeviceInfo DIAntTransmissionType) uInt8zP
    FNum 21  -> liftM (maybe NoDeviceInfo DIAntDeviceNumber) $ uInt16zP arch
    FNum 22  -> liftM (maybe NoDeviceInfo DIAntNetwork) enumP
    FNum 25  -> liftM (maybe NoDeviceInfo DISourceType) enumP
    FNum 27  -> liftM (maybe NoDeviceInfo DIProductName) $ stringP sz
    {-| otherwise   = baseTypeValueP arch fDef >> return NoDeviceInfo-}
    FNum fNum' -> liftM (NoDeviceInfo' fNum') $ baseTypeValueP arch fDef

fileCreatorP :: Arch -> FieldDefinition -> Parser FileCreator
fileCreatorP arch (FieldDef fNum _ _) = case fNum of
    FNum 0 -> liftM SoftwareVersion $ word16P arch
    FNum 1 -> liftM HardwareVersion word8P
    _      -> throwError $ InvalidFieldNum' fNum

lapP :: Arch -> FieldDefinition -> Parser Lap
lapP arch fDef@(FieldDef fNum _ _) = case fNum of
    FNum 254 -> liftM (maybe NoLap $ LapMessageIndex . T.mkMsgIdx) $ uInt16P arch
    FNum 253 -> liftM (maybe NoLap $ LapTimestamp . DateTime) $ uInt32P arch
    FNum 0   -> liftM (maybe NoLap LapEvent) enumP
    FNum 1   -> liftM (maybe NoLap LapEventType) enumP
    FNum 2   -> liftM (maybe NoLap $ LapStartTime . DateTime) $ uInt32P arch
    FNum 3   -> liftM (maybe NoLap LapStartPositionLat) $ sInt32P arch
    FNum 4   -> liftM (maybe NoLap LapStartPositionLong) $ sInt32P arch
    FNum 5   -> liftM (maybe NoLap LapEndPositionLat) $ sInt32P arch
    FNum 6   -> liftM (maybe NoLap LapEndPositionLong) $ sInt32P arch
    FNum 7   -> liftM (maybe NoLap $ LapTotalElapsedTime . scaleBy 1000) $ uInt32P arch
    FNum 8   -> liftM (maybe NoLap $ LapTotalTimerTime   . scaleBy 1000) $ uInt32P arch
    FNum 9   -> liftM (maybe NoLap $ LapTotalDistance    . scaleBy 100)  $ uInt32P arch
    {-FNum 10  -> liftM LapTotalCycles                   -}  -- dynamic
    FNum 11  -> liftM (maybe NoLap LapTotalCalories) $ uInt16P arch
    FNum 12  -> liftM (maybe NoLap LapTotalFatCalories) $ uInt16P arch
    FNum 13  -> liftM (maybe NoLap $ LapAvgSpeed . scaleBy 1000) $ uInt16P arch
    FNum 14  -> liftM (maybe NoLap $ LapMaxSpeed . scaleBy 1000) $ uInt16P arch
    FNum 15  -> liftM (maybe NoLap LapAvgHeartRate) uInt8P
    FNum 16  -> liftM (maybe NoLap LapMaxHeartRate) uInt8P
    {-FNum 17  -> liftM LapAvgCadence                     dynamic-}
    {-FNum 18  -> liftM LapMaxCadence                     dynamic-}
    FNum 19  -> liftM (maybe NoLap LapAvgPower) $ uInt16P arch
    FNum 20  -> liftM (maybe NoLap LapMaxPower) $ uInt16P arch
    FNum 21  -> liftM (maybe NoLap LapTotalAscent) $ uInt16P arch
    FNum 22  -> liftM (maybe NoLap LapTotalDescent) $ uInt16P arch
    FNum 23  -> liftM (maybe NoLap LapIntensity) enumP
    FNum 24  -> liftM (maybe NoLap LapLapTrigger) enumP
    FNum 25  -> liftM (maybe NoLap LapSport) enumP
    FNum 26  -> liftM (maybe NoLap LapEventGroup) uInt8P
    FNum 32  -> liftM (maybe NoLap LapNumLengths) $ uInt16P arch
    FNum 33  -> liftM (maybe NoLap LapNormalizedPower) $ uInt16P arch
    FNum 34  -> liftM (maybe NoLap $ LapLeftRightBalance . T.mkLRB100) $ uInt16P arch
    FNum 35  -> liftM (maybe NoLap LapFirstLengthIndex) $ uInt16P arch
    FNum 37  -> liftM (maybe NoLap $ LapAvgStrokeDistance . scaleBy 100) $ uInt16P arch
    FNum 38  -> liftM (maybe NoLap LapSwimStroke) enumP
    FNum 39  -> liftM (maybe NoLap LapSubSport) enumP
    FNum 40  -> liftM (maybe NoLap LapNumActiveLengths) $ uInt16P arch
    FNum 41  -> liftM (maybe NoLap LapTotalWork) $ uInt32P arch
    FNum 42  -> liftM (maybe NoLap $ LapAvgAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 43  -> liftM (maybe NoLap $ LapMaxAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 44  -> liftM (maybe NoLap LapGpsAccuracy) uInt8P
    FNum 45  -> liftM (maybe NoLap $ LapAvgGrade . scaleBy 100) $ sInt16P arch
    FNum 46  -> liftM (maybe NoLap $ LapAvgPosGrade . scaleBy 100) $ sInt16P arch
    FNum 47  -> liftM (maybe NoLap $ LapAvgNegGrade . scaleBy 100) $ sInt16P arch
    FNum 48  -> liftM (maybe NoLap $ LapMaxPosGrade . scaleBy 100) $ sInt16P arch
    FNum 49  -> liftM (maybe NoLap $ LapMaxNegGrade . scaleBy 100) $ sInt16P arch
    FNum 50  -> liftM (maybe NoLap LapAvgTemperature) sInt8P
    FNum 51  -> liftM (maybe NoLap LapMaxTemperature) sInt8P
    FNum 52  -> liftM (maybe NoLap $ LapTotalMovingTime . scaleBy 1000) $ uInt16P arch
    FNum 53  -> liftM (maybe NoLap $ LapAvgPosVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 54  -> liftM (maybe NoLap $ LapAvgNegVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 55  -> liftM (maybe NoLap $ LapMaxPosVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 56  -> liftM (maybe NoLap $ LapMaxNegVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 57  -> liftM (maybe NoLap $ LapTimeInHRZone      . scaleBy 1000) $ uInt32P arch
    FNum 58  -> liftM (maybe NoLap $ LapTimeInSpeedZone   . scaleBy 1000) $ uInt32P arch
    FNum 59  -> liftM (maybe NoLap $ LapTimeInCadenceZone . scaleBy 1000) $ uInt32P arch
    FNum 60  -> liftM (maybe NoLap $ LapTimeInPowerZone   . scaleBy 1000) $ uInt32P arch
    FNum 61  -> liftM (maybe NoLap LapRepetitionNum) $ uInt16P arch
    FNum 62  -> liftM (maybe NoLap $ LapMinAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 63  -> liftM (maybe NoLap LapMinHeartRate) uInt8P
    FNum 71  -> liftM (maybe NoLap $ LapWktStepIndex . T.mkMsgIdx) $ uInt16P arch
    FNum 74  -> liftM (maybe NoLap LapOpponentScore) $ uInt16P arch
    FNum 75  -> liftM (maybe NoLap LapStrokeCount) $ uInt16P arch
    FNum 76  -> liftM (maybe NoLap LapZoneCount) $ uInt16P arch
    FNum 77  -> liftM (maybe NoLap $ LapAvgVerticalOscillation . scaleBy 10) $ uInt16P arch
    FNum 78  -> liftM (maybe NoLap $ LapAvgStanceTimePercent . scaleBy 100) $ uInt16P arch
    FNum 79  -> liftM (maybe NoLap $ LapAvgStanceTime . scaleBy 10) $ uInt16P arch
    FNum 80  -> liftM (maybe NoLap $ LapAvgFractionalCadence . scaleBy 128) uInt8P
    FNum 81  -> liftM (maybe NoLap $ LapMaxFractionalCadence . scaleBy 128) uInt8P
    FNum 82  -> liftM (maybe NoLap $ LapTotalFractionalCycles . scaleBy 128) uInt8P
    FNum 83  -> liftM (maybe NoLap LapPlayerScore) $ uInt16P arch
    FNum 84  -> liftM (maybe NoLap $ LapAvgTotalHemoglobinConc . scaleBy 100) $ uInt16P arch
    FNum 85  -> liftM (maybe NoLap $ LapMinTotalHemoglobinConc . scaleBy 100) $ uInt16P arch
    FNum 86  -> liftM (maybe NoLap $ LapMaxTotalHemoglobinConc . scaleBy 100) $ uInt16P arch
    FNum 87  -> liftM (maybe NoLap $ LapAvgSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    FNum 88  -> liftM (maybe NoLap $ LapMinSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    FNum 89  -> liftM (maybe NoLap $ LapMaxSaturatedHemoglobinPercent . scaleBy 10) $ uInt16P arch
    FNum 91  -> liftM (maybe NoLap $ LapAvgLeftTorqueEffectiveness  . scaleBy 2) uInt8P
    FNum 92  -> liftM (maybe NoLap $ LapAvgRightTorqueEffectiveness . scaleBy 2) uInt8P
    FNum 93  -> liftM (maybe NoLap $ LapAvgLeftPedalSmoothness      . scaleBy 2) uInt8P
    FNum 94  -> liftM (maybe NoLap $ LapAvgRightPedalSmoothness     . scaleBy 2) uInt8P
    FNum 95  -> liftM (maybe NoLap $ LapAvgCombinedPedalSmoothness  . scaleBy 2) uInt8P
    FNum 98  -> liftM (maybe NoLap $ LapTimeStanding . scaleBy 1000) $ uInt32P arch
    FNum 99  -> liftM (maybe NoLap LapStandCount) $ uInt16P arch
    FNum 100 -> liftM (maybe NoLap LapAvgLeftPco)  sInt8P
    FNum 101 -> liftM (maybe NoLap LapAvgRightPco) sInt8P
    FNum 102 -> liftM (maybe NoLap $ LapAvgLeftPowerPhase      . scaleBy 0.7111111) uInt8P
    FNum 103 -> liftM (maybe NoLap $ LapAvgLeftPowerPhasePeak  . scaleBy 0.7111111) uInt8P
    FNum 104 -> liftM (maybe NoLap $ LapAvgRightPowerPhase     . scaleBy 0.7111111) uInt8P
    FNum 105 -> liftM (maybe NoLap $ LapAvgRightPowerPhasePeak . scaleBy 0.7111111) uInt8P
    FNum 106 -> liftM (maybe NoLap LapAvgPowerPosition) $ uInt16P arch
    FNum 107 -> liftM (maybe NoLap LapMaxPowerPosition) $ uInt16P arch
    FNum 108 -> liftM (maybe NoLap LapAvgCadencePosition) uInt8P
    FNum 109 -> liftM (maybe NoLap LapMaxCadencePosition) uInt8P
    FNum 110 -> liftM (maybe NoLap $ LapEnhancedAvgSpeed . scaleBy 1000) $ uInt32P arch
    FNum 111 -> liftM (maybe NoLap $ LapEnhancedMaxSpeed . scaleBy 1000) $ uInt32P arch
    FNum 112 -> liftM (maybe NoLap $ LapEnhancedAvgAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 113 -> liftM (maybe NoLap $ LapEnhancedMinAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 114 -> liftM (maybe NoLap $ LapEnhancedMaxAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 115 -> liftM (maybe NoLap LapAvgLevMotorPower) $ uInt16P arch
    FNum 116 -> liftM (maybe NoLap LapMaxLevMotorPower) $ uInt16P arch
    FNum 117 -> liftM (maybe NoLap $ LapLevBatteryConsumption . scaleBy 2) uInt8P
    {-| otherwise   = baseTypeValueP arch fDef >> return NoLap-}
    FNum fNum' -> liftM (NoLap' fNum') $ baseTypeValueP arch fDef

activityP :: Arch -> FieldDefinition -> Parser Activity
activityP arch (FieldDef fNum _ _) = case fNum of
    FNum 253 -> liftM (maybe NoActivity $ ActivityTimestamp . DateTime) $ uInt32P arch
    FNum 0   -> liftM (maybe NoActivity ActivityTotalTimerTime) $ uInt32P arch 
    FNum 1   -> liftM (maybe NoActivity ActivityNumSessions) $ uInt16P arch
    FNum 2   -> liftM (maybe NoActivity ActivityType) enumP
    FNum 3   -> liftM (maybe NoActivity ActivityEvent) enumP
    FNum 4   -> liftM (maybe NoActivity ActivityEventType) enumP
    FNum 5   -> liftM (maybe NoActivity $ ActivityLocalTimestamp . LocalDateTime) $ uInt32P arch
    FNum 6   -> liftM (maybe NoActivity ActivityEventGroup) uInt8P
    _        -> throwError $ InvalidFieldNum' fNum

sessionP :: Arch -> FieldDefinition -> Parser Session
sessionP arch fDef@(FieldDef fNum sz _) = case fNum of
    FNum 254 -> liftM (maybe NoSession $ SessionMessageIndex . T.mkMsgIdx) $ uInt16P arch
    FNum 253 -> liftM (maybe NoSession $ SessionTimestamp . DateTime) $ uInt32P arch
    FNum 0   -> liftM (maybe NoSession SessionEvent) enumP
    FNum 1   -> liftM (maybe NoSession SessionEventType) enumP
    FNum 2   -> liftM (maybe NoSession $ SessionStartTime . DateTime) $ uInt32P arch
    FNum 3   -> liftM (maybe NoSession SessionStartPositionLat) $ sInt32P arch
    FNum 4   -> liftM (maybe NoSession SessionStartPositionLong) $ sInt32P arch
    FNum 5   -> liftM (maybe NoSession SessionSport) enumP
    FNum 6   -> liftM (maybe NoSession SessionSubSport) enumP
    FNum 7   -> liftM (maybe NoSession $ SessionTotalElapsedTime . scaleBy 1000) $ uInt32P arch
    FNum 8   -> liftM (maybe NoSession $ SessionTotalTimerTime   . scaleBy 1000) $ uInt32P arch
    FNum 9   -> liftM (maybe NoSession $ SessionTotalDistance    . scaleBy 100)  $ uInt32P arch
    {-FNum 10  -> liftM SessionTotalCycles                   -}  -- dynamic
    FNum 11  -> liftM (maybe NoSession SessionTotalCalories) $ uInt16P arch
    FNum 13  -> liftM (maybe NoSession SessionTotalFatCalories) $ uInt16P arch
    FNum 14  -> liftM (maybe NoSession $ SessionAvgSpeed . scaleBy 1000) $ uInt16P arch
    FNum 15  -> liftM (maybe NoSession $ SessionMaxSpeed . scaleBy 1000) $ uInt16P arch
    FNum 16  -> liftM (maybe NoSession SessionAvgHeartRate) uInt8P
    FNum 17  -> liftM (maybe NoSession SessionMaxHeartRate) uInt8P
    {-FNum 18  -> liftM SessionAvgCadence                     dynamic-}
    {-FNum 19  -> liftM SessionMaxCadence                     dynamic-}
    FNum 20  -> liftM (maybe NoSession SessionAvgPower) $ uInt16P arch
    FNum 21  -> liftM (maybe NoSession SessionMaxPower) $ uInt16P arch
    FNum 22  -> liftM (maybe NoSession SessionTotalAscent) $ uInt16P arch
    FNum 23  -> liftM (maybe NoSession SessionTotalDescent) $ uInt16P arch
    FNum 24  -> liftM (maybe NoSession $ SessionTotalTrainingEffect . scaleBy 10) uInt8P
    FNum 25  -> liftM (maybe NoSession SessionFirstLapIndex) $ uInt16P arch
    FNum 26  -> liftM (maybe NoSession SessionNumLaps) $ uInt16P arch
    FNum 27  -> liftM (maybe NoSession SessionEventGroup) uInt8P
    FNum 28  -> liftM (maybe NoSession SessionTrigger) enumP
    FNum 29  -> liftM (maybe NoSession SessionNecLat) $ sInt32P arch
    FNum 30  -> liftM (maybe NoSession SessionNecLong) $ sInt32P arch
    FNum 31  -> liftM (maybe NoSession SessionSwcLat) $ sInt32P arch
    FNum 32  -> liftM (maybe NoSession SessionSwcLong) $ sInt32P arch
    FNum 34  -> liftM (maybe NoSession SessionNormalizedPower) $ uInt16P arch
    FNum 35  -> liftM (maybe NoSession $ SessionTrainingStressScore . scaleBy 10) $ uInt16P arch
    FNum 36  -> liftM (maybe NoSession $ SessionIntensityFactor . scaleBy 1000) $ uInt16P arch
    FNum 37  -> liftM (maybe NoSession $ SessionLeftRightBalance . T.mkLRB100) $ uInt16P arch
    FNum 41  -> liftM (maybe NoSession $ SessionAvgStrokeCount . scaleBy 10) $ uInt32P arch
    FNum 42  -> liftM (maybe NoSession $ SessionAvgStrokeDistance . scaleBy 100) $ uInt16P arch
    FNum 43  -> liftM (maybe NoSession SessionSwimStroke) enumP
    FNum 44  -> liftM (maybe NoSession $ SessionPoolLength . scaleBy 100) $ uInt16P arch
    FNum 45  -> liftM (maybe NoSession SessionThresholdPower) $ uInt16P arch
    FNum 46  -> liftM (maybe NoSession SessionPoolLengthUnit) enumP
    FNum 47  -> liftM (maybe NoSession SessionNumActiveLengths) $ uInt16P arch
    FNum 48  -> liftM (maybe NoSession SessionTotalWork) $ uInt32P arch
    FNum 49  -> liftM (maybe NoSession $ SessionAvgAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 50  -> liftM (maybe NoSession $ SessionMaxAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 51  -> liftM (maybe NoSession SessionGPSAccuracy) uInt8P
    FNum 52  -> liftM (maybe NoSession $ SessionAvgGrade . scaleBy 100) $ sInt16P arch
    FNum 53  -> liftM (maybe NoSession $ SessionAvgPosGrade . scaleBy 100) $ sInt16P arch
    FNum 54  -> liftM (maybe NoSession $ SessionAvgNegGrade . scaleBy 100) $ sInt16P arch
    FNum 55  -> liftM (maybe NoSession $ SessionMaxPosGrade . scaleBy 100) $ sInt16P arch
    FNum 56  -> liftM (maybe NoSession $ SessionMaxNegGrade . scaleBy 100) $ sInt16P arch
    FNum 57  -> liftM (maybe NoSession SessionAvgTemperature) sInt8P
    FNum 58  -> liftM (maybe NoSession SessionMaxTemperature) sInt8P
    FNum 59  -> liftM (maybe NoSession $ SessionTotalMovingTime . scaleBy 1000) $ uInt32P arch
    FNum 60  -> liftM (maybe NoSession $ SessionAvgPosVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 61  -> liftM (maybe NoSession $ SessionAvgNegVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 62  -> liftM (maybe NoSession $ SessionMaxPosVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 63  -> liftM (maybe NoSession $ SessionMaxNegVerticalSpeed . scaleBy 1000) $ sInt16P arch
    FNum 64  -> liftM (maybe NoSession SessionMinHeartRate) uInt8P
    FNum 65  -> liftM (maybe NoSession $ SessionTimeInHRZone      . scaleBy 1000) $ uInt32P arch
    FNum 66  -> liftM (maybe NoSession $ SessionTimeInSpeedZone   . scaleBy 1000) $ uInt32P arch
    FNum 67  -> liftM (maybe NoSession $ SessionTimeInCadenceZone . scaleBy 1000) $ uInt32P arch
    FNum 68  -> liftM (maybe NoSession $ SessionTimeInPowerZone   . scaleBy 1000) $ uInt32P arch
    FNum 69  -> liftM (maybe NoSession $ SessionAvgLapTime . scaleBy 1000) $ uInt32P arch
    FNum 70  -> liftM (maybe NoSession SessionBestLapIndex) $ uInt16P arch
    FNum 71  -> liftM (maybe NoSession $ SessionMinAltitude . translateBy 500 . scaleBy 5) $ uInt16P arch
    FNum 82  -> liftM (maybe NoSession SessionPlayerScore) $ uInt16P arch
    FNum 83  -> liftM (maybe NoSession SessionOpponentScore) $ uInt16P arch
    FNum 84  -> liftM (maybe NoSession SessionOpponentName) $ stringP sz
    FNum 85  -> liftM (maybe NoSession SessionStrokeCount) $ uInt16P arch
    FNum 86  -> liftM (maybe NoSession SessionZoneCount) $ uInt16P arch
    FNum 87  -> liftM (maybe NoSession $ SessionMaxBallSpeed . scaleBy 100) $ uInt16P arch
    FNum 88  -> liftM (maybe NoSession $ SessionAvgBallSpeed . scaleBy 100) $ uInt16P arch
    FNum 89  -> liftM (maybe NoSession $ SessionAvgVerticalOscillation . scaleBy 10) $ uInt16P arch
    FNum 90  -> liftM (maybe NoSession $ SessionAvgStanceTimePercent . scaleBy 100) $ uInt16P arch
    FNum 91  -> liftM (maybe NoSession $ SessionAvgStanceTime  . scaleBy 10) $ uInt16P arch
    FNum 92  -> liftM (maybe NoSession $ SessionAvgFractionalCadence . scaleBy 128) uInt8P
    FNum 93  -> liftM (maybe NoSession $ SessionMaxFractionalCadence . scaleBy 128) uInt8P
    FNum 94  -> liftM (maybe NoSession $ SessionTotalFractionalCycles . scaleBy 128) uInt8P
    FNum 95  -> liftM (maybe NoSession $ SessionAvgTotalHemoglobinConc  . scaleBy 100) $ uInt16P arch
    FNum 96  -> liftM (maybe NoSession $ SessionMinTotalHemoglobinConc  . scaleBy 100) $ uInt16P arch
    FNum 97  -> liftM (maybe NoSession $ SessionMaxTotalHemoglobinConc  . scaleBy 100) $ uInt16P arch
    FNum 98  -> liftM (maybe NoSession $ SessionAvgSaturatedHemoglobinPercent  . scaleBy 10) $ uInt16P arch
    FNum 99  -> liftM (maybe NoSession $ SessionMinSaturatedHemoglobinPercent  . scaleBy 10) $ uInt16P arch
    FNum 100 -> liftM (maybe NoSession $ SessionMaxSaturatedHemoglobinPercent  . scaleBy 10) $ uInt16P arch
    FNum 101 -> liftM (maybe NoSession $ SessionAvgLeftTorqueEffectiveness  . scaleBy 2) uInt8P
    FNum 102 -> liftM (maybe NoSession $ SessionAvgRightTorqueEffectiveness . scaleBy 2) uInt8P
    FNum 103 -> liftM (maybe NoSession $ SessionAvgLeftPedalSmoothness . scaleBy 2) uInt8P
    FNum 104 -> liftM (maybe NoSession $ SessionAvgRightPedalSmoothness . scaleBy 2) uInt8P
    FNum 105 -> liftM (maybe NoSession $ SessionAvgCombinedPedalSmoothness . scaleBy 2) uInt8P
    FNum 111 -> liftM (maybe NoSession SessionSportIndex) uInt8P
    FNum 112 -> liftM (maybe NoSession $ SessionTimeStanding . scaleBy 1000) $ uInt32P arch
    FNum 113 -> liftM (maybe NoSession SessionStandCount) $ uInt16P arch
    FNum 114 -> liftM (maybe NoSession SessionAvgLeftPco) sInt8P 
    FNum 115 -> liftM (maybe NoSession SessionAvgRightPco) sInt8P
    FNum 116 -> liftM (maybe NoSession $ SessionAvgLeftPowerPhase      . scaleBy 0.7111111) uInt8P
    FNum 117 -> liftM (maybe NoSession $ SessionAvgLeftPowerPhasePeak  . scaleBy 0.7111111) uInt8P
    FNum 118 -> liftM (maybe NoSession $ SessionAvgRightPowerPhase     . scaleBy 0.7111111) uInt8P
    FNum 119 -> liftM (maybe NoSession $ SessionAvgRightPowerPhasePeak . scaleBy 0.7111111) uInt8P
    FNum 120 -> liftM (maybe NoSession SessionAvgPowerPosition) $ uInt16P arch
    FNum 121 -> liftM (maybe NoSession SessionMaxPowerPosition) $ uInt16P arch
    FNum 122 -> liftM (maybe NoSession SessionAvgCadencePosition) uInt8P
    FNum 123 -> liftM (maybe NoSession SessionMaxCadencePosition) uInt8P
    FNum 124 -> liftM (maybe NoSession $ SessionEnhancedAvgSpeed . scaleBy 1000) $ uInt32P arch
    FNum 125 -> liftM (maybe NoSession $ SessionEnhancedMaxSpeed . scaleBy 1000) $ uInt32P arch
    FNum 126 -> liftM (maybe NoSession $ SessionEnhancedAvgAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 127 -> liftM (maybe NoSession $ SessionEnhancedMinAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 128 -> liftM (maybe NoSession $ SessionEnhancedMaxAltitude . translateBy 500 . scaleBy 5) $ uInt32P arch
    FNum 129 -> liftM (maybe NoSession SessionAvgLevMotorPower) $ uInt16P arch
    FNum 130 -> liftM (maybe NoSession SessionMaxLevMotorPower) $ uInt16P arch
    FNum 131 -> liftM (maybe NoSession $ SessionLevBatteryConsumption . scaleBy 2) uInt8P
    FNum fNum' -> liftM (NoSession' fNum') $ baseTypeValueP arch fDef

baseTypeValueP :: Arch -> FieldDefinition -> Parser BaseTypeValue
baseTypeValueP arch fDef = case fDef of
    FieldDef _ _    0  -> liftM Enum    word8P
    FieldDef _ _    1  -> liftM SInt8   num8P
    FieldDef _ _    2  -> liftM UInt8   word8P
    FieldDef _ _    3  -> liftM SInt16  $ num16P arch
    FieldDef _ _    4  -> liftM UInt16  $ word16P arch
    FieldDef _ _    5  -> liftM SInt32  $ num32P arch
    FieldDef fNum _ 6  -> do w <- word32P arch
                             _ <- when (fNum == FNum 253) $ modify $ setTimestamp $ Timestamp w
                             return $ UInt32 w
    FieldDef _ size 7  -> liftM String  $ replicateM size word8P
    FieldDef _ _    8  -> liftM Float32 $ num32P arch
    FieldDef _ _    9  -> liftM Float64 $ num64P arch
    FieldDef _ _    10 -> liftM UInt8z  word8P
    FieldDef _ _    11 -> liftM UInt16z $ word16P arch
    FieldDef _ _    12 -> liftM UInt32z $ word32P arch
    FieldDef _ size 13 -> liftM Byte    $ replicateM size word8P
    FieldDef _ _    bt -> throwError $ InvalidBasetype bt

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
byteToCSD bs = case bs of
    (b1:b2:b3:_) -> bytesToCSD b1 b2 b3
    _            -> (-1, -1)

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
    if w == 0xFFFF
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
    if w == 0x0000
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
