module Profiles where

import Data.Word
import Data.Int

import BaseType
import Timestamp

import qualified Types as T

data FileID = 
    Type          !T.File
  | Manufacturer  !T.Manufacturer
  | Product       !Word16 -- ProductSubfield 
  | Serial        !Word32
  | TimeCreated   !DateTime
  | Number        !Word16
  | ProductName   !String
  | NoFileID     
  | NoFileID'     !Word8 !BaseTypeValue
  deriving (Show)

data ProductSubfield =
    GarminProduct !T.GarminProduct
  | Product'      !Word16
  deriving (Show)

data FileCreator = 
    SoftwareVersion    !Word16
  | HardwareVersion    !Word8
  deriving (Show)

data TimestampCorrelation = 
    TCTimestamp                 !DateTime
  | TCFractionalTimestamp       !Word16
  | TCSystemTimestamp           !DateTime
  | TCFractionalSystemTimestamp !Word16
  | TCLocalTimestamp            !LocalDateTime
  | TCTimestampMs               !Word16
  | TCSystemTimestampMs         !Word16
  deriving (Show)

data Software =
    MessageIndex !T.MessageIndex
  | Version      !Word16
  | PartNumber   !String
  deriving (Show)

data SlaveDevice =
    SDManufacturer !T.Manufacturer
  | SDProduct      !Word16 -- SDProductSubfield 
  deriving (Show)

data SDProductSubfield =
    SDGarminProduct !T.GarminProduct
  | SDProduct'      !Word16
  deriving (Show)

data Capabilities =
    Languages             !Word8
  | Sports                !T.SportBits0
  | WorkoutsSupported     !T.WorkoutCapabilities
  | ConnectivitySupported !T.ConnectivityCapabilities
  deriving (Show)

data FileCapabilities =
    FCMessageIndex !T.MessageIndex
  | FCType         !T.File
  | FCFlags        !T.FileFlag
  | FCDirectory    !String
  | FCMaxCount     !Word16
  | FCMaxSize      !Word32
  deriving (Show)

data MesgCapabilities =
    MesgCapabilitiesIndex !T.MessageIndex
  | MesgCapabilitiesFile  !T.File
  | MesgCapabilitiesNum   !T.MesgNum
  | CountType             !T.MesgCount
  | Count                 !Word16 -- CountSubfield
  deriving (Show)

data CountSubfield =
    NumPerFile     !Word16
  | MaxPerFile     !Word16
  | MaxPerFileType !Word16
  | Count'         !Word16
  deriving (Show)

data FieldCapabilities =
    FieldCapabilitiesMesgIndex !T.MessageIndex
  | FieldCapabilitiesMesgFile  !T.File
  | FieldCapabilitiesMesgNum   !T.MesgNum
  | FieldCapabilitiesFieldNum  !Word8
  | FieldCapabilitiesCount     !Word16
  deriving (Show)

data Activity = 
    ActivityTimestamp      !DateTime
  | ActivityTotalTimerTime !Word32
  | ActivityNumSessions    !Word16
  | ActivityType           !T.Activity
  | ActivityEvent          !T.Event
  | ActivityEventType      !T.EventType
  | ActivityLocalTimestamp !LocalDateTime
  | ActivityEventGroup     !Word8
  | NoActivity
  deriving (Show)

data Session = 
    SessionMessageIndex                  !T.MessageIndex
  | SessionTimestamp                     !DateTime
  | SessionEvent                         !T.Event
  | SessionEventType                     !T.EventType
  | SessionStartTime                     !DateTime
  | SessionStartPositionLat              !Int32
  | SessionStartPositionLong             !Int32
  | SessionSport                         !T.Sport
  | SessionSubSport                      !T.SubSport
  | SessionTotalElapsedTime              !Float
  | SessionTotalTimerTime                !Float
  | SessionTotalDistance                 !Float
  | SessionTotalCycles                   !Word32 -- STotalCyclesSubfield
  | SessionTotalCalories                 !Word16
  | SessionTotalFatCalories              !Word16
  | SessionAvgSpeed                      !Float
  | SessionMaxSpeed                      !Float
  | SessionAvgHeartRate                  !Word8
  | SessionMaxHeartRate                  !Word8
  | SessionAvgCadence                    !Word8 -- SAvgCadenceSubfield
  | SessionMaxCadence                    !Word8 -- SMaxCadenceSubfield
  | SessionAvgPower                      !Word16
  | SessionMaxPower                      !Word16
  | SessionTotalAscent                   !Word16
  | SessionTotalDescent                  !Word16
  | SessionTotalTrainingEffect           !Float
  | SessionFirstLapIndex                 !Word16
  | SessionNumLaps                       !Word16
  | SessionEventGroup                    !Word8
  | SessionTrigger                       !T.SessionTrigger
  | SessionNecLat                        !Int32
  | SessionNecLong                       !Int32
  | SessionSwcLat                        !Int32
  | SessionSwcLong                       !Int32
  | SessionNormalizedPower               !Word16
  | SessionTrainingStressScore           !Float
  | SessionIntensityFactor               !Float
  | SessionLeftRightBalance              !T.LeftRightBalance100
  | SessionAvgStrokeCount                !Float
  | SessionAvgStrokeDistance             !Float
  | SessionSwimStroke                    !T.SwimStroke
  | SessionPoolLength                    !Float
  | SessionThresholdPower                !Word16
  | SessionPoolLengthUnit                !T.DisplayMeasure
  | SessionNumActiveLengths              !Word16
  | SessionTotalWork                     !Word32
  | SessionAvgAltitude                   !Float
  | SessionMaxAltitude                   !Float
  | SessionGPSAccuracy                   !Word8
  | SessionAvgGrade                      !Float
  | SessionAvgPosGrade                   !Float
  | SessionAvgNegGrade                   !Float
  | SessionMaxPosGrade                   !Float
  | SessionMaxNegGrade                   !Float
  | SessionAvgTemperature                !Int8
  | SessionMaxTemperature                !Int8
  | SessionTotalMovingTime               !Float
  | SessionAvgPosVerticalSpeed           !Float
  | SessionAvgNegVerticalSpeed           !Float
  | SessionMaxPosVerticalSpeed           !Float
  | SessionMaxNegVerticalSpeed           !Float
  | SessionMinHeartRate                  !Word8
  | SessionTimeInHRZone                  !Float
  | SessionTimeInSpeedZone               !Float
  | SessionTimeInCadenceZone             !Float
  | SessionTimeInPowerZone               !Float
  | SessionAvgLapTime                    !Float
  | SessionBestLapIndex                  !Word16
  | SessionMinAltitude                   !Float
  | SessionPlayerScore                   !Word16
  | SessionOpponentScore                 !Word16
  | SessionOpponentName                  !String
  | SessionStrokeCount                   !Word16
  | SessionZoneCount                     !Word16
  | SessionMaxBallSpeed                  !Float
  | SessionAvgBallSpeed                  !Float
  | SessionAvgVerticalOscillation        !Float
  | SessionAvgStanceTimePercent          !Float
  | SessionAvgStanceTime                 !Float
  | SessionAvgFractionalCadence          !Float
  | SessionMaxFractionalCadence          !Float
  | SessionTotalFractionalCycles         !Float
  | SessionAvgTotalHemoglobinConc        !Float
  | SessionMinTotalHemoglobinConc        !Float
  | SessionMaxTotalHemoglobinConc        !Float
  | SessionAvgSaturatedHemoglobinPercent !Float
  | SessionMinSaturatedHemoglobinPercent !Float
  | SessionMaxSaturatedHemoglobinPercent !Float
  | SessionAvgLeftTorqueEffectiveness    !Float
  | SessionAvgRightTorqueEffectiveness   !Float
  | SessionAvgLeftPedalSmoothness        !Float
  | SessionAvgRightPedalSmoothness       !Float
  | SessionAvgCombinedPedalSmoothness    !Float
  | SessionSportIndex                    !Word8
  | SessionTimeStanding                  !Float
  | SessionStandCount                    !Word16
  | SessionAvgLeftPco                    !Int8
  | SessionAvgRightPco                   !Int8
  | SessionAvgLeftPowerPhase             !Float
  | SessionAvgLeftPowerPhasePeak         !Float
  | SessionAvgRightPowerPhase            !Float
  | SessionAvgRightPowerPhasePeak        !Float
  | SessionAvgPowerPosition              !Word16
  | SessionMaxPowerPosition              !Word16
  | SessionAvgCadencePosition            !Word8
  | SessionMaxCadencePosition            !Word8
  | SessionEnhancedAvgSpeed              !Float 
  | SessionEnhancedMaxSpeed              !Float
  | SessionEnhancedAvgAltitude           !Float
  | SessionEnhancedMinAltitude           !Float
  | SessionEnhancedMaxAltitude           !Float
  | SessionAvgLevMotorPower              !Word16
  | SessionMaxLevMotorPower              !Word16
  | SessionLevBatteryConsumption         !Float
  | NoSession
  | NoSession'                           !Word8 !BaseTypeValue
  deriving (Show)

data STotalCyclesSubfield =
    TotalStrides  !Word32
  | STotalCycles' !Word32
  deriving (Show)

data SAvgCadenceSubfield =
    SAvgRunningCadence !Word8
  | SAvgCadence'       !Word8
  deriving (Show)

data SMaxCadenceSubfield =
    SMaxRunningCadence !Word8
  | SMaxCadence'       !Word8
  deriving (Show)

data Lap = 
    LapMessageIndex                  !T.MessageIndex
  | LapTimestamp                     !DateTime
  | LapEvent                         !T.Event
  | LapEventType                     !T.EventType
  | LapStartTime                     !DateTime
  | LapStartPositionLat              !Int32
  | LapStartPositionLong             !Int32
  | LapEndPositionLat                !Int32
  | LapEndPositionLong               !Int32
  | LapTotalElapsedTime              !Float
  | LapTotalTimerTime                !Float
  | LapTotalDistance                 !Float
  | LapTotalCycles                   !Word32 -- LapTotalCyclesSubfield
  | LapTotalCalories                 !Word16
  | LapTotalFatCalories              !Word16
  | LapAvgSpeed                      !Float
  | LapMaxSpeed                      !Float
  | LapAvgHeartRate                  !Word8
  | LapMaxHeartRate                  !Word8
  | LapAvgCadence                    !Word8 -- LapAvgCadenceSubfield
  | LapMaxCadence                    !Word8 -- LapAvgCadenceSubfield
  | LapAvgPower                      !Word16
  | LapMaxPower                      !Word16
  | LapTotalAscent                   !Word16
  | LapTotalDescent                  !Word16
  | LapIntensity                     !T.Intensity
  | LapLapTrigger                    !T.LapTrigger
  | LapSport                         !T.Sport
  | LapEventGroup                    !Word8
  | LapNumLengths                    !Word16
  | LapNormalizedPower               !Word16
  | LapLeftRightBalance              !T.LeftRightBalance100
  | LapFirstLengthIndex              !Word16
  | LapAvgStrokeDistance             !Float
  | LapSwimStroke                    !T.SwimStroke
  | LapSubSport                      !T.SubSport
  | LapNumActiveLengths              !Word16
  | LapTotalWork                     !Word32
  | LapAvgAltitude                   !Float
  | LapMaxAltitude                   !Float
  | LapGpsAccuracy                   !Word8
  | LapAvgGrade                      !Float
  | LapAvgPosGrade                   !Float
  | LapAvgNegGrade                   !Float
  | LapMaxPosGrade                   !Float
  | LapMaxNegGrade                   !Float
  | LapAvgTemperature                !Int8
  | LapMaxTemperature                !Int8
  | LapTotalMovingTime               !Float
  | LapAvgPosVerticalSpeed           !Float
  | LapAvgNegVerticalSpeed           !Float
  | LapMaxPosVerticalSpeed           !Float
  | LapMaxNegVerticalSpeed           !Float
  | LapTimeInHRZone                  !Float
  | LapTimeInSpeedZone               !Float
  | LapTimeInCadenceZone             !Float
  | LapTimeInPowerZone               !Float
  | LapRepetitionNum                 !Word16
  | LapMinAltitude                   !Float
  | LapMinHeartRate                  !Word8
  | LapWktStepIndex                  !T.MessageIndex
  | LapOpponentScore                 !Word16
  | LapStrokeCount                   !Word16
  | LapZoneCount                     !Word16
  | LapAvgVerticalOscillation        !Float
  | LapAvgStanceTimePercent          !Float
  | LapAvgStanceTime                 !Float
  | LapAvgFractionalCadence          !Float
  | LapMaxFractionalCadence          !Float
  | LapTotalFractionalCycles         !Float
  | LapPlayerScore                   !Word16
  | LapAvgTotalHemoglobinConc        !Float
  | LapMinTotalHemoglobinConc        !Float
  | LapMaxTotalHemoglobinConc        !Float
  | LapAvgSaturatedHemoglobinPercent !Float
  | LapMinSaturatedHemoglobinPercent !Float
  | LapMaxSaturatedHemoglobinPercent !Float
  | LapAvgLeftTorqueEffectiveness    !Float
  | LapAvgRightTorqueEffectiveness   !Float
  | LapAvgLeftPedalSmoothness        !Float
  | LapAvgRightPedalSmoothness       !Float
  | LapAvgCombinedPedalSmoothness    !Float
  | LapTimeStanding                  !Float
  | LapStandCount                    !Word16
  | LapAvgLeftPco                    !Int8
  | LapAvgRightPco                   !Int8
  | LapAvgLeftPowerPhase             !Float
  | LapAvgLeftPowerPhasePeak         !Float
  | LapAvgRightPowerPhase            !Float
  | LapAvgRightPowerPhasePeak        !Float
  | LapAvgPowerPosition              !Word16
  | LapMaxPowerPosition              !Word16
  | LapAvgCadencePosition            !Word8
  | LapMaxCadencePosition            !Word8
  | LapEnhancedAvgSpeed              !Float
  | LapEnhancedMaxSpeed              !Float
  | LapEnhancedAvgAltitude           !Float
  | LapEnhancedMinAltitude           !Float
  | LapEnhancedMaxAltitude           !Float
  | LapAvgLevMotorPower              !Word16
  | LapMaxLevMotorPower              !Word16
  | LapLevBatteryConsumption         !Float
  | NoLap    
  | NoLap'                           !Word8 !BaseTypeValue
  deriving (Show)

data LapTotalCyclesSubfield =
    LapTotalStrides !Word32
  | LapTotalCycles' !Word32
  deriving (Show)

data LapAvgCadenceSubfield =
    LapAvgRunningCadence !Word8
  | LapAvgCadence'       !Word8
  deriving (Show)

data LapMaxCadenceSubfield =
    LapMaxRunningCadence !Word8
  | LapMaxCadence'       !Word8
  deriving (Show)

data Length = 
    LengthMessageIndex       !T.MessageIndex
  | LengthTimestamp          !DateTime
  | LengthEvent              !T.Event
  | LengthEventType          !T.EventType
  | LengthStartTime          !DateTime
  | LengthTotalElapsedTime   !Float
  | LengthTotalTimerTime     !Float
  | LengthTotalStrokes       !Word16
  | LengthAvgSpeed           !Float
  | LengthSwimStroke         !T.SwimStroke
  | LengthAvgSwimmingCadence !Word8
  | LengthEventGroup         !Word8
  | LengthTotalCalories      !Word16
  | LengthLengthType         !T.LengthType
  | LengthPlayerScore        !Word16
  | LengthOpponentScore      !Word16
  | LengthStrokeCount        !Word16
  | LengthZoneCount          !Word16
  deriving (Show)

data Record = 
    RecTimestamp                     !DateTime
  | RecPositionLat                   !Int32
  | RecPositionLong                  !Int32
  | RecAltitude                      !Float
  | RecHeartRate                     !Word8
  | RecCadence                       !Word8
  | RecDistance                      !Float
  | RecSpeed                         !Float
  | RecPower                         !Word16
  | RecCompressedSpeedDistance       !Float !Float
  | RecGrade                         !Float
  | RecResistance                    !Word8
  | RecTimeFromCourse                !Float
  | RecCycleLength                   !Float
  | RecTemperature                   !Int8
  | RecSpeed1s                       !Float
  | RecCycles                        !Word8
  | RecTotalCycles                   !Word32
  | RecCompressedAccumulatedPower    !Word16
  | RecAccumulatedPower              !Word32
  | RecLeftRightBalance              !T.LeftRightBalance
  | RecGpsAccuracy                   !Word8
  | RecVerticalSpeed                 !Float
  | RecCalories                      !Word16
  | RecVerticalOscillation           !Float
  | RecStanceTimePercent             !Float
  | RecStanceTime                    !Float
  | RecActivityType                  !T.ActivityType
  | RecLeftTorqueEffectiveness       !Float
  | RecRightTorqueEffectiveness      !Float
  | RecLeftPedalSmoothness           !Float
  | RecRightPedalSmoothness          !Float
  | RecCombinedPedalSmoothness       !Float
  | RecTime128                       !Float
  | RecStrokeType                    !T.StrokeType
  | RecZone                          !Word8
  | RecBallSpeed                     !Float
  | RecCadence256                    !Float
  | RecFractionalCadence             !Float
  | RecTotalHemoglobinConc           !Float
  | RecTotalHemoglobinConcMin        !Float
  | RecTotalHemoglobinConcMax        !Float
  | RecSaturatedHemoglobinPercent    !Float
  | RecSaturatedHemoglobinPercentMin !Float
  | RecSaturatedHemoglobinPercentMax !Float
  | RecDeviceIndex                   !T.DeviceIndex
  | RecLeftPco                       !Int8
  | RecRightPco                      !Int8
  | RecLeftPowerPhase                !Float
  | RecLeftPowerPhasePeak            !Float
  | RecRightPowerPhase               !Float
  | RecRightPowerPhasePeak           !Float
  | RecEnhancedSpeed                 !Float
  | RecEnhancedAltitude              !Float
  | RecBatterySoc                    !Float
  | RecMotorPower                    !Word16
  | NoRecord     
  | NoRecord'                        !Word8 !BaseTypeValue
  deriving (Show)

data Event = 
    EventTimestamp        !DateTime
  | Event                 !T.Event
  | EventType             !T.EventType
  | Data16                !Word16
  | Data                  !Word32 -- DataSubfield
  | EventGroup            !Word8
  | Score                 !Word16 
  | OpponentScore         !Word16
  | FrontGearNum          !Word8
  | FrontGear             !Word8
  | RearGearNum           !Word8
  | RearGear              !Word8
  | DeviceIndex           !T.DeviceIndex
  | NoEvent     
  | NoEvent'              !Word8 BaseTypeValue
  deriving (Show)

data DataSubfield =
    TimerTrigger          !T.TimeTrigger
  | CoursePointIndex      !T.MessageIndex
  | BatteryLevel          !Float
  | VirtualPartnerSpeed   !Float
  | HRHighAlert           !Word8
  | HRLowAlert            !Word8
  | SpeedHighAlert        !Float
  | SpeedLowAlert         !Float
  | CadHighAlert          !Word16
  | CadLowAlert           !Word16
  | PowerHighAlert        !Word16
  | PowerLowAlert         !Word16
  | TimeDurationAlert     !Float
  | DistanceDurationAlert !Float
  | CalorieDurationAlert  !Word32
  | FitnessEquipmentState !T.FitnessEquipmentState
  | SportPoint            !Word16 !Word16
  | GearChangeData        !Word8 !Word8 !Word8 !Word8
  | RiderPosition         !T.RiderPositionType
  | CommTimeout           !T.CommTimeoutType
  | Data'                 !Word32
  deriving (Show)

data DeviceInfo =
    DITimestamp           !DateTime
  | DIDeviceIndex         !T.DeviceIndex
  | DIDeviceType          !Word8 -- DeviceTypeSubfield
  | DIManufacturer        !T.Manufacturer
  | DISerialNumber        !Word32
  | DIProduct             !Word16 -- DIProductSubfield
  | DISoftwareVersion     !Float
  | DIHardwareVersion     !Word8
  | DICumOperatingTime    !Word32
  | DIBatteryVoltage      !Float
  | DIBatteryStatus       !T.BatteryStatus
  | DISensorPosition      !T.BodyLocation
  | DIDescriptor          !String
  | DIAntTransmissionType !Word8
  | DIAntDeviceNumber     !Word16
  | DIAntNetwork          !T.AntNetwork
  | DISourceType          !T.SourceType
  | DIProductName         !String
  | NoDeviceInfo     
  | NoDeviceInfo'         !Word8 !BaseTypeValue
  deriving (Show)

data DeviceTypeSubfield =
    DIAntplusDeviceType !T.AntplusDeviceType
  | DIAntDeviceType     !Word8
  | DIDeviceType'       !Word8
  deriving (Show)

data DIProductSubfield =
    DIGarminProduct !T.GarminProduct
  | DIProduct'      !Word16
  deriving (Show)

data TrainingFile = 
    TFTimestamp    !DateTime
  | TFType         !T.File
  | TFManufacturer !T.Manufacturer
  | TFProduct      !Word16 -- TFProductSubfield
  | TFSerialNumber !Word32
  | TFTimeCreated  !DateTime
  deriving (Show)

data TFProductSubfield =
    TFGarminProduct !T.GarminProduct
  | TFProduct'      !Word16
  deriving (Show)

newtype HRV = HRVTime Float
  deriving (Show)

data CameraEvent = 
    CETimestamp       !DateTime
  | CETimestampMs     !Word16
  | CameraEventType   !T.CameraEventType
  | CameraFileUUID    !String
  | CameraOrientation !T.CameraOrientationType
  deriving (Show)

data GyroscopeData = 
    GDTimestamp        !DateTime
  | GDTimestampMs      !Word16
  | GDSampleTimeOffset !Word16
  | GyroX              !Word16
  | GyroY              !Word16
  | GyroZ              !Word16
  | CalibratedGyroX    !Float
  | CalibratedGyroY    !Float
  | CalibratedGyroZ    !Float
  deriving (Show)

data AccelerometerData = 
    ADTimestamp        !DateTime
  | ADTimestampMs      !Word16
  | ADSampleTimeOffset !Word16
  | AccelX             !Word16
  | AccelY             !Word16
  | AccelZ             !Word16
  | CalibratedAccelX   !Float
  | CalibratedAccelY   !Float
  | CalibratedAccelZ   !Float
  deriving (Show)

data ThreeDSensorCalibration = 
    TDSCTimestamp      !DateTime
  | SensorType         !T.SensorType
  | CalibrationFactor  !Word32 -- CalibrationFactorSubfield 
  | CalibrationDivisor !Word32
  | LevelShift         !Word32
  | OffsetCal          !Int32
  | OrientationMatrix  !Float
  deriving (Show)

data CalibrationFactorSubfield =
    AccelCalFactor     !Word32
  | GyroCalFactor      !Word32
  | CalibrationFactor' !Word32
  deriving (Show)

data VideoFrame =
    VFTimestamp   !DateTime
  | VFTimestampMs !Word16
  | VFFrameNumber !Word32
  deriving (Show)

data ObdiiData = 
    ODTimestamp        !DateTime
  | ODTimestampMs      !Word16
  | ODTimeOffset       !Word16
  | ODPid              ![Word8]
  | ODRawData          ![Word8]
  | ODPidDataSize      !Word8
  | ODSystemTime       !Word32
  | ODStartTimestamp   !DateTime
  | ODStartTimestampMs !Word16
  deriving (Show)

data NmeaSentence =
    NSTimestamp   !DateTime
  | NSTimestampMs !Word16
  | Sentence      !String
  deriving (Show)

data AviationAttitude =
    AATimestamp           !DateTime
  | AATimestampMs         !Word16
  | AASystemTime          !Word32
  | Pitch                 !Float
  | Roll                  !Float
  | AccelLateral          !Float
  | AccelNormal           !Float
  | TurnRate              !Float
  | Stage                 !T.AttitudeStage
  | AttitudeStageComplete !Word8
  | Track                 !Float
  | Validity              !T.AttitudeValidity
  deriving (Show)

data Video =
    Url             !String
  | HostingProvider !String
  | Duration        !Word32
  deriving (Show)

data VideoTitle =
    VideoTitleMessageIndex !T.MessageIndex
  | VideoTitleMessageCount !Word16
  | VideoTitleText         !String
  deriving (Show)

data VideoDescription =
    VideoDescMessageIndex !T.MessageIndex
  | VideoDescMessageCount !Word16
  | VideoDescText         !String
  deriving (Show)

data VideoClip =
    ClipNumber       !Word16
  | StartTimestamp   !DateTime
  | StartTimestampMs !Word16
  | EndTimestamp     !DateTime
  | EndTimestampMs   !Word16
  | ClipStart        !Word32
  | ClipEnd          !Word32
  deriving (Show)

data Course =
    Sport        !T.Sport
  | Name         !String
  | Capabilities !T.CourseCapabilities
  deriving (Show)

data CoursePoint =
    CPMessageIndex !T.MessageIndex
  | CPTimestamp    !DateTime
  | CPPositionLat  !Int32
  | CPPositionLong !Int32
  | CPDistance     !Float
  | CPType         !T.CoursePoint
  | CPName         !String
  | CPFavorite     !Bool
  deriving (Show)

data SegmentID =
    SegIDName             !String
  | SegIDUUID             !String
  | SegIDSport            !T.Sport
  | SegIDEnabled          !Bool
  | UserProfilePrimaryKey !Word32
  | DeviceID              !Word32
  | DefaultRaceLeader     !Word8
  | DeleteStatus          !T.SegmentDeleteStatus
  | SelectionType         !T.SegmentSelectionType
  deriving (Show)

data SegmentLeaderboardEntry =
    SegLeaderboardEntryMessageIndex !T.MessageIndex
  | SegLeaderboardEntryName         !String
  | SegLeaderboardEntryType         !T.SegmentLeaderboardType
  | GroupPrimaryKey                 !Word32
  | ActivityID                      !Word32
  | SegmentTime                     !Float
  | ActivityIDString                !String
  deriving (Show)

data SegmentPoint =
    SegPointMessageIndex !T.MessageIndex
  | SegPointPositionLat  !Int32
  | SegPointPositionLong !Int32
  | SegPointDistance     !Float
  | SegPointAltitude     !Float
  | SegPointLeaderTime   !Float
  deriving (Show)

data SegmentLap =
    SegLapMessageIndex                !T.MessageIndex
  | SegLapTimestamp                   !DateTime
  | SegLapEvent                       !T.Event
  | SegLapEventType                   !T.EventType
  | SegLapStartTime                   !DateTime
  | SegLapStartPositionLat            !Int32
  | SegLapStartPositionLong           !Int32
  | SegLapEndPositionLat              !Int32
  | SegLapEndPositionLong             !Int32
  | SegLapTotalElapsedTime            !Float
  | SegLapTotalTimerTime              !Float
  | SegLapTotalDistance               !Float
  | SegLapTotalCycles                 !Word32 -- SegLapTotalCyclesSubfield
  | SegLapTotalCalories               !Word16
  | SegLapTotalFatCalories            !Word16
  | SegLapAvgSpeed                    !Float
  | SegLapMaxSpeed                    !Float
  | SegLapAvgHeartRate                !Word8
  | SegLapMaxHeartRate                !Word8
  | SegLapAvgCadence                  !Word8
  | SegLapMaxCadence                  !Word8
  | SegLapAvgPower                    !Word16
  | SegLapMaxPower                    !Word16
  | SegLapTotalAscent                 !Word16
  | SegLapTotalDescent                !Word16
  | SegLapSport                       !T.Sport
  | SegLapEventGroup                  !Word8
  | SegLapNecLat                      !Int32
  | SegLapNecLong                     !Int32
  | SegLapSwcLat                      !Int32
  | SegLapSwcLong                     !Int32
  | SegLapName                        !String
  | SegLapNormalizedPower             !Word16
  | SegLapLeftRightBalance            !T.LeftRightBalance100
  | SegLapSubSport                    !T.SubSport
  | SegLapTotalWork                   !Word32
  | SegLapAvgAltitude                 !Float
  | SegLapMaxAltitude                 !Float
  | SegLapGpsAccuracy                 !Word8
  | SegLapAvgGrade                    !Float
  | SegLapAvgPosGrade                 !Float
  | SegLapAvgNegGrade                 !Float
  | SegLapMaxPosGrade                 !Float
  | SegLapMaxNegGrade                 !Float
  | SegLapAvgTemperature              !Int8
  | SegLapMaxTemperature              !Int8
  | SegLapTotalMovingTime             !Float
  | SegLapAvgPosVerticalSpeed         !Float
  | SegLapAvgNegVerticalSpeed         !Float
  | SegLapMaxPosVerticalSpeed         !Float
  | SegLapMaxNegVerticalSpeed         !Float
  | SegLapTimeInHRZone                !Float
  | SegLapTimeInSpeedZone             !Float
  | SegLapTimeInCadenceZone           !Float
  | SegLapTimeInPowerZone             !Float
  | SegLapRepetitionNum               !Word16
  | SegLapMinAltitude                 !Float
  | SegLapMinHeartRate                !Word8
  | SegLapActiveTime                  !Float
  | SegLapWktStepIndex                !T.MessageIndex
  | SegLapSportEvent                  !T.SportEvent
  | SegLapAvgLeftTorqueEffectiveness  !Float
  | SegLapAvgRightTorqueEffectiveness !Float
  | SegLapAvgLeftPedalSmoothness      !Float
  | SegLapAvgRightPedalSmoothness     !Float
  | SegLapAvgCombinedPedalSmoothness  !Float
  | SegLapStatus                      !T.SegmentLapStatus
  | SegLapUUID                        !String
  | SegLapAvgFractionalCadence        !Float
  | SegLapMaxFractionalCadence        !Float
  | SegLapTotalFractionalCycles       !Float
  | SegLapFrontGearShiftCount         !Word16
  | SegLapRearGearShiftCount          !Word16
  | SegLapTimeStanding                !Float
  | SegLapStandCount                  !Word16
  | SegLapAvgLeftPco                  !Int8
  | SegLapAvgRightPco                 !Int8
  | SegLapAvgLeftPowerPhase           !Float
  | SegLapAvgLeftPowerPhasePeak       !Float
  | SegLapAvgRightPowerPhase          !Float
  | SegLapAvgRightPowerPhasePeak      !Float
  | SegLapAvgPowerPosition            !Word16
  | SegLapMaxPowerPosition            !Word16
  | SegLapAvgCadencePosition          !Word8
  | SegLapMaxCadencePosition          !Word8
  deriving (Show)

data SegLapTotalCyclesSubfield =
    SegLapTotalStrokes !Word32
  | SegLapTotalCycles' !Word32
  deriving (Show)

data SegmentFile =
    SegFileMessageIndex           !T.MessageIndex
  | SegFileUUID                   !String
  | SegFileEnabled                !Bool
  | SegFileUserProfilePrimaryKey  !Word32
  | SegFileLeaderType             !T.SegmentLeaderboardType
  | SegFileLeaderGroupPrimaryKey  !Word32
  | SegFileLeaderActivityID       !Word32
  | SegFileLeaderActivityIDString !String
  deriving (Show)

data Workout =
    WktSport         !T.Sport
  | WktCapabilities  !T.WorkoutCapabilities
  | WktNumValidSteps !Word16
  | WktName          !String
  deriving (Show)

data WorkoutStep =
    WktStepMessageIndex          !T.MessageIndex
  | WktStepName                  !String
  | WktStepDurationType          !T.WktStepDuration
  | WktStepDurationValue         !Word32 -- DurationValueSubfield
  | WktStepTargetType            !T.WktStepTarget
  | WktStepTargetValue           !Word32 -- TargetValueSubfield
  | WktStepCustomTargetValueLow  !Word32 -- CustomTargetValueLowSubfield
  | WktStepCustomTargetValueHigh !Word32 -- CustomTargetValueHighSubfield
  | WktStepIntensity             !T.Intensity
  deriving (Show)

data DurationValueSubfield =
    DurationTime     !Float
  | DurationDistance !Float
  | DurationHR       !T.WorkoutHR
  | DurationCalories !Word32
  | DurationStep     !Word32
  | DurationPower    !T.WorkoutPower
  | DurationValue'   !Word32
  deriving (Show)

data TargetValueSubfield =
    TargetHRZone    !Word32
  | TargetPowerZone !Word32
  | RepeatSteps     !Word32
  | RepeatTime      !Float
  | RepeatDistance  !Float
  | RepeatCalories  !Word32
  | RepeatHR        !T.WorkoutHR
  | RepeatPower     !T.WorkoutPower
  | TargetValue'    !Word32
  deriving (Show)

data CustomTargetValueLowSubfield =
    CustomTargetSpeedLow     !Float
  | CustomTargetHeartRateLow !T.WorkoutHR
  | CustomTargetCadenceLow   !Word32
  | CustomTargetPowerLow     !T.WorkoutPower
  | CustomTargetValueLow'    !Word32
  deriving (Show)

data CustomTargetValueHighSubfield =
    CustomTargetSpeedHigh     !Float
  | CustomTargetHeartRateHigh !T.WorkoutHR
  | CustomTargetCadenceHigh   !Word32
  | CustomTargetPowerHigh     !T.WorkoutPower
  | CustomTargetValueHigh'    !Word32
  deriving (Show)

data Schedule =
    ScheduleManufacturer !T.Manufacturer
  | ScheduleProduct      !Word16 -- ScheduleProductSubfield
  | ScheduleSerialNumber !Word32
  | ScheduleTimeCreated  !DateTime
  | ScheduleCompleted    !Bool
  | ScheduleType         !T.Schedule
  | ScheduledTime        !LocalDateTime
  deriving (Show)

data ScheduleProductSubfield =
    ScheduleGarminProduct !T.GarminProduct
  | ScheduleProduct'      !Word16
  deriving (Show)

data Totals =
    TotalsMessageIndex !T.MessageIndex
  | TotalsTimestamp    !DateTime
  | TotalsTimerTime    !Word32
  | TotalsDistance     !Word32
  | TotalsCalories     !Word32
  | TotalsSport        !T.Sport
  | TotalsElapsedTime  !Word32
  | TotalsSessions     !Word16
  | TotalsActiveTime   !Word32
  | TotalsSportIndex   !Word8
  deriving (Show)

data WeightScale =
    WeightScaleTimestamp !DateTime
  | Weight               !T.Weight
  | PercentFat           !Float
  | PercentHydration     !Float
  | VisceralFatMass      !Float
  | BoneMass             !Float
  | MuscleMass           !Float
  | BasalMet             !Float
  | PhysiqueRating       !Word8
  | ActiveMET            !Float
  | MetabolicAge         !Word8
  | VisceralFatRating    !Word8
  | UserProfileIndex     !T.MessageIndex
  deriving (Show)

data BloodPressure =
    BPTimestamp          !DateTime
  | SystolicPressure     !Word16
  | DiastolicPressure    !Word16
  | MeanArterialPressure !Word16
  | Map3SampleMean       !Word16
  | MapMorningValues     !Word16
  | MapEveningValues     !Word16
  | BPHeartRate          !Word8
  | BPHeartRateType      !T.HRType
  | BPStatus             !T.BPStatus
  | BPUserProfileIndex   !T.MessageIndex
  deriving (Show)

data MonitoringInfo =
    MonitoringInfoTimestamp            !DateTime
  | MonitoringInfoLocalTimestamp       !LocalDateTime
  | MonitoringInfoActivityType         !T.ActivityType
  | MonitoringInfoCyclesToDistance     !Float
  | MonitoringInfoCyclesToCalories     !Float
  | MonitoringInfoRestingMetabolicRate !Word16
  deriving (Show)

data Monitoring =
    MonitoringTimestamp                    !DateTime
  | MonitoringDeviceIndex                  !T.DeviceIndex
  | MonitoringCalories                     !Word16
  | MonitoringDistance                     !Float
  | MonitoringCycles                       !Float -- MonitoringCyclesSubfield
  | MonitoringActiveTime                   !Float
  | MonitoringActivityType                 !T.ActivityType
  | MonitoringActivitySubtype              !T.ActivitySubtype
  | MonitoringActivityLevel                !T.ActivityLevel
  | MonitoringDistance16                   !Word16
  | MonitoringCycles16                     !Word16
  | MonitoringActiveTime16                 !Word16
  | MonitoringLocalTimestamp               !LocalDateTime
  | MonitoringTemperature                  !Float
  | MonitoringTemperatureMin               !Float
  | MonitoringTemperatureMax               !Float
  | MonitoringActivityTime                 !Word16
  | MonitoringActiveCalories               !Word16
  | MonitoringCurrentActivityTypeIntensity !T.ActivityType !T.Intensity
  | MonitoringTimestampMin8                !Word8
  | MonitoringTimestamp16                  !Word16
  | MonitoringHeartRate                    !Word8
  | MonitoringIntensity                    !Float
  | MonitoringDurationMin                  !Word16
  | MonitoringDuration                     !Word32
  deriving (Show)

data MonitoringCyclesSubfield =
    MonitoringSteps   !Float
  | MonitoringStrokes !Float
  | MonitoringCycles' !Float
  deriving (Show)

data MemoGlob =
    MGPartIndex     !Word32
  | MGMemo          ![Word8]
  | MGMessageNumber !Word16
  | MGMessageIndex  !T.MessageIndex
  deriving (Show)

