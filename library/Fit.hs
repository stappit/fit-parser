module Fit where

import Data.Word
import Data.Int
import Data.Bits

import Timestamp
import BaseType
import CRC
import Profiles

data Fit = Fit !FitHeader ![Message] !CRC
  deriving Show

data FitHeader = FitHdr !HeaderSize !ProtocolVersion !ProfileVersion !MessageSize !(Maybe CRC)
  deriving Show

data HeaderSize = Twelve | Fourteen
  deriving (Show)

type ProtocolVersion = Word8
type ProfileVersion  = Word16
type MessageSize     = Int64

data Header = 
    DefnH !LocalMsgNum
  | DataH !LocalMsgNum 
  | CompH !LocalMsgNum !Offset
  deriving Show

type Offset = Word8

makeHeader :: Word8 -> Header
makeHeader n
  | testBit n 7 =
      let offsetMask        = 31
          compressedMsgMask = 96
          lNum               = fromIntegral $ (n .&. compressedMsgMask) `shiftR` 5
          offset            = n .&. offsetMask
      in  CompH (LNum lNum) offset
  | otherwise   = 
      let normalMsgMask = 15 
          lNum           = fromIntegral $ n .&. normalMsgMask
      in  if testBit n 6 then DefnH (LNum lNum) else DataH (LNum lNum) 

data Message = 
    DefnM !DefinitionMessage 
  | DataM !DataMessage
  deriving Show

data DefinitionMessage = Def !LocalMsgNum !Arch !GlobalMsgNum ![FieldDefinition]
  deriving (Show)

data Arch = BigEndian | LittleEndian
  deriving (Show)

newtype LocalMsgNum  = LNum Word16
  deriving (Show, Eq, Ord)

newtype GlobalMsgNum = GNum Word16
  deriving (Show, Eq)

data FieldDefinition = FieldDef !FieldNumber !FieldSize !BaseType
  deriving Show

newtype FieldNumber = FNum Word8
  deriving (Show, Eq)

type FieldSize   = Int
type BaseType    = Word8

data DataMessage = Dat !LocalMsgNum !(Maybe Timestamp) !Profile
  deriving Show

data Profile = 
    FileIDProfile                  ![FileID]
  | FileCreatorProfile             ![FileCreator]
  | TimestampCorrelationProfile    ![TimestampCorrelation]
  | SoftwareProfile                ![Software]
  | SlaveDeviceProfile             ![SlaveDevice]
  | CapabilitiesProfile            ![Capabilities]
  | FileCapabilitiesProfile        ![FileCapabilities]
  | MesgCapabilitiesProfile        ![MesgCapabilities]
  | FieldCapabilitiesProfile       ![FieldCapabilities]
  | ActivityProfile                ![Activity]
  | SessionProfile                 ![Session]
  | LapProfile                     ![Lap]
  | LengthProfile                  ![Length]
  | RecProfile                     ![Record]
  | EventProfile                   ![Event]
  | DeviceInfoProfile              ![DeviceInfo]
  | TrainingFileProfile            ![TrainingFile]
  | HRVProfile                     ![HRV]
  | CameraEventProfile             ![CameraEvent]
  | GyroscopeDataProfile           ![GyroscopeData]
  | AccelerometerDataProfile       ![AccelerometerData]
  | ThreeDSensorCalibrationProfile ![ThreeDSensorCalibration]
  | VideoFrameProfile              ![VideoFrame]
  | ObdiiDataProfile               ![ObdiiData]
  | NmeaSentenceProfile            ![NmeaSentence]
  | AviationAttitudeProfile        ![AviationAttitude]
  | VideoProfile                   ![Video]
  | VideoTitleProfile              ![VideoTitle]
  | VideoDescriptionProfile        ![VideoDescription]
  | VideoClipProfile               ![VideoClip]
  | CourseProfile                  ![Course]
  | CoursePointProfile             ![CoursePoint]
  | SegmentIDProfile               ![SegmentID]
  | SegmentLeaderboardEntryProfile ![SegmentLeaderboardEntry]
  | SegmentPointProfile            ![SegmentPoint]
  | SegmentLapProfile              ![SegmentLap]
  | SegmentFileProfile             ![SegmentFile]
  | WorkoutProfile                 ![Workout]
  | WorkoutStepProfile             ![WorkoutStep]
  | ScheduleProfile                ![Schedule]
  | TotalsProfile                  ![Totals]
  | WeightScaleProfile             ![WeightScale]
  | BloodPressureProfile           ![BloodPressure]
  | MonitoringInfoProfile          ![MonitoringInfo]
  | MonitoringProfile              ![Monitoring]
  | MemoGlobProfile                ![MemoGlob]
  | NoProfile
  deriving (Show)

