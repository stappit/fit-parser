module Fit where

import Data.Word
import Data.Int
import Data.Bits
import Timestamp
import BaseType
import CRC

import qualified Data.Map as M

data Fit = Fit FitHeader [Message] CRC
         deriving Show

data FitHeader = FitHdr HeaderSize Version MessageSize (Maybe CRC)
               deriving Show

data Message = DefnM Definition 
             | DataM Data
             deriving Show

data Definition = Defn {
  localMsgNum  :: LocalMsgNum,
  arch         :: Arch,
  globalMsgNum :: GlobalMsgNum,
  fieldDefs    :: [FieldDefinition]
} deriving Show

data Header = DefnH LocalMsgNum
            | DataH LocalMsgNum 
            | CompH LocalMsgNum Offset
            deriving Show

makeHeader :: Word8 -> Header
makeHeader n
  | testBit n 7 =
      let offsetMask        = 31
          compressedMsgMask = 96
          msg               = fromIntegral $ (n .&. compressedMsgMask) `shiftR` 5
          offset            = n .&. offsetMask
      in  CompH msg offset
  | otherwise   = 
      let normalMsgMask = 15 
          msg           = fromIntegral $ n .&. normalMsgMask
      in  if testBit n 6 then DefnH msg else DataH msg 

data MessageType = DefnType | DataType
                 deriving Show

data Arch = BigEndian | LittleEndian
          deriving Show

data FieldDefinition = FieldDef FieldNumber FieldSize BaseType
                     deriving Show

data Data = Data LocalMsgNum GlobalMsgNum (Maybe Timestamp) [Profile]
          deriving Show

data Field = Field String BaseTypeValue
           deriving Show

type Size        = Int64
type HeaderSize  = Int64
type MessageSize = Int64
type Version = Word8

type GlobalMsgNum = Word16

type LocalMsgNum = Word16
type Offset      = Word8

type FieldNumber = Word8
type FieldSize   = Int
type BaseType    = Word8

data Profile = ProfileFile   FileId
             | ProfileRecord Record
             | NoProfile
             deriving Show

profiles :: M.Map GlobalMsgNum (M.Map FieldNumber Modification)
profiles = M.fromAscList
    [
      (0,   fileProfile)
    , (20,  recordProfile)
    ]

data FileId = Type         File
            | Manufacturer Manufacturer
            | Product      GarminProduct
            | Serial       Word32
            | TimeCreated  DateTime
            | Number       Word16
            | ProductName  [Word8]
            deriving Show

data File = Device 
          | Settings
          | Sport
          | Activity
          | Workout
          | Course
          | Schedules
          | Weight
          | Totals
          | Goals
          | BloodPressure
          | MonitoringA
          | ActivitySummary
          | MonitoringDaily
          | MonitoringB
          | Segment
          | SegmentList
          | MfgRangeMin
          | MfgRangeMax
          deriving Show

fileMap :: M.Map FieldNumber File
fileMap = M.fromAscList [
                (1,    Device)
              , (2,    Settings)
              , (3,    Sport)
              , (4,    Activity)
              , (5,    Workout        )
              , (6,    Course         )
              , (7,    Schedules      )
              , (9,    Weight         )
              , (10,   Totals         )
              , (11,   Goals          )
              , (14,   BloodPressure  )
              , (15,   MonitoringA    )
              , (20,   ActivitySummary)
              , (28,   MonitoringDaily)
              , (32,   MonitoringB    )
              , (34,   Segment        )
              , (35,   SegmentList    )
              , (0xF7, MfgRangeMin)
              , (0xFE, MfgRangeMax)
              ]

type Modification = BaseTypeValue -> Maybe Profile
scaleBy = flip (/)
translateBy = flip (-)
find = flip M.lookup

fileProfile :: M.Map FieldNumber Modification
fileProfile = M.fromAscList
    [
      (0, fmap (ProfileFile . Type) . maybe Nothing (find fileMap) . unEnum)
    , (1, fmap (ProfileFile . Manufacturer) . maybe Nothing (find manufacturerMap . fromIntegral) . unUInt16)  
    , (2, const (Just $ ProfileFile (Product UnknownGarminProd)))  
    , (3, fmap (ProfileFile . Serial) . unUInt32z)  
    , (4, fmap (ProfileFile . TimeCreated . DateTime) . unUInt32)  
    , (5, fmap (ProfileFile . Number) . unUInt16)
    , (8, fmap (ProfileFile . ProductName) . unString)
    ]

data Manufacturer = Garmin
                  | Dynastream
                  | Suunto
                  | Specialized
                  | Cateye
                  | Tomtom
                  | Strava
                  deriving Show

manufacturerMap = M.fromAscList
  [
    (1, Garmin)
  , (15, Dynastream)  
  , (23, Suunto)  
  , (63, Specialized)  
  , (68, Cateye)  
  , (71, Tomtom)  
  , (265, Strava)  
  ]

data GarminProduct = FR220
                   | UnknownGarminProd
                   deriving Show

garminProductMap = M.fromAscList
  [
    (1632, Garmin)
  ]

data Record = Timestamp DateTime
            | PositionLat Int32
            | PositionLong Int32
            | Altitude Float
            | HeartRate Word8
            | Cadence Word8
            | Distance Float
            | Speed Float
            | Power Word16
            | CompressedSpeedDistance Float Float
            | NoRecord
            deriving Show

recordProfile :: M.Map FieldNumber Modification
recordProfile = M.fromAscList
    [
      (0,   fmap (ProfileRecord . PositionLat) . unSInt32)
    , (1,   fmap (ProfileRecord . PositionLong) . unSInt32)
    , (2,   fmap (ProfileRecord . Altitude . translateBy 500 . scaleBy 5 . fromIntegral) . unUInt16)
    , (3,   const (Just $ ProfileRecord NoRecord))
    , (4,   const (Just $ ProfileRecord NoRecord))
    , (5,   const (Just $ ProfileRecord NoRecord))
    , (6,   const (Just $ ProfileRecord NoRecord))
    , (7,   const (Just $ ProfileRecord NoRecord))
    , (8,   const (Just $ ProfileRecord NoRecord))
    , (9,   const (Just $ ProfileRecord NoRecord))
    , (10,  const (Just $ ProfileRecord NoRecord))
    , (11,  const (Just $ ProfileRecord NoRecord))
    , (12,  const (Just $ ProfileRecord NoRecord))
    , (13,  const (Just $ ProfileRecord NoRecord))
    , (17,  const (Just $ ProfileRecord NoRecord))
    , (18,  const (Just $ ProfileRecord NoRecord))
    , (19,  const (Just $ ProfileRecord NoRecord))
    , (28,  const (Just $ ProfileRecord NoRecord))
    , (29,  const (Just $ ProfileRecord NoRecord))
    , (30,  const (Just $ ProfileRecord NoRecord))
    , (31,  const (Just $ ProfileRecord NoRecord))
    , (32,  const (Just $ ProfileRecord NoRecord))
    , (33,  const (Just $ ProfileRecord NoRecord))
    , (39,  const (Just $ ProfileRecord NoRecord))
    , (40,  const (Just $ ProfileRecord NoRecord))
    , (41,  const (Just $ ProfileRecord NoRecord))
    , (42,  const (Just $ ProfileRecord NoRecord))
    , (43,  const (Just $ ProfileRecord NoRecord))
    , (44,  const (Just $ ProfileRecord NoRecord))
    , (45,  const (Just $ ProfileRecord NoRecord))
    , (46,  const (Just $ ProfileRecord NoRecord))
    , (47,  const (Just $ ProfileRecord NoRecord))
    , (48,  const (Just $ ProfileRecord NoRecord))
    , (49,  const (Just $ ProfileRecord NoRecord))
    , (50,  const (Just $ ProfileRecord NoRecord))
    , (51,  const (Just $ ProfileRecord NoRecord))
    , (52,  const (Just $ ProfileRecord NoRecord))
    , (53,  const (Just $ ProfileRecord NoRecord))
    , (54,  const (Just $ ProfileRecord NoRecord))
    , (55,  const (Just $ ProfileRecord NoRecord))
    , (56,  const (Just $ ProfileRecord NoRecord))
    , (57,  const (Just $ ProfileRecord NoRecord))
    , (58,  const (Just $ ProfileRecord NoRecord))
    , (59,  const (Just $ ProfileRecord NoRecord))
    , (62,  const (Just $ ProfileRecord NoRecord))
    , (67,  const (Just $ ProfileRecord NoRecord))
    , (68,  const (Just $ ProfileRecord NoRecord))
    , (69,  const (Just $ ProfileRecord NoRecord))
    , (70,  const (Just $ ProfileRecord NoRecord))
    , (71,  const (Just $ ProfileRecord NoRecord))
    , (72,  const (Just $ ProfileRecord NoRecord))
    , (73,  const (Just $ ProfileRecord NoRecord))
    , (78,  const (Just $ ProfileRecord NoRecord))
    , (81,  const (Just $ ProfileRecord NoRecord))
    , (82,  const (Just $ ProfileRecord NoRecord))
    , (253, fmap (ProfileRecord . Timestamp . DateTime) . unUInt32)
    ]
