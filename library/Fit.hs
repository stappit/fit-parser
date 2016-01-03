module Fit where

import Data.Word
import Data.Int
import Data.Bits
import Timestamp
import BaseType
import CRC

import Profiles
import qualified Types    as T

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

profiles :: M.Map GlobalMsgNum (M.Map FieldNumber Modification)
profiles = M.fromAscList
    [
      (0,   fileProfile)
    , (20,  recordProfile)
    ]

type Modification = BaseTypeValue -> Maybe Profile
scaleBy = flip (/) :: Float -> Float -> Float
translateBy = flip (-) :: Float -> Float -> Float
find = flip M.lookup 

fileMap :: M.Map Word8 T.File
fileMap = M.fromAscList [
                (1,    T.Device)
              , (2,    T.Settings)
              , (3,    T.Sport)
              , (4,    T.Activity)
              , (5,    T.Workout        )
              , (6,    T.Course         )
              , (7,    T.Schedules      )
              , (9,    T.Weight         )
              , (10,   T.Totals         )
              , (11,   T.Goals          )
              , (14,   T.BloodPressure  )
              , (15,   T.MonitoringA    )
              , (20,   T.ActivitySummary)
              , (28,   T.MonitoringDaily)
              , (32,   T.MonitoringB    )
              , (34,   T.Segment        )
              , (35,   T.SegmentList    )
              , (0xF7, T.MfgRangeMin)
              , (0xFE, T.MfgRangeMax)
              ]

fileProfile :: M.Map FieldNumber Modification
fileProfile = M.fromAscList
    [
      (0, fmap (FileIDProfile . Type) . maybe Nothing (find fileMap) . unEnum)
    , (1, fmap (FileIDProfile . Manufacturer) . maybe Nothing (flip M.lookup manufacturerMap . fromIntegral) . unUInt16)  
    , (2, const (Just $ FileIDProfile (Product T.UnknownGarminProd)))  
    , (3, fmap (FileIDProfile . Serial) . unUInt32z)  
    , (4, fmap (FileIDProfile . TimeCreated . DateTime) . unUInt32)  
    , (5, fmap (FileIDProfile . Number) . unUInt16)
    , (8, fmap (FileIDProfile . ProductName) . unString)
    ]

manufacturerMap :: M.Map Word16 T.Manufacturer
manufacturerMap = M.fromAscList
  [
    (1,   T.Garmin)
  , (15,  T.Dynastream)  
  , (23,  T.Suunto)  
  , (63,  T.Specialized)  
  , (68,  T.Cateye)  
  , (71,  T.Tomtom)  
  , (265, T.Strava)  
  ]

garminProductMap :: M.Map Word16 T.GarminProduct
garminProductMap = M.fromAscList
  [
    (1632, T.Fr220)
  ]

recordProfile :: M.Map FieldNumber Modification
recordProfile = M.fromAscList
    [
      (0,   fmap (RecProfile . RecPositionLat) . unSInt32)
    , (1,   fmap (RecProfile . RecPositionLong) . unSInt32)
    , (2,   fmap (RecProfile . RecAltitude . translateBy 500 . scaleBy 5 . fromIntegral) . unUInt16)
    , (3,   const (Just $ RecProfile NoRecord))
    , (4,   const (Just $ RecProfile NoRecord))
    , (5,   const (Just $ RecProfile NoRecord))
    , (6,   const (Just $ RecProfile NoRecord))
    , (7,   const (Just $ RecProfile NoRecord))
    , (8,   const (Just $ RecProfile NoRecord))
    , (9,   const (Just $ RecProfile NoRecord))
    , (10,  const (Just $ RecProfile NoRecord))
    , (11,  const (Just $ RecProfile NoRecord))
    , (12,  const (Just $ RecProfile NoRecord))
    , (13,  const (Just $ RecProfile NoRecord))
    , (17,  const (Just $ RecProfile NoRecord))
    , (18,  const (Just $ RecProfile NoRecord))
    , (19,  const (Just $ RecProfile NoRecord))
    , (28,  const (Just $ RecProfile NoRecord))
    , (29,  const (Just $ RecProfile NoRecord))
    , (30,  const (Just $ RecProfile NoRecord))
    , (31,  const (Just $ RecProfile NoRecord))
    , (32,  const (Just $ RecProfile NoRecord))
    , (33,  const (Just $ RecProfile NoRecord))
    , (39,  const (Just $ RecProfile NoRecord))
    , (40,  const (Just $ RecProfile NoRecord))
    , (41,  const (Just $ RecProfile NoRecord))
    , (42,  const (Just $ RecProfile NoRecord))
    , (43,  const (Just $ RecProfile NoRecord))
    , (44,  const (Just $ RecProfile NoRecord))
    , (45,  const (Just $ RecProfile NoRecord))
    , (46,  const (Just $ RecProfile NoRecord))
    , (47,  const (Just $ RecProfile NoRecord))
    , (48,  const (Just $ RecProfile NoRecord))
    , (49,  const (Just $ RecProfile NoRecord))
    , (50,  const (Just $ RecProfile NoRecord))
    , (51,  const (Just $ RecProfile NoRecord))
    , (52,  const (Just $ RecProfile NoRecord))
    , (53,  const (Just $ RecProfile NoRecord))
    , (54,  const (Just $ RecProfile NoRecord))
    , (55,  const (Just $ RecProfile NoRecord))
    , (56,  const (Just $ RecProfile NoRecord))
    , (57,  const (Just $ RecProfile NoRecord))
    , (58,  const (Just $ RecProfile NoRecord))
    , (59,  const (Just $ RecProfile NoRecord))
    , (62,  const (Just $ RecProfile NoRecord))
    , (67,  const (Just $ RecProfile NoRecord))
    , (68,  const (Just $ RecProfile NoRecord))
    , (69,  const (Just $ RecProfile NoRecord))
    , (70,  const (Just $ RecProfile NoRecord))
    , (71,  const (Just $ RecProfile NoRecord))
    , (72,  const (Just $ RecProfile NoRecord))
    , (73,  const (Just $ RecProfile NoRecord))
    , (78,  const (Just $ RecProfile NoRecord))
    , (81,  const (Just $ RecProfile NoRecord))
    , (82,  const (Just $ RecProfile NoRecord))
    , (253, fmap (RecProfile . RecTimestamp . DateTime) . unUInt32)
    ]
