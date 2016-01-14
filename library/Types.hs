module Types where

import Data.Word
import Enum

import qualified Data.Map as M

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
          deriving (Show)

instance Enum' File where
  fromEnum' file = case file of
    Device            -> 1
    Settings          -> 2
    Sport             -> 3
    Activity          -> 4
    Workout           -> 5
    Course            -> 6
    Schedules         -> 7
    Weight            -> 9
    Totals            -> 10
    Goals             -> 11
    BloodPressure     -> 14
    MonitoringA       -> 15
    ActivitySummary   -> 20
    MonitoringDaily   -> 28
    MonitoringB       -> 32
    Segment           -> 34
    SegmentList       -> 35
    MfgRangeMin       -> 0xF7
    MfgRangeMax       -> 0xFE

  toEnum' n
    | n == 1    = Just Device
    | n == 2    = Just Settings       
    | n == 3    = Just Sport          
    | n == 4    = Just Activity       
    | n == 5    = Just Workout        
    | n == 6    = Just Course         
    | n == 7    = Just Schedules      
    | n == 9    = Just Weight         
    | n == 10   = Just Totals         
    | n == 11   = Just Goals          
    | n == 14   = Just BloodPressure  
    | n == 15   = Just MonitoringA    
    | n == 20   = Just ActivitySummary
    | n == 28   = Just MonitoringDaily
    | n == 32   = Just MonitoringB    
    | n == 34   = Just Segment        
    | n == 35   = Just SegmentList    
    | n == 0xF7 = Just MfgRangeMin    
    | n == 0xFE = Just MfgRangeMax    
    | otherwise = Nothing

data MesgNum =
    FileIDNum
  | CapabilitiesNum
  | DeviceSettingsNum
  | UserProfileNum
  | HrmProfileNum
  | SdmProfileNum
  | BikeProfileNum
  | ZonesTargetNum
  | HRZoneNum
  | PowerZoneNum
  | MetZoneNum
  | SportNum
  | GoalNum
  | SessionNum
  | LapNum
  | RecordNum
  | EventNum
  | DeviceInfoNum
  | WorkoutNum
  | WorkoutStepNum
  | ScheduleNum
  | WeightScaleNum
  | CourseNum
  | CoursePointNum
  | TotalsNum
  | ActivityNum
  | SoftwareNum
  | FileCapabilitiesNum
  | MesgCapabilitiesNum
  | FieldCapabilitiesNum
  | FileCreatorNum
  | BloodPressureNum
  | SpeedZoneNum
  | MonitoringNum
  | TrainingFileNum
  | HrvNum
  | LengthNum
  | MonitoringInfoNum
  | PadNum
  | SlaveDeviceNum
  | CadenceZoneNum
  | SegmentLapNum
  | MemoGlobNum
  | SegmentIDNum
  | SegmentLeaderboardEntryNum
  | SegmentPointNum
  | SegmentFileNum
  | GpsMetadataNum
  | CameraEventNum
  | TimestampCorrelationNum
  | GyroscopeDataNum
  | AccelerometerDataNum
  | ThreeDSensorCalibrationNum
  | VideoFrameNum
  | ObdiiDataNum
  | NmeaSentenceNum
  | AviationAttitudeNum
  | VideoNum
  | VideoTitleNum
  | VideoDescriptionNum
  | VideoClipNum
  | MfgRangeMinNum
  | MfgRangeMaxNum
  deriving (Show)

data Checksum = 
    Clear 
  | Ok
  deriving (Show, Enum, Bounded)

instance Enum' Checksum where
  fromEnum' Clear = 0
  fromEnum' Ok = 1
  toEnum' 0 = Just $ toEnum 0
  toEnum' 1 = Just $ toEnum 1
  toEnum' n = Nothing

data FileFlag =
    Read
  | Write
  | Erase
  deriving (Show, Enum)

instance Enum' FileFlag where
  fromEnum' ff = 2^(1 + fromEnum ff)
  toEnum' 2 = Just $ toEnum 2
  toEnum' 4 = Just $ toEnum 4
  toEnum' 8 = Just $ toEnum 8
  toEnum' n = Nothing

data MesgCount =
    NumPerFile
  | MaxPerFile
  | MaxPerFileType
  deriving (Show, Enum, Bounded)

data MessageIndex =
    Selected
  | Reserved
  | Mask
  deriving (Show)

instance Enum' MessageIndex where
  toEnum' 0x8000 = Just Selected
  toEnum' 0x7000 = Just Reserved
  toEnum' 0x0FFF = Just Mask
  toEnum' _      = Nothing
  fromEnum' = undefined

newtype DeviceIndex = 
    Creator Word8
  deriving (Show)

data Gender =
    Female
  | Male
  deriving (Show, Enum, Bounded)

data Language =
    English
  | French
  | Italian
  | German
  | Spanish
  | Croatian
  | Czech
  | Danish
  | Dutch
  | Finnish
  | Greek
  | Hungarian
  | Norwegian
  | Polish
  | Portuguese
  | Slovakian
  | Slovenian
  | Swedish
  | Russian
  | Turkish
  | Latvian
  | Ukrainian
  | Arabic
  | Farsi
  | Bulgarian
  | Romanian
  | CustomLanguage
  deriving (Show)

data TimeZone =
    Almaty
  | Bangkok
  | Bombay
  | Brasilia
  | Cairo
  | CapeVerdeIs
  | Darwin
  | Eniwetok
  | Fiji
  | HongKong
  | Islamabad
  | Kabul
  | Magadan
  | MidAtlantic
  | Moscow
  | Muscat
  | Newfoundland
  | Samoa
  | Sydney
  | Tehran
  | Tokyo
  | UsAlaska
  | UsAtlantic
  | UsCentral
  | UsEastern
  | UsHawaii
  | UsMountain
  | UsPacific
  | OtherTZ
  | Auckland
  | Kathmandu
  | EuropeWesternWet
  | EuropeCentralCet
  | EuropeEasternEet
  | Jakarta
  | Perth
  | Adelaide
  | Brisbane
  | Tasmania
  | Iceland
  | Amsterdam
  | Athens
  | Barcelona
  | Berlin
  | Brussels
  | Budapest
  | Copenhagen
  | Dublin
  | Helsinki
  | Lisbon
  | London
  | Madrid
  | Munich
  | Oslo
  | Paris
  | Prague
  | Reykjavik
  | Rome
  | Stockholm
  | Vienna
  | Warsaw
  | Zurich
  | Quebec
  | Ontario
  | Manitoba
  | Saskatchewan
  | Alberta
  | BritishColumbia
  | Boise
  | Boston
  | Chicago
  | Dallas
  | Denver
  | KansasCity
  | LasVegas
  | LosAngeles
  | Miami
  | Minneapolis
  | NewYork
  | NewOrleans
  | Phoenix
  | SantaFe
  | Seattle
  | WashingtonDc
  | UsArizona
  | Chita
  | Ekaterinburg
  | Irkutsk
  | Kaliningrad
  | Krasnoyarsk
  | Novosibirsk
  | PetropavlovskKamchatskiy
  | Samara
  | Vladivostok
  | MexicoCentral
  | MexicoMountain
  | MexicoPacific
  | CapeTown
  | Winkhoek
  | Lagos
  | Riyahd
  | Venezuela
  | AustraliaLh
  | Santiago
  | ManualTZ
  | AutomaticTZ
  deriving (Show)

data DisplayMeasure = 
    Metric 
  | Statute
  deriving (Show, Enum, Bounded)

data DisplayHeart = 
    BPM
  | Max
  | Reserve
  deriving (Show, Enum, Bounded)

data DisplayPower = 
    WattsDP
  | PercentFTPDP
  deriving (Show, Enum, Bounded)

data DisplayPosition =
    Degree
  | DegreeMinute
  | DegreeMinuteSecond
  | AustrianGrid
  | BritishGrid
  | DutchGrid
  | HungarianGrid
  | FinnishGrid
  | GermanGrid
  | IcelandicGrid
  | IndonesianEquatorial
  | IndonesianIrian
  | IndonesianSouthern
  | IndiaZone0
  | IndiaZoneIA
  | IndiaZoneIB
  | IndiaZoneIIA
  | IndiaZoneIIB
  | IndiaZoneIIIA
  | IndiaZoneIIIB
  | IndiaZoneIVA
  | IndiaZoneIVB
  | IrishTransverse
  | IrishGrid
  | Loran
  | MaidenheadGrid
  | MgrsGrid
  | NewZealandGrid
  | NewZealandTransverse
  | QatarGrid
  | ModifiedSwedishGrid
  | SwedishGrid
  | SouthAfricanGrid
  | SwissGrid
  | TaiwanGrid
  | UnitedStatesGrid
  | UtmUpsGrid
  | WestMalayan
  | BorneoRso
  | EstonianGrid
  | LatvianGrid
  | SwedishRef99Grid
  deriving (Show, Enum, Bounded)

data Sport =
    GenericSport
  | Running
  | Cycling
  | Transition
  | FitnessEquipment
  | Swimming
  | Basketball
  | Soccer
  | Tennis
  | AmericanFootball
  | Training
  | Walking
  | CrossCountrySkiing
  | AlpineSkiing
  | Snowboarding
  | Rowing
  | Mountaineering
  | Hiking
  | Multisport
  | Paddling
  | Flying
  | EBiking
  | Motorcycling
  | Boating
  | Driving
  | Golf
  | HangGliding
  | HorsebackRiding
  | Hunting
  | Fishing
  | InlineSkating
  | RockClimbing
  | Sailing
  | IceSkating
  | SkyDiving
  | Snowshoeing
  | Snowmobiling
  | StandUpPaddleboarding
  | Surfing
  | Wakeboarding
  | WaterSkiing
  | Kayaking
  | Rafting
  | Windsurfing
  | Kitesurfing
  | AllSports
  deriving (Show)

data SportBits0 =
    Generic0
  | Running0
  | Cycling0
  | Transition0
  | FitnessEquipment0
  | Swimming0
  | Basketball0
  | Soccer0
  deriving (Show)

data SportBits1 =
    Tennis1
  | AmericanFootball1
  | Training1
  | Walking1
  | CrossCountrySkiing1
  | AlpineSkiing1
  | Snowboarding1
  | Rowing1
  deriving (Show)

data SportBits2 =
    Mountaineering2
  | Hiking2
  | Multisport2
  | Paddling2
  | Flying2
  | EBiking2
  | Motorcycling2
  | Boating2
  deriving (Show)

data SportBits3 =
    Driving3
  | Golf3
  | HangGliding3
  | HorsebackRiding3
  | Hunting3
  | Fishing3
  | InlineSkating3
  | RockClimbing3
  deriving (Show)

data SportBits4 =
    Sailing4
  | IceSkating4
  | SkyDiving4
  | Snowshoeing4
  | Snowmobiling4
  | StandUpPaddleboarding4
  | Surfing4
  | Wakeboarding4
  deriving (Show)

data SportBits5 =
    WaterSkiing5
  | Kayaking5
  | Rafting5
  | Windsurfing5
  | Kitesurfing5
  deriving (Show)

data SubSport =
    GenericSubSport
  | TreadmillSubSport
  | StreetSubSport
  | TrailSubSport
  | TrackSubSport
  | SpinSubSport
  | IndoorCyclingSubSport
  | RoadSubSport
  | MountainSubSport
  | DownhillSubSport
  | RecumbentSubSport
  | CyclocrossSubSport
  | HandCyclingSubSport
  | TrackCyclingSubSport
  | IndoorRowingSubSport
  | EllipticalSubSport
  | StairClimbingSubSport
  | LapSwimmingSubSport
  | OpenWaterSubSport
  | FlexibilityTrainingSubSport
  | StrengthTrainingSubSport
  | WarmUpSubSport
  | MatchSubSport
  | ExerciseSubSport
  | ChallengeSubSport
  | IndoorSkiingSubSport
  | CardioTrainingSubSport
  | IndoorWalkingSubSport
  | EBikeFitnessSubSport
  | BMXSubSport
  | CasualWalkingSubSport
  | SpeedWalkingSubSport
  | BikeToRunTransitionSubSport
  | RunToBikeTransitionSubSport
  | SwimToBikeTransitionSubSport
  | ATVSubSport
  | MotocrossSubSport
  | BackcountrySubSport
  | ResortSubSport
  | RCDroneSubSport
  | WingsuitSubSport
  | WhitewaterSubSport
  | AllSubSports
  deriving (Show)

data SportEvent =
    UncategorizedSportEvent
  | Geocaching
  | Fitness
  | Recreation
  | Race
  | SpecialEvent
  | TrainingSportEvent
  | Transportation
  | Touring
  deriving (Show, Enum, Bounded)

data Activity = 
    Manual
  | AutoMultiSport
  deriving (Show, Enum, Bounded)

data Intensity =
    Active
  | Rest
  | Warmup
  | Cooldown
  deriving (Show, Enum, Bounded)

data SessionTrigger = 
    STActivityEnd
  | STManual
  | STAutoMultiSport
  | STFitnessEquipment
  deriving (Show, Enum, Bounded)

data AutolapTrigger =
    ATTime
  | ATDistance
  | ATPositionStart
  | ATPositionLap
  | ATPositionWaypoint
  | ATPositionMarked
  | ATOff
  deriving (Show, Enum, Bounded)

data LapTrigger =
    LTManual
  | LTTime
  | LTDistance
  | LTPositionStart
  | LTPositionLap
  | LTPositionWaypoint
  | LTPositionMarked
  | LTSessionEnd
  | LTFitnessEquipment
  deriving (Show, Enum, Bounded)

data Event =
    EventTimer
  | EventWorkout
  | EventWorkoutStep
  | EventPowerDown
  | EventPowerUp
  | EventOffCourse
  | EventSession
  | EventLap
  | EventCoursePoint
  | EventBattery
  | EventVirtualPartnerPace
  | EventHRHighAlert
  | EventHRLowAlert
  | EventSpeedHighAlert
  | EventSpeedLowAlert
  | EventCadHighAlert
  | EventCadLowAlert
  | EventPowerHighAlert
  | EventPowerLowAlert
  | EventRecoveryHR
  | EventBatteryLow
  | EventTimeDurationAlert
  | EventDistanceDurationAlert
  | EventCalorieDurationAlert
  | EventActivity
  | EventFitnessEquipment
  | EventLength
  | EventUserMarker
  | EventSportPoint
  | EventCalibration
  | EventFrontGearChange
  | EventRearGearChange
  | EventRiderPositionChange
  | EventElevHighAlert
  | EventElevLowAlert
  | EventCommTimeout
  deriving (Show)

instance Enum' Event where
  toEnum' n
    | n == 0  = Just EventTimer
    | n == 3  = Just EventWorkout
    | n == 4  = Just EventWorkoutStep
    | n == 5  = Just EventPowerDown
    | n == 6  = Just EventPowerUp
    | n == 7  = Just EventOffCourse
    | n == 8  = Just EventSession
    | n == 9  = Just EventLap
    | n == 10 = Just EventCoursePoint
    | n == 11 = Just EventBattery
    | n == 12 = Just EventVirtualPartnerPace
    | n == 13 = Just EventHRHighAlert
    | n == 14 = Just EventHRLowAlert
    | n == 15 = Just EventSpeedHighAlert
    | n == 16 = Just EventSpeedLowAlert
    | n == 17 = Just EventCadHighAlert
    | n == 18 = Just EventCadLowAlert
    | n == 19 = Just EventPowerHighAlert
    | n == 20 = Just EventPowerLowAlert
    | n == 21 = Just EventRecoveryHR
    | n == 22 = Just EventBatteryLow
    | n == 23 = Just EventTimeDurationAlert
    | n == 24 = Just EventDistanceDurationAlert
    | n == 25 = Just EventCalorieDurationAlert
    | n == 26 = Just EventActivity
    | n == 27 = Just EventFitnessEquipment
    | n == 28 = Just EventLength
    | n == 32 = Just EventUserMarker
    | n == 33 = Just EventSportPoint
    | n == 36 = Just EventCalibration
    | n == 42 = Just EventFrontGearChange
    | n == 43 = Just EventRearGearChange
    | n == 44 = Just EventRiderPositionChange
    | n == 45 = Just EventElevHighAlert
    | n == 46 = Just EventElevLowAlert
    | n == 47 = Just EventCommTimeout
    | otherwise = Nothing
  fromEnum' = undefined

data EventType =
    Start
  | Stop
  | ConsecutiveDepreciated
  | Marker
  | StopAll
  | BeginDepreciated
  | EndDepreciated
  | EndAllDepreciated
  | StopDisable
  | StopDisableAll
  deriving (Show, Enum, Bounded)

instance Enum' EventType where
  toEnum' n
    | n == 0 = Just Start
    | n == 1 = Just Stop
    | n == 2 = Just ConsecutiveDepreciated
    | n == 3 = Just Marker
    | n == 4 = Just StopAll
    | n == 5 = Just BeginDepreciated
    | n == 6 = Just EndDepreciated
    | n == 7 = Just EndAllDepreciated
    | n == 8 = Just StopDisable
    | n == 9 = Just StopDisableAll
    | otherwise = Nothing
  fromEnum' = undefined

data TimeTrigger =
    TTManual
  | TTAuto
  | TTFitnessEquipment
  deriving (Show, Enum, Bounded)

data FitnessEquipmentState =
    Ready
  | InUse
  | Paused
  | UnknownFES
  deriving (Show, Enum, Bounded)

data ActivityClass =
    Level
  | LevelMax
  | Athlete
  deriving (Show)

data HRZoneCalc =
    CustomHRZC
  | PercentMaxHR
  | PercentHRr
  deriving (Show, Enum, Bounded)

data PwrZoneCalc =
    CustomPwrZC
  | PercentFTPCalc
  deriving (Show, Enum, Bounded)

data WktStepDuration =
    WktTime
  | WktDistance
  | WktHRLessThan
  | WktHRGreaterThan
  | WktCalories
  | WktOpen
  | WktRepeatUntilStepsCmplt
  | WktRepeatUntilTime
  | WktRepeatUntilDistance
  | WktRepeatUntilCalories
  | WktRepeatUntilHRLessThan
  | WktRepeatUntilHRGreaterThan
  | WktRepeatUntilPowerLessThan
  | WktRepeatUntilPowerGreaterThan
  | WktPowerLessThan
  | WktPowerGreaterThan
  | WktRepetitionTime
  deriving (Show)

data WktStepTarget =
    SpeedWST
  | HeartRateWST
  | OpenWST
  | CadenceWST
  | PowerWST
  | GradeWST
  | ResistanceWST
  deriving (Show, Enum, Bounded)

data Goal =
    TimeGoal
  | DistanceGoal
  | CaloriesGoal
  | FrequencyGoal
  | StepsGoal
  deriving (Show, Enum, Bounded)

data GoalRecurrence =
    Off
  | Daily
  | Weekly
  | Monthly
  | Yearly
  | CustomGR
  deriving (Show, Enum, Bounded)

data Schedule =
    WorkoutSchedule
  | CourseSchedule
  deriving (Show, Enum, Bounded)

data CoursePoint =
    GenericCoursePoint
  | Summit
  | Valley
  | Water
  | Food
  | Danger
  | Left
  | Right
  | Straight
  | FirstAid
  | FourthCategory
  | ThirdCategory
  | SecondCategory
  | FirstCategory
  | HorsCategory
  | Sprint
  | LeftFork
  | RightFork
  | MiddleFork
  | SlightLeft
  | SharpLeft
  | SlightRight
  | SharpRight
  | UTurn
  deriving (Show, Enum, Bounded)

data Manufacturer =
    UnknownManufacturer
  | Garmin
  | GarminFr405Antfs
  | Zephyr
  | Dayton
  | Idt
  | Srm
  | Quarq
  | Ibike
  | Saris
  | SparkHk
  | Tanita
  | Echowell
  | DynastreamOem
  | Nautilus
  | Dynastream
  | Timex
  | Metrigear
  | Xelic
  | Beurer
  | Cardiosport
  | AAndD
  | Hmm
  | Suunto
  | ThitaElektronik
  | Gpulse
  | CleanMobile
  | PedalBrain
  | Peaksware
  | Saxonar
  | LemondFitness
  | Dexcom
  | WahooFitness
  | OctaneFitness
  | Archinoetics
  | TheHurtBox
  | CitizenSystems
  | Magellan
  | Osynce
  | Holux
  | Concept2
  | OneGiantLeap
  | AceSensor
  | BrimBrothers
  | Xplova
  | PerceptionDigital
  | Bf1systems
  | Pioneer
  | Spantec
  | Metalogics
  | Fouriiiis
  | SeikoEpson
  | SeikoEpsonOem
  | IforPowell
  | MaxwellGuider
  | StarTrac
  | Breakaway
  | AlatechTechnologyLtd
  | MioTechnologyEurope
  | Rotor
  | Geonaute
  | IdBike
  | Specialized
  | Wtek
  | PhysicalEnterprises
  | NorthPoleEngineering
  | Bkool
  | Cateye
  | StagesCycling
  | Sigmasport
  | Tomtom
  | Peripedal
  | Wattbike
  | Moxy
  | Ciclosport
  | Powerbahn
  | AcornProjectsAps
  | Lifebeam
  | Bontrager
  | Wellgo
  | Scosche
  | Magura
  | Woodway
  | Elite
  | NielsenKellerman
  | DkCity
  | Tacx
  | DirectionTechnology
  | Magtonic
  | Onepartcarbon
  | InsideRideTechnologies
  | SoundOfMotion
  | Stryd
  | Icg
  | MiPulse
  | BsxAthletics
  | Look
  | Development
  | Healthandlife
  | Lezyne
  | ScribeLabs
  | Zwift
  | Watteam
  | Recon
  | FaveroElectronics
  | Dynovelo
  | Strava
  | Actigraphcorp
  deriving (Show)

instance Enum' Manufacturer where
  toEnum' n = M.lookup n manufacturerMap
  fromEnum' = undefined

manufacturerMap :: M.Map Word16 Manufacturer
manufacturerMap = M.fromAscList
  [
    (0   , UnknownManufacturer)
  , (1   , Garmin)
  , (2   , GarminFr405Antfs)
  , (3   , Zephyr)
  , (4   , Dayton)
  , (5   , Idt)
  , (6   , Srm)
  , (7   , Quarq)
  , (8   , Ibike)
  , (9   , Saris)
  , (10  , SparkHk)
  , (11  , Tanita)
  , (12  , Echowell)
  , (13  , DynastreamOem)
  , (14  , Nautilus)
  , (15  , Dynastream)
  , (16  , Timex)
  , (17  , Metrigear)
  , (18  , Xelic)
  , (19  , Beurer)
  , (20  , Cardiosport)
  , (21  , AAndD)
  , (22  , Hmm)
  , (23  , Suunto)
  , (24  , ThitaElektronik)
  , (25  , Gpulse)
  , (26  , CleanMobile)
  , (27  , PedalBrain)
  , (28  , Peaksware)
  , (29  , Saxonar)
  , (30  , LemondFitness)
  , (31  , Dexcom)
  , (32  , WahooFitness)
  , (33  , OctaneFitness)
  , (34  , Archinoetics)
  , (35  , TheHurtBox)
  , (36  , CitizenSystems)
  , (37  , Magellan)
  , (38  , Osynce)
  , (39  , Holux)
  , (40  , Concept2)
  , (42  , OneGiantLeap)
  , (43  , AceSensor)
  , (44  , BrimBrothers)
  , (45  , Xplova)
  , (46  , PerceptionDigital)
  , (47  , Bf1systems)
  , (48  , Pioneer)
  , (49  , Spantec)
  , (50  , Metalogics)
  , (51  , Fouriiiis)
  , (52  , SeikoEpson)
  , (53  , SeikoEpsonOem)
  , (54  , IforPowell)
  , (55  , MaxwellGuider)
  , (56  , StarTrac)
  , (57  , Breakaway)
  , (58  , AlatechTechnologyLtd)
  , (59  , MioTechnologyEurope)
  , (60  , Rotor)
  , (61  , Geonaute)
  , (62  , IdBike)
  , (63  , Specialized)
  , (64  , Wtek)
  , (65  , PhysicalEnterprises)
  , (66  , NorthPoleEngineering)
  , (67  , Bkool)
  , (68  , Cateye)
  , (69  , StagesCycling)
  , (70  , Sigmasport)
  , (71  , Tomtom)
  , (72  , Peripedal)
  , (73  , Wattbike)
  , (76  , Moxy)
  , (77  , Ciclosport)
  , (78  , Powerbahn)
  , (79  , AcornProjectsAps)
  , (80  , Lifebeam)
  , (81  , Bontrager)
  , (82  , Wellgo)
  , (83  , Scosche)
  , (84  , Magura)
  , (85  , Woodway)
  , (86  , Elite)
  , (87  , NielsenKellerman)
  , (88  , DkCity)
  , (89  , Tacx)
  , (90  , DirectionTechnology)
  , (91  , Magtonic)
  , (92  , Onepartcarbon)
  , (93  , InsideRideTechnologies)
  , (94  , SoundOfMotion)
  , (95  , Stryd)
  , (96  , Icg)
  , (97  , MiPulse)
  , (98  , BsxAthletics)
  , (99  , Look)
  , (255 , Development)
  , (257 , Healthandlife)
  , (258 , Lezyne)
  , (259 , ScribeLabs)
  , (260 , Zwift)
  , (261 , Watteam)
  , (262 , Recon)
  , (263 , FaveroElectronics)
  , (264 , Dynovelo)
  , (265 , Strava)
  , (5759, Actigraphcorp)
  ]

data GarminProduct =
    Hrm1
  | Axh01
  | Axb01
  | Axb02
  | Hrm2ss
  | DsiAlf02
  | Hrm3ss
  | HrmRunSingleByteProductID
  | Bsm
  | Bcm
  | Axs01
  | HrmTriSingleByteProductID
  | Fr225SingleByteProductID
  | Fr301China
  | Fr301Japan
  | Fr301Korea
  | Fr301Taiwan
  | Fr405
  | Fr50
  | Fr405Japan
  | Fr60
  | DsiAlf01
  | Fr310xt
  | Edge500
  | Fr110
  | Edge800
  | Edge500Taiwan
  | Edge500Japan
  | Chirp
  | Fr110Japan
  | Edge200
  | Fr910xt
  | Edge800Taiwan
  | Edge800Japan
  | Alf04
  | Fr610
  | Fr210Japan
  | VectorSs
  | VectorCp
  | Edge800China
  | Edge500China
  | Fr610Japan
  | Edge500Korea
  | Fr70
  | Fr310xt4t
  | Amx
  | Fr10
  | Edge800Korea
  | Swim
  | Fr910xtChina
  | Fenix
  | Edge200Taiwan
  | Edge510
  | Edge810
  | Tempe
  | Fr910xtJapan
  | Fr620
  | Fr220
  | Fr910xtKorea
  | Fr10Japan
  | Edge810Japan
  | VirbElite
  | EdgeTouring
  | Edge510Japan
  | HrmTri
  | HrmRun
  | Fr920xt
  | Edge510Asia
  | Edge810China
  | Edge810Taiwan
  | Edge1000
  | VivoFit
  | VirbRemote
  | VivoKi
  | Fr15
  | VivoActive
  | Edge510Korea
  | Fr620Japan
  | Fr620China
  | Fr220Japan
  | Fr220China
  | ApproachS6
  | VivoSmart
  | Fenix2
  | Epix
  | Fenix3
  | Edge1000Taiwan
  | Edge1000Japan
  | Fr15Japan
  | Edge520
  | Edge1000China
  | Fr620Russia
  | Fr220Russia
  | VectorS
  | Edge1000Korea
  | Fr920xtTaiwan
  | Fr920xtChina
  | Fr920xtJapan
  | Virbx
  | VivoSmartApac
  | EtrexTouch
  | Edge25
  | VivoFit2
  | Fr225
  | VivoActiveApac
  | Vector2
  | Vector2s
  | Virbxe
  | Fr620Taiwan
  | Fr220Taiwan
  | Fenix3China
  | Fenix3Twn
  | VariaHeadlight
  | VariaTaillightOld
  | Fr225Asia
  | VariaRadarTaillight
  | VariaRadarDisplay
  | Edge20
  | D2Bravo
  | VariaRemote
  | Sdm4
  | EdgeRemote
  | TrainingCenter
  | AndroidAntplusPlugin
  | Connect
  | UnknownGarminProd -- to be removed
  deriving (Show)

instance Enum' GarminProduct where
  toEnum' n = M.lookup n garminProductMap
  fromEnum' = undefined

garminProductMap :: M.Map Word16 GarminProduct
garminProductMap = M.fromAscList
  [
    (1    , Hrm1)
  , (2    , Axh01)
  , (3    , Axb01)
  , (4    , Axb02)
  , (5    , Hrm2ss)
  , (6    , DsiAlf02)
  , (7    , Hrm3ss)
  , (8    , HrmRunSingleByteProductID)
  , (9    , Bsm)
  , (10   , Bcm)
  , (11   , Axs01)
  , (12   , HrmTriSingleByteProductID)
  , (14   , Fr225SingleByteProductID)
  , (473  , Fr301China)
  , (474  , Fr301Japan)
  , (475  , Fr301Korea)
  , (494  , Fr301Taiwan)
  , (717  , Fr405)
  , (782  , Fr50)
  , (987  , Fr405Japan)
  , (988  , Fr60)
  , (1011 , DsiAlf01)
  , (1018 , Fr310xt)
  , (1036 , Edge500)
  , (1124 , Fr110)
  , (1169 , Edge800)
  , (1199 , Edge500Taiwan)
  , (1213 , Edge500Japan)
  , (1253 , Chirp)
  , (1274 , Fr110Japan)
  , (1325 , Edge200)
  , (1328 , Fr910xt)
  , (1333 , Edge800Taiwan)
  , (1334 , Edge800Japan)
  , (1341 , Alf04)
  , (1345 , Fr610)
  , (1360 , Fr210Japan)
  , (1380 , VectorSs)
  , (1381 , VectorCp)
  , (1386 , Edge800China)
  , (1387 , Edge500China)
  , (1410 , Fr610Japan)
  , (1422 , Edge500Korea)
  , (1436 , Fr70)
  , (1446 , Fr310xt4t)
  , (1461 , Amx)
  , (1482 , Fr10)
  , (1497 , Edge800Korea)
  , (1499 , Swim)
  , (1537 , Fr910xtChina)
  , (1551 , Fenix)
  , (1555 , Edge200Taiwan)
  , (1561 , Edge510)
  , (1567 , Edge810)
  , (1570 , Tempe)
  , (1600 , Fr910xtJapan)
  , (1623 , Fr620)
  , (1632 , Fr220)
  , (1664 , Fr910xtKorea)
  , (1688 , Fr10Japan)
  , (1721 , Edge810Japan)
  , (1735 , VirbElite)
  , (1736 , EdgeTouring)
  , (1742 , Edge510Japan)
  , (1743 , HrmTri)
  , (1752 , HrmRun)
  , (1765 , Fr920xt)
  , (1821 , Edge510Asia)
  , (1822 , Edge810China)
  , (1823 , Edge810Taiwan)
  , (1836 , Edge1000)
  , (1837 , VivoFit)
  , (1853 , VirbRemote)
  , (1885 , VivoKi)
  , (1903 , Fr15)
  , (1907 , VivoActive)
  , (1918 , Edge510Korea)
  , (1928 , Fr620Japan)
  , (1929 , Fr620China)
  , (1930 , Fr220Japan)
  , (1931 , Fr220China)
  , (1936 , ApproachS6)
  , (1956 , VivoSmart)
  , (1967 , Fenix2)
  , (1988 , Epix)
  , (2050 , Fenix3)
  , (2052 , Edge1000Taiwan)
  , (2053 , Edge1000Japan)
  , (2061 , Fr15Japan)
  , (2067 , Edge520)
  , (2070 , Edge1000China)
  , (2072 , Fr620Russia)
  , (2073 , Fr220Russia)
  , (2079 , VectorS)
  , (2100 , Edge1000Korea)
  , (2130 , Fr920xtTaiwan)
  , (2131 , Fr920xtChina)
  , (2132 , Fr920xtJapan)
  , (2134 , Virbx)
  , (2135 , VivoSmartApac)
  , (2140 , EtrexTouch)
  , (2147 , Edge25)
  , (2150 , VivoFit2)
  , (2153 , Fr225)
  , (2160 , VivoActiveApac)
  , (2161 , Vector2)
  , (2162 , Vector2s)
  , (2172 , Virbxe)
  , (2173 , Fr620Taiwan)
  , (2174 , Fr220Taiwan)
  , (2188 , Fenix3China)
  , (2189 , Fenix3Twn)
  , (2192 , VariaHeadlight)
  , (2193 , VariaTaillightOld)
  , (2219 , Fr225Asia)
  , (2225 , VariaRadarTaillight)
  , (2226 , VariaRadarDisplay)
  , (2238 , Edge20)
  , (2262 , D2Bravo)
  , (2276 , VariaRemote)
  , (10007, Sdm4)
  , (10014, EdgeRemote)
  , (20119, TrainingCenter)
  , (65532, AndroidAntplusPlugin)
  , (65534, Connect)
  ]


data AntplusDeviceType =
    AntfsADT
  | BikePowerADT
  | EnvironmentSensorLegacyADT
  | MultiSportSpeedDistanceADT
  | ControlADT
  | FitnessEquipmentADT
  | BloodPressureADT
  | GeocacheNodeADT
  | LightElectricVehicleADT
  | EnvSensorADT
  | RacquetADT
  | WeightScaleADT
  | HeartRateADT
  | BikeSpeedCadenceADT
  | BikeCadenceADT
  | BikeSpeedADT
  | StrideSpeedDistanceADT
  deriving (Show)

data AntNetwork =
    PublicAN
  | AntplusAN
  | AntfsAN
  | PrivateAN
  deriving (Show, Enum, Bounded)

instance Enum' AntNetwork where
  toEnum' n
    | n == 0 = Just PublicAN
    | n == 1 = Just AntplusAN
    | n == 2 = Just AntfsAN
    | n == 3 = Just PrivateAN
    | otherwise = Nothing
  fromEnum' = fromIntegral . fromEnum

data WorkoutCapabilities =
    IntervalWC
  | CustomWC
  | FitnessEquipmentWC
  | FirstbeatWC
  | NewLeafWC
  | TcxWC
  | SpeedWC
  | HeartRateWC
  | DistanceWC
  | CadenceWC
  | PowerWC
  | GradeWC
  | ResistanceWC
  | ProtectedWC
  deriving (Show)

data BatteryStatus =
    NewBS
  | GoodBS
  | OkBS
  | LowBS
  | CriticalBS
  | UnknownBS
  deriving (Show)

instance Enum' BatteryStatus where
  toEnum' n
    | n == 1 = Just NewBS
    | n == 2 = Just GoodBS
    | n == 3 = Just OkBS
    | n == 4 = Just LowBS
    | n == 5 = Just CriticalBS
    | n == 7 = Just UnknownBS
    | otherwise = Nothing
  fromEnum' = undefined

data HRType =
    Normal
  | Irregular
  deriving (Show, Enum, Bounded)

data CourseCapabilities =
    ProcessedCC
  | ValidCC
  | TimeCC
  | DistanceCC
  | PositionCC
  | HeartRateCC
  | PowerCC
  | CadenceCC
  | TrainingCC
  | NavigationCC
  | BikewayCC
  deriving (Show)

newtype Weight = Calculating Word16
  deriving (Show)

data WorkoutHR =
    PercentMax Word32
  | WorkoutBPM Word32
  deriving (Show)

data WorkoutPower =
    PercentFTP
  | WorkoutWatts
  deriving (Show)

data BPStatus =
    NoError
  | ErrorIncompleteData
  | ErrorNoMeasurement
  | ErrorDataOutOfRange
  | ErrorIrregularHeartRate
  deriving (Show, Enum, Bounded)

data UserLocalID =
    LocalMin
  | LocalMax
  | StationaryMin
  | StationaryMax
  | PortableMin
  | PortableMax
  deriving (Show)

data SwimStroke =
    Freestyle
  | Backstroke
  | Breaststroke
  | Butterfly
  | Drill
  | Mixed
  | Im
  deriving (Show, Enum, Bounded)

data ActivityType =
    GenericActivity
  | RunningActivity
  | CyclingActivity
  | TransitionActivity
  | FitnessEquipmentActivity
  | SwimmingActivity
  | WalkingActivity
  | AllActivities
  deriving (Show)

{-activityTypeMap :: M.Map Word8 ActivityType-}
activityTypeMap = M.fromAscList
  [
    (0,   GenericActivity)
  , (1,   RunningActivity)
  , (2,   CyclingActivity)
  , (3,   TransitionActivity)
  , (4,   FitnessEquipmentActivity)
  , (5,   SwimmingActivity)
  , (6,   WalkingActivity)
  , (254, AllActivities)
  ]

instance Enum' ActivityType where
  toEnum' n = M.lookup n activityTypeMap
  fromEnum' = undefined

data ActivitySubtype =
    GenericSubactivity
  | TreadmillSubactivity
  | StreetSubactivity
  | TrailSubactivity
  | TrackSubactivity
  | SpinSubactivity
  | IndoorCyclingSubactivity
  | RoadSubactivity
  | MountainSubactivity
  | DownhillSubactivity
  | RecumbentSubactivity
  | CyclocrossSubactivity
  | HandCyclingSubactivity
  | TrackCyclingSubactivity
  | IndoorRowingSubactivity
  | EllipticalSubactivity
  | StairClimbingSubactivity
  | LapSwimmingSubactivity
  | OpenWaterSubactivity
  | AllSubactivities
  deriving (Show)

data ActivityLevel =
    Low
  | Medium
  | High
  deriving (Show, Enum, Bounded)

data LeftRightBalance =
    LRBMask
  | LRBRight
  deriving (Show, Enum, Bounded)

instance Enum' LeftRightBalance where
  toEnum' 0 = Just LRBMask
  toEnum' 1 = Just LRBRight
  toEnum' _ = Nothing
  fromEnum' LRBMask  = 0
  fromEnum' LRBRight = 1

data LeftRightBalance100 =
    LRBMask100
  | LRBRight100
  deriving (Show, Enum, Bounded)

data LengthType =
    IdleLength
  | ActiveLength
  deriving (Show, Enum, Bounded)

data ConnectivityCapabilities =
    BluetoothCC
  | BluetoothLeCC
  | AntCC
  | ActivityUploadCC
  | CourseDownloadCC
  | WorkoutDownloadCC
  | LiveTrackCC
  | WeatherConditionsCC
  | WeatherAlertsCC
  | GpsEphemerisDownloadCC
  | ExplicitArchiveCC
  | SetupIncompleteCC
  | ContinueSyncAfterSoftwareUpdateCC
  | ConnectIqAppDownloadCC
  deriving (Show)

data StrokeType =
    NoEvent
  | OtherStrokeType
  | Serve
  | Forehand
  | Backhand
  | Smash
  deriving (Show, Enum, Bounded)

{-strokeTypeMap :: M.Map Word8 StrokeType-}
strokeTypeMap = M.fromAscList
  [
    (0, NoEvent)
  , (1, OtherStrokeType)
  , (2, Serve)
  , (3, Forehand)
  , (4, Backhand)
  , (5, Smash)
  ]

instance Enum' StrokeType where
  toEnum' n = M.lookup n strokeTypeMap
  fromEnum' = undefined

data BodyLocation =
    LeftLeg
  | LeftCalf
  | LeftShin
  | LeftHamstring
  | LeftQuad
  | LeftGlute
  | RightLeg
  | RightCalf
  | RightShin
  | RightHamstring
  | RightQuad
  | RightGlute
  | TorsoBack
  | LeftLowerBack
  | LeftUpperBack
  | RightLowerBack
  | RightUpperBack
  | TorsoFront
  | LeftAbdomen
  | LeftChest
  | RightAbdomen
  | RightChest
  | LeftArm
  | LeftShoulder
  | LeftBicep
  | LeftTricep
  | LeftBrachioradialis
  | LeftForearmExtensors
  | RightArm
  | RightShoulder
  | RightBicep
  | RightTricep
  | RightBrachioradialis
  | RightForearmExtensors
  | Neck
  | Throat
  deriving (Show, Enum, Bounded)

instance Enum' BodyLocation where
  toEnum' n 
    | n >= minBound && n <= maxBound = Just . toEnum . fromIntegral $ n
    | otherwise                      = Nothing
  fromEnum' = fromIntegral . fromEnum

data SegmentLapStatus =
    End
  | Fail
  deriving (Show, Enum, Bounded)

data SegmentLeaderboardType =
    Overall
  | PersonalBest
  | Connections
  | Group
  | Challenger
  | Kom
  | Qom
  | PR
  | Goal
  | Rival
  | ClubLeader
  deriving (Show, Enum, Bounded)

data SegmentDeleteStatus =
    DoNotDelete
  | DeleteOne
  | DeleteAll
  deriving (Show, Enum, Bounded)

data SegmentSelectionType =
    Starred
  | Suggested
  deriving (Show, Enum, Bounded)

data SourceType =
    Ant
  | Antplus
  | Bluetooth
  | BluetoothLowEnergy
  | Wifi
  | Local
  deriving (Show, Enum, Bounded)

instance Enum' SourceType where
  toEnum' n
    | n == 0 = Just Ant
    | n == 1 = Just Antplus
    | n == 2 = Just Bluetooth
    | n == 3 = Just BluetoothLowEnergy
    | n == 4 = Just Wifi
    | n == 5 = Just Local
    | otherwise = Nothing
  fromEnum' = fromIntegral . fromEnum

data RiderPositionType =
    Seated
  | Standing
  deriving (Show, Enum, Bounded)

data PowerPhasetype =
    PowerPhaseStartAngle
  | PowerPhaseEndAngle
  | PowerPhaseArcLength
  | PowerPhaseCenter
  deriving (Show, Enum, Bounded)

data CameraEventType =
    VideoStart
  | VideoSplit
  | VideoEnd
  | PhotoTaken
  | VideoSecondStreamStart
  | VideoSecondStreamSplit
  | VideoSecondStreamEnd
  | VideoSplitStart
  | VideoSecondStreamSplitStart
  deriving (Show, Enum, Bounded)

data SensorType =
    Accelerometer
  | Gyroscope
  | Compass
  deriving (Show, Enum, Bounded)

data CommTimeoutType =
    WildcardPairingTimeout
  | PairingTimeout
  | ConnectionLost
  | ConnectionTimeout
  deriving (Show, Enum, Bounded)

data CameraOrientationType =
    CameraOrientation0
  | CameraOrientation90
  | CameraOrientation180
  | CameraOrientation270
  deriving (Show, Enum, Bounded)

data AttitudeStage =
    Failed
  | Aligning
  | Degraded
  | Valid
  deriving (Show, Enum, Bounded)

data AttitudeValidity =
    TrackAngleHeadingValid
  | PitchValid
  | RollValid
  | LateralBodyAccelValid
  | NormalBodyAccelValid
  | TurnRateValid
  | HwFail
  | MagInvalid
  | NoGps
  | GpsInvalid
  | SolutionCoasting
  | TrueTrackAngle
  | MagneticHeading
  deriving (Show)

