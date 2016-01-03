module Types where

import Data.Word

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

data FileFlag =
    Read
  | Write
  | Erase
  deriving (Show)

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
    Garmin
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

