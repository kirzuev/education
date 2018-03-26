module Model where

import Data.List (find, zipWith4)
import System.Random

data Experiment = Experiment
  { bank          :: Bank
  , newRequests   :: RequestsQueue
  , parameters    :: Parameters
  , statistic     :: Statistic
  , isInitialized :: Bool
  , isPaused      :: Bool
  , isEnded       :: Bool
  } deriving (Eq)

initExperiment :: Experiment
initExperiment = Experiment
  { bank          = initBank
  , newRequests   = initNewRequests
  , parameters    = initParameters
  , statistic     = initStatistic
  , isInitialized = False
  , isPaused      = True
  , isEnded       = False
  }

changeParameter :: ParametersField -> ChangeAction -> Experiment -> Experiment
changeParameter ClerksNum action ex = ex
  { parameters = (parameters ex)
      { clerksNum = case action of
          Sub -> clerksNum (parameters ex) - 1
          Add -> clerksNum (parameters ex) + 1
      }
  , bank = case action of
      Sub -> delClerk (bank ex)
      Add -> addClerk (bank ex)
  }
changeParameter field action ex = ex
  { parameters = case field of
      TimeStep         -> (parameters ex)
        { timeStep = case action of
            Sub -> timeStep (parameters ex) - 10
            Add -> timeStep (parameters ex) + 10
        }
      SimulationPeriod -> (parameters ex)
        { simulationPeriod = case action of
            Sub -> simulationPeriod (parameters ex) - 7
            Add -> simulationPeriod (parameters ex) + 7
        }
      ClerksNum        -> (parameters ex)
        { clerksNum = case action of
            Sub -> clerksNum (parameters ex) - 1
            Add -> clerksNum (parameters ex) + 1
        }
      QueueLenLimit    -> (parameters ex)
        { queueLenLimit = case action of
            Sub -> queueLenLimit (parameters ex) - 5
            Add -> queueLenLimit (parameters ex) + 5
        }
      ServiceMinTime   -> (parameters ex)
        { serviceMinTime = case action of
            Sub -> serviceMinTime (parameters ex) - 4
            Add -> serviceMinTime (parameters ex) + 4
        }
      ServiceMaxTime   -> (parameters ex)
        { serviceMaxTime = case action of
            Sub -> serviceMaxTime (parameters ex) - 4
            Add -> serviceMaxTime (parameters ex) + 4
        }
      ComingMinTime    -> (parameters ex)
        { comingMinTime = case action of
            Sub -> comingMinTime (parameters ex) - 2
            Add -> comingMinTime (parameters ex) + 2
        }
      ComingMaxTime    -> (parameters ex)
        { comingMaxTime = case action of
            Sub -> comingMaxTime (parameters ex) - 2
            Add -> comingMaxTime (parameters ex) + 2
        }
  }

startExperiment :: StdGen -> Experiment -> Experiment
startExperiment g ex = ex
  { newRequests   = genNewRequests g (parameters ex)
  , isInitialized = True
  }

playPauseExperiment :: Experiment -> Experiment
playPauseExperiment ex = ex
  { isPaused  = not (isPaused ex)
  }

resetExperiment :: Experiment
resetExperiment = initExperiment

finishExperiment :: Experiment -> Experiment
finishExperiment ex = addTime timeToEnd ex
  where
    timeToEnd     = daysToEnd * day + timeToDaysEnd
    daysToEnd     = simulationPeriod (parameters ex) - spentDays (statistic ex) - 1
    timeToDaysEnd = 24 * hour - currentTime (statistic ex)

addTime :: Minutes -> Experiment -> Experiment
addTime t ex
  | t <= 0    = ex
  | otherwise = addTime (t - 1) ex
    { statistic = (statistic ex)
      { spentDays   = newSpentDays
      , currentDay  = newDay
      , currentTime = if simulationIsEnded
        then 0
        else newTime `mod` day
      }
    , isEnded = simulationIsEnded
    }
  where
    newSpentDays = spentDays (statistic ex) + daysChanges
    daysChanges  = newTime `div` day
    newTime      = currentTime (statistic ex) + 1
    newDay
      | daysChanges > 0 = addDays daysChanges (currentDay (statistic ex))
      | otherwise       = currentDay (statistic ex)
    simulationIsEnded = newSpentDays >= simulationPeriod (parameters ex)

isWorkingTime :: Experiment -> Bool
isWorkingTime ex =
  (day_ `elem` fulltimeDays) && (inFulltimeShedule time)
  || (day_ `elem` parttimeDays) && (inParttimeShedule time)
  where
    day_  = currentDay  (statistic ex)
    time = currentTime (statistic ex)
    inFulltimeShedule t =
      (t >= fst fulltimeWorkShedule) && (t <= snd fulltimeWorkShedule)
      && (t <= fst fulltimeDinnerShedule) && (t >= snd fulltimeDinnerShedule)
    inParttimeShedule t =
      (t >= fst parttimeWorkShedule) && (t <= snd parttimeWorkShedule)
      && (t <= fst parttimeDinnerShedule) && (t >= snd parttimeDinnerShedule)

initClerks :: [Clerk]
initClerks = take (fst defaultClerksNums) defaultClerks

initInfoTable :: [TableLine]
initInfoTable = initTableLine <$> initClerks

initNewRequests :: RequestsQueue
initNewRequests = []

genNewRequests :: StdGen -> Parameters -> RequestsQueue
genNewRequests g params = zipWith4 mkRequest profits days times durations
  where
    (g1, g')      = split g
    (g2, g3)      = split g'
    profits       = randomRs defaultProfitInterval g1
    durations     = randomRs (serviceMinTime params, serviceMaxTime params) g2
    (days, times) = unzip $ takeWhile (\(d,_) -> d < simulationPeriod params) $
      genNewTime g3 params (0,0)

genNewTime :: StdGen -> Parameters -> (Int, Minutes) -> [(Int, Minutes)]
genNewTime g params (prevDay, prevTime) =
  newTime : genNewTime g' params newTime
  where
    (genTime, g') = randomR (comingMinTime params, comingMaxTime params) g
    currDay       = prevDay + (prevTime + genTime) `div` day
    currTime      = (prevTime + genTime) `mod` day
    newTime       = (currDay, currTime)

addClient :: Request -> Experiment -> Experiment
addClient req ex = ex
  { bank = (bank ex)
    { queue = queue (bank ex) ++ [client]
    }
  , statistic = (statistic ex)
    { clientsCome = clientsCome (statistic ex) + 1
    }
  }
  where
    client = Client
      { request  = req
      , waitTime = 0
      , number   = head $ dropWhile (`elem` usedNumbers) defaultClientNumbers
      }
    usedNumbers = number <$> queue (bank ex) 

leaveClient :: Client -> Experiment -> Experiment
leaveClient client ex = ex
  { bank = (bank ex)
    { queue = filter (/= client) (queue (bank ex))
    }
  , statistic = (statistic ex)
    { clientsLeft = clientsLeft (statistic ex) + 1
    }
  }

data Bank = Bank
  { queue     :: ClientsQueue
  , infoTable :: [TableLine]
  , clerks    :: [Clerk]
  } deriving (Eq)

initBank :: Bank
initBank = Bank
  { queue     = []
  , infoTable = initInfoTable
  , clerks    = initClerks
  }

setWorkToClerks :: Bank -> Bank
setWorkToClerks bank_ = bank_
  { clerks    = newClerks
  , infoTable = newInfoTable
  }
  where
    freeClerks    = filter withoutWork (clerks bank_)
    readyClients  = take (length freeClerks) (queue bank_)
    updatedClerks = zipWith takeClientToClerk readyClients freeClerks
    newClerks     = setWork <$> clerks bank_
    setWork clerk = case find (== clerk) updatedClerks of
      Nothing  -> clerk
      Just new -> new
    newInfoTable  = updateInfoTable newClerks (infoTable bank_)

delClient :: Client -> Bank -> Bank
delClient client bank_ = bank_
  { queue = filter (/= client) (queue bank_)
  }

addClerk :: Bank -> Bank
addClerk bank_
  | amount == snd defaultClerksNums = bank_
  | otherwise                             = bank_
    { clerks    = clerks bank_ ++ [defaultClerks !! amount]
    , infoTable = initTableLine <$> newClerks
    }
  where
    amount    = length (clerks bank_)
    newClerks = clerks bank_ ++ [defaultClerks !! amount]

delClerk :: Bank -> Bank
delClerk bank_
  | amount == fst defaultClerksNums = bank_
  | otherwise                             = bank_
    { clerks    = take (amount - 1) (clerks bank_)
    , infoTable = delTableLine deletedClerk (infoTable bank_)
    }
  where
    amount       = length (clerks bank_)
    deletedClerk = last   (clerks bank_)

data Client = Client
  { request      :: Request
  , waitTime     :: Minutes
  , number       :: Int
  } deriving (Eq)

data Request = Request
  { profit       :: Money
  , dayToComing  :: Int
  , timeToComing :: Minutes
  , duration     :: Minutes
  } deriving (Eq)

mkRequest :: Money -> Int -> Minutes -> Minutes -> Request
mkRequest profit_ day_ time_ duration_ = Request
  { profit       = profit_
  , dayToComing  = day_
  , timeToComing = time_
  , duration     = duration_
  }

data TableLine = TableLine
  { tableClerk  :: String
  , tableClient :: Maybe Int
  } deriving (Eq)

delTableLine :: Clerk -> [TableLine] -> [TableLine]
delTableLine clerk table =
  filter (\line -> tableClerk line /= name clerk) table

addTableLine :: Clerk -> [TableLine] -> [TableLine]
addTableLine clerk table = initTableLine clerk : table

initTableLine :: Clerk -> TableLine
initTableLine clerk = TableLine
  { tableClerk  = name clerk
  , tableClient = Nothing
  }

updateInfoTable :: [Clerk] -> [TableLine] -> [TableLine]
updateInfoTable [] table = table
updateInfoTable (clerk : cs) table =
  updateInfoTable cs $ addTableLine clerk $ delTableLine clerk table

data Clerk = Clerk
  { name   :: String
  , salary :: Money
  , work   :: Maybe Client
  } deriving (Eq)

takeClientToClerk :: Client -> Clerk -> Clerk
takeClientToClerk client clerk = clerk
  { work = Just client
  }

withoutWork :: Clerk -> Bool
withoutWork clerk = work clerk == Nothing

data Parameters = Parameters
  { timeStep         :: Minutes
  , simulationPeriod :: Int
  , clerksNum        :: Int
  , queueLenLimit    :: Int
  , serviceMinTime   :: Minutes
  , serviceMaxTime   :: Minutes
  , comingMinTime    :: Minutes
  , comingMaxTime    :: Minutes
  } deriving (Eq)

initParameters :: Parameters
initParameters = Parameters
  { timeStep         = fst defaultTimeSteps
  , simulationPeriod = fst defaultSimulationPeriods
  , clerksNum        = fst defaultClerksNums
  , queueLenLimit    = fst defaultQueueLenLimits
  , serviceMinTime   = fst defaultServiceTimes
  , serviceMaxTime   = snd defaultServiceTimes
  , comingMinTime    = fst defaultComingTimes
  , comingMaxTime    = snd defaultComingTimes
  }

data Statistic = Statistic
  { servicedClientsNum   :: Int
  , leftClientsNum       :: Int
  , minQueueLen          :: Int
  , medQueueLen          :: Int
  , maxQueueLen          :: Int
  , medClientWaitingTime :: Minutes
  , medClerksWorkTime    :: Minutes
  , bankProfit           :: Money
  , spentDays            :: Int
  , currentDay           :: Day
  , currentTime          :: Minutes
  , clientsCome          :: Int
  , clientsLeft          :: Int
  } deriving (Eq)

initStatistic :: Statistic
initStatistic = Statistic
  { servicedClientsNum   = 0
  , leftClientsNum       = 0
  , minQueueLen          = 0
  , medQueueLen          = 0
  , maxQueueLen          = 0
  , medClientWaitingTime = 0
  , medClerksWorkTime    = 0
  , bankProfit           = 0
  , spentDays            = 0
  , currentDay           = head fulltimeDays
  , currentTime          = fst fulltimeWorkShedule
  , clientsCome          = 0
  , clientsLeft          = 0
  }

type Minutes       = Int
type Money         = Int
type ClientsQueue  = [Client]
type RequestsQueue = [Request]

data Day
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq)

addDays :: Int -> Day -> Day
addDays 0 day_ = day_
addDays n day_ = addDays (n - 1) nextDay
  where
    nextDay = case day_ of
      Monday    -> Tuesday
      Tuesday   -> Wednesday
      Wednesday -> Thursday
      Thursday  -> Friday
      Friday    -> Saturday
      Saturday  -> Sunday
      Sunday    -> Monday

data Action
  = NoOp
  | StartExperiment
  | PlayPauseExperiment
  | ResetExperiment
  | ChangeParameter ParametersField ChangeAction
  | AddTime Minutes
  | FinishExperiment
  deriving (Eq)

data ParametersField
  = TimeStep
  | SimulationPeriod
  | ClerksNum
  | QueueLenLimit
  | ServiceMinTime
  | ServiceMaxTime
  | ComingMinTime
  | ComingMaxTime
  deriving (Eq)

data ChangeAction = Sub | Add
  deriving (Eq)

defaultTimeSteps :: (Minutes, Minutes)
defaultTimeSteps = (10, 60)

defaultClerksNums :: (Int, Int)
defaultClerksNums = (2, 7)

defaultClerks :: [Clerk]
defaultClerks = mkClerk <$> defaultClerksNames

mkClerk :: String -> Clerk
mkClerk s = Clerk
  { name   = s
  , salary = defaultClerkSalary
  , work   = Nothing
  }

defaultClerkSalary :: Money
defaultClerkSalary = 2

defaultClerksNames :: [String]
defaultClerksNames = (\num -> "Clerk " ++ show num) <$> [1,2..]

defaultClientNumbers :: [Int]
defaultClientNumbers = [1,2..]

defaultQueueLenLimits :: (Int, Int)
defaultQueueLenLimits = (10, 25)

defaultServiceTimes :: (Minutes, Minutes)
defaultServiceTimes = (2, 30)

defaultComingTimes :: (Minutes, Minutes)
defaultComingTimes = (0, 10)

defaultSimulationPeriods :: (Int, Int)
defaultSimulationPeriods = (7, 28)

hour :: Minutes
hour = 60

day :: Minutes
day = 24 * hour

fulltimeDays :: [Day]
fulltimeDays = [Monday, Tuesday, Wednesday, Thursday, Friday]

parttimeDays :: [Day]
parttimeDays = [Saturday]

fulltimeWorkShedule :: (Minutes, Minutes)
fulltimeWorkShedule = (10 * hour, 19 * hour)

fulltimeDinnerShedule :: (Minutes, Minutes)
fulltimeDinnerShedule = (14 * hour, 15 * hour)

parttimeWorkShedule :: (Minutes, Minutes)
parttimeWorkShedule = (10 * hour, 17 * hour)

parttimeDinnerShedule :: (Minutes, Minutes)
parttimeDinnerShedule = (13 * hour, 14 * hour)

defaultProfitInterval :: (Money, Money)
defaultProfitInterval = (3, 50)
