module Model where

data Experiment = Experiment
  { bank        :: Bank
  , newRequests :: RequestsQueue
  , parameters  :: Parameters
  , statistic   :: Statistic
  , isStarted   :: Bool
  } deriving (Eq)

initExperiment :: Experiment
initExperiment  = Experiment
  { bank        = initBank
  , newRequests = genNewRequests
  , parameters  = initParameters
  , statistic   = initStatistic
  , isStarted   = False
  }

initClerks :: [Clerk]
initClerks = take (fst defaultClerksNums) defaultClerks

genNewRequests :: RequestsQueue
genNewRequests = []

data Bank = Bank
  { queue     :: ClientsQueue
  , infoTable :: InfoTable
  , clerks    :: [Clerk]
  } deriving (Eq)

initBank :: Bank
initBank = Bank
  { queue     = []
  , infoTable = initInfoTable
  , clerks    = initClerks
  }

delClerk :: Bank -> Bank
delClerk bank_
  | amount == fst defaultClerksNums = bank_
  | otherwise                             = bank_
    { clerks    = take (amount - 1) (clerks bank_)
    , infoTable = delTableLine (infoTable bank_)
    }
  where
    amount = length (clerks bank_)

addClerk :: Bank -> Bank
addClerk bank_
  | amount == snd defaultClerksNums = bank_
  | otherwise                             = bank_
    { clerks = clerks bank_ ++ [defaultClerks !! amount]
    , infoTable = addTableLine (defaultClerks !! amount) (infoTable bank_)
    }
  where
    amount = length (clerks bank_)

data Client = Client
  { request      :: Request
  , number       :: Int
  } deriving (Eq)

data Request = Request
  { profit       :: Money
  , timeToComing :: Minutes
  , duration     :: Minutes
  } deriving (Eq)

data InfoTable = InfoTable
  { tableLines :: [TableLine]
  } deriving (Eq)

initInfoTable :: InfoTable
initInfoTable = InfoTable
  { tableLines = initTableLine <$> initClerks
  }

delTableLine :: InfoTable -> InfoTable
delTableLine table = table
  { tableLines = take (amount - 1) (tableLines table)
  }
  where
    amount = length (tableLines table)

addTableLine :: Clerk -> InfoTable -> InfoTable
addTableLine clerk table = table
  { tableLines = tableLines table ++ [initTableLine clerk]
  }

data TableLine = TableLine
  { tableClerk  :: String
  , tableClient :: Maybe Int
  } deriving (Eq)

initTableLine :: Clerk -> TableLine
initTableLine clerk = TableLine
  { tableClerk  = name clerk
  , tableClient = Nothing
  }

data Clerk = Clerk
  { name   :: String
  , salary :: Money
  , work   :: Maybe Client
  } deriving (Eq)

data Parameters = Parameters
  { timeStep       :: Minutes
  , clerksNum      :: Int
  , queueLenLimit  :: Int
  , serviceMinTime :: Minutes
  , serviceMaxTime :: Minutes
  } deriving (Eq)

initParameters :: Parameters
initParameters = Parameters
  { timeStep       = fst defaultTimeSteps
  , clerksNum      = fst defaultClerksNums
  , queueLenLimit  = fst defaultQueueLenLimits
  , serviceMinTime = fst defaultServiceTimes
  , serviceMaxTime = snd defaultServiceTimes
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
  , currentDay           = Monday
  , currentTime          = 0
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

data Action
  = NoOp
  | StartExperiment
  | RestartExperiment
  | ChangeParameter ParametersField ChangeAction
  deriving (Eq)

data ParametersField
  = TimeStep
  | ClerksNum
  | QueueLenLimit
  | ServiceMinTime
  | ServiceMaxTime
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
  , salary = 2
  , work   = Nothing
  }

defaultClerksNames :: [String]
defaultClerksNames = (\num -> "Clerk " ++ show num) <$> [1,2..]

defaultQueueLenLimits :: (Int, Int)
defaultQueueLenLimits = (10, 25)

defaultServiceTimes :: (Minutes, Minutes)
defaultServiceTimes = (2, 30)

hour :: Minutes
hour = 60

day :: Minutes
day = 24 * hour
