data Lgraph = Lgraph
  { states      :: [State]
  , symbols     :: [Char]
  , beginStates :: [State]
  , finalStates :: [State]
  , transitions :: [Line]
  } deriving (Eq, Show)

data Bracket = L Int | R Int -- ^ Left/Right bracket with index
  deriving (Eq, Show)

type State = Char

data Line = Line
  { oldState :: State
  , symbol   :: Maybe Char
  , firstBr  :: Maybe Bracket
  , secondBr :: Maybe Bracket
  , newState :: State
  } deriving (Eq, Show)

test :: Lgraph
test = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Line
      { oldState = 'A'
      , symbol   = Just 'a'
      , firstBr  = Just (L 1)
      , secondBr = Just (L 1)
      , newState = 'A'
      }
    l2 = Line
      { oldState = 'A'
      , symbol   = Just 'b'
      , firstBr  = Just (L 1)
      , secondBr = Just (L 1)
      , newState = 'B'
      }
    l3 = Line
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 1)
      , newState = 'A'
      }
    l4 = Line
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 2)
      , newState = 'B'
      }

-- | Is the L-graph determined?
isLgraphDetermined :: Lgraph -> Bool
isLgraphDetermined g = and $ map isStateDetermined $ states g
  where
    isStateDetermined s =
      and $ map simpleTrans $ mkPairs $ takeTransitionsWithState s $ transitions g

-- | Take transitions which started from current state
takeTransitionsWithState :: State -> [Line] -> [Line]
takeTransitionsWithState s ts = filter (\t -> oldState t == s) ts

-- | Make the all pairs of the transitions list
mkPairs :: [Line] -> [(Line, Line)]
mkPairs [] = []
mkPairs (t:[]) = []
mkPairs (t:ts) = mkPairWith t ts ++ mkPairs ts

-- | Make pairs of transtions with first argument
mkPairWith :: Line -> [Line] -> [(Line, Line)]
mkPairWith t = map (\x -> (t,x))

-- | Check for the determined transition from one state for two lines
simpleTrans :: (Line, Line) -> Bool
simpleTrans (x, y) = case (symbol x, symbol y) of
  (Nothing, Nothing) ->
    checkBrackets (firstBr x) (firstBr y) || checkBrackets (secondBr x) (secondBr y)
  (Just a, Just b)   -> if a /= b then True
    else checkBrackets (firstBr x) (firstBr y) || checkBrackets (secondBr x) (secondBr y)
  (_, _)             -> False

-- | Check for the determined the transition by its brackets
checkBrackets :: Maybe Bracket -> Maybe Bracket -> Bool
checkBrackets (Just (R a)) (Just (R b)) = a /= b
checkBrackets _          _              = False
