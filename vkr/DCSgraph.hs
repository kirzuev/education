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
  { states = ['A', 'B']
  , symbols = ['a', 'b']
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
      , symbol   = Just 'b'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 1)
      , newState = 'B'
      }

-- | Is the L-graph determined?
isDetermined :: Lgraph -> Bool
isDetermined g =
  and $ concat $ map (\s -> map simpleTrans $ pairs $ filter (\t -> oldState t == s) $ transitions g) $ states g

-- | Make the all pairs of the transitions list
pairs :: [Line] -> [(Line, Line)]
pairs [] = []
pairs (x:[]) = []
pairs (x:xs) = zip (x:xs) xs ++ pairs xs

-- | Check for the determined transition from one state for two lines
simpleTrans :: (Line, Line) -> Bool
simpleTrans (x, y) = case (symbol x, symbol y) of
  (Nothing, Nothing) ->
    checkBrackets (firstBr x) (firstBr y) || checkBrackets (secondBr x) (secondBr y)
  (Just a, Just b)   -> if a /= b then True
    else checkBrackets (firstBr x) (firstBr y) || checkBrackets (secondBr x) (secondBr y)
  (_, _)             -> False

checkBrackets :: Maybe Bracket -> Maybe Bracket -> Bool
checkBrackets (Just (R a)) (Just (R b)) = a /= b
checkBrackets _          _          = False
