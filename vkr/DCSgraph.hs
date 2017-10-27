import Data.List

data Lgraph = Lgraph
  { states      :: [State]
  , symbols     :: [Char]
  , beginStates :: [State]
  , finalStates :: [State]
  , transitions :: [Transition]
  } deriving (Eq, Show)

data Bracket = L Int | R Int -- ^ Left/Right bracket with index
  deriving (Eq, Show)

type State = Char

data Transition = Transition
  { oldState :: State
  , symbol   :: Maybe Char
  , firstBr  :: Maybe Bracket
  , secondBr :: Maybe Bracket
  , newState :: State
  } deriving (Eq, Show)

test1 :: Lgraph
test1 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState = 'A'
      , symbol   = Just 'b'
      , firstBr  = Just (L 1)
      , secondBr = Just (L 1)
      , newState = 'A'
      }
    l2 = Transition
      { oldState = 'A'
      , symbol   = Just 'b'
      , firstBr  = Just (L 2)
      , secondBr = Just (L 1)
      , newState = 'B'
      }
    l3 = Transition
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 1)
      , newState = 'A'
      }
    l4 = Transition
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 1)
      , newState = 'B'
      }

test2 :: Lgraph
test2 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState = 'A'
      , symbol   = Just 'a'
      , firstBr  = Just (L 1)
      , secondBr = Just (L 1)
      , newState = 'A'
      }
    l2 = Transition
      { oldState = 'A'
      , symbol   = Just 'b'
      , firstBr  = Just (L 2)
      , secondBr = Just (L 1)
      , newState = 'B'
      }
    l3 = Transition
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 1)
      , newState = 'A'
      }
    l4 = Transition
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 2)
      , newState = 'B'
      }

test3 :: Lgraph
test3 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState = 'A'
      , symbol   = Just 'a'
      , firstBr  = Nothing
      , secondBr = Nothing
      , newState = 'A'
      }
    l2 = Transition
      { oldState = 'A'
      , symbol   = Nothing
      , firstBr  = Nothing
      , secondBr = Nothing
      , newState = 'B'
      }
    l3 = Transition
      { oldState = 'B'
      , symbol   = Nothing
      , firstBr  = Nothing
      , secondBr = Nothing
      , newState = 'A'
      }
    l4 = Transition
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Nothing
      , secondBr = Nothing
      , newState = 'B'
      }

test4 :: Lgraph
test4 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState = 'A'
      , symbol   = Just 'a'
      , firstBr  = Just (R 1)
      , secondBr = Just (R 1)
      , newState = 'A'
      }
    l2 = Transition
      { oldState = 'A'
      , symbol   = Nothing
      , firstBr  = Nothing
      , secondBr = Just (L 1)
      , newState = 'B'
      }
    l3 = Transition
      { oldState = 'B'
      , symbol   = Nothing
      , firstBr  = Just (L 1)
      , secondBr = Just (L 1)
      , newState = 'A'
      }
    l4 = Transition
      { oldState = 'B'
      , symbol   = Just 'a'
      , firstBr  = Just (R 2)
      , secondBr = Nothing
      , newState = 'B'
      }

-- =====================
-- | Determine L-graph |
-- =====================

{-determineLgraph :: Lgraph -> Lgraph
determineLgraph g = g
  { transitions = determineAllTransitions g
  }

determineAllTransitions :: Lgraph -> [Transition]
determineAllTransitions g =
  concat $ map (determineTransitionsFromState (transitions g)) (states g)

determineTransitionsFromState :: [Transition] -> State -> [Transition]
determineTransitionsFromState ts s =
  determineTransitions $ filter (\t -> oldState t == s) ts

determineTransitions :: [Transition] -> [Transition]
determineTransitions []     = []
determineTransitions (t:ts) =
  case find (\y -> (not . simpleTrans) (t,y)) ts of
    Nothing -> t:ts
    Just x  -> determineTransitions $ newTransition t x : delete x ts

newTransition :: Transition -> Transition -> Transition
newTransition x y
  | x == y    = x
  | otherwise = x-}

-- ==============================
-- | Is the L-graph determined? |
-- ==============================

-- | Is the L-graph determined?
isLgraphDetermined :: Lgraph -> IO Bool
isLgraphDetermined g = andIO $ map isStateDetermined $ states g
  where
    isStateDetermined s =
      andIO $ map simpleTrans $ mkPairs $ takeTransitionsWithState s $ transitions g

andIO :: [IO Bool] -> IO Bool
andIO [] = return True
andIO (x:xs) = do
  val  <- x
  vals <- andIO xs
  return $ val && vals

-- | Take transitions which started from current state
takeTransitionsWithState :: State -> [Transition] -> [Transition]
takeTransitionsWithState s ts = filter (\t -> oldState t == s) ts

-- | Make the all pairs of the transitions list
mkPairs :: [Transition] -> [(Transition, Transition)]
mkPairs []     = []
mkPairs (t:[]) = []
mkPairs (t:ts) = mkPairsWith t ts ++ mkPairs ts

-- | Make pairs of transtions with first argument
mkPairsWith :: Transition -> [Transition] -> [(Transition, Transition)]
mkPairsWith t = map (\x -> (t,x))

showTrans :: Transition -> String
showTrans t = [oldState t] ++ " --{ " ++ stateMark ++ " }-> " ++ [newState t]
  where
    stateMark  = mark ++ " : " ++ fstBr ++ " : " ++ sndBr
    mark  = case symbol t of
      Nothing    -> "e"
      Just x     -> [x]
    fstBr = case firstBr t of
      Nothing    -> "e"
      Just (L x) -> "(_" ++ show x
      Just (R x) -> ")_" ++ show x
    sndBr = case secondBr t of
      Nothing    -> "e"
      Just (L x) -> "[_" ++ show x
      Just (R x) -> "]_" ++ show x

-- | Check for the determined transition from one state for two Transitions
simpleTrans :: (Transition, Transition) -> IO Bool
simpleTrans (x, y) = case (symbol x, symbol y) of
  (Nothing, Nothing) -> do
    fstVal <- checkBrackets (firstBr x) (firstBr y)
    sndVal <- checkBrackets (secondBr x) (secondBr y)
    if fstVal || sndVal then
      return True
    else do
      printConflictWith x y
      return False
  (Just a, Just b) -> if a /= b then return True
    else do
      fstVal <- checkBrackets (firstBr x) (firstBr y)
      sndVal <- checkBrackets (secondBr x) (secondBr y)
      if fstVal || sndVal then
        return True
      else do
        printConflictWith x y
        return False
  (_, _) -> do
    printConflictWith x y
    return False

-- | Check for the determined the transition by its brackets
checkBrackets :: Maybe Bracket -> Maybe Bracket -> IO Bool
checkBrackets (Just (R a)) (Just (R b)) = return $ a /= b
checkBrackets _          _              = return False

printConflictWith :: Transition -> Transition -> IO ()
printConflictWith x y = putStrLn $ "Conflict: " ++ showTrans x ++ " with " ++ showTrans y

-- =====================
-- | Sequence analysis |
-- =====================

{-data Path = Path
  { pathStates  :: [State]
  , firstStack  :: [Bracket]
  , secondStack :: [Bracket]
  } deriving (Eq, Show)

analyzeSequence :: String -> Lgraph -> [Path]
analyzeSequence [] _     = []
analyzeSequence (c:[]) g = filter (analyzeChar c) (transitions g)-}

-- ===========================
-- | Is the L-graph regular? |
-- ===========================

-- | Is the L-graph regular?
isLgraphRegular :: Lgraph -> IO Bool
isLgraphRegular g = andIO $ map hasRegularTransitions $ states g
  where
    hasRegularTransitions s =
      andIO $ map isTransitionRegular $ takeTransitionsWithState s $ transitions g

isTransitionRegular :: Transition -> IO Bool
isTransitionRegular t = do
  hasFirst  <- hasFirstBracket t
  hasSecond <- hasSecondBracket t
  return $ not (hasFirst || hasSecond)

hasFirstBracket :: Transition -> IO Bool
hasFirstBracket t
  | firstBr t == Nothing = return False
  | otherwise            = do
    putStrLn $ showTrans t ++ " has a first bracket"
    return True

hasSecondBracket :: Transition -> IO Bool
hasSecondBracket t
  | secondBr t == Nothing = return False
  | otherwise             = do
    putStrLn $ showTrans t ++ " has a second bracket"
    return True

-- ============================
-- | Is L-graph context free? |
-- ============================

-- | Is the L-graph context free?
isLgraphCF :: Lgraph -> IO Bool
isLgraphCF g = andIO $ map hasCFTransitions $ states g
  where
    hasCFTransitions s =
      andIO $ map isTransitionCF $ takeTransitionsWithState s $ transitions g

isTransitionCF :: Transition -> IO Bool
isTransitionCF t = do
  hasSecond <- hasSecondBracket t
  return $ not hasSecond
