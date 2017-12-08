import Data.List
import Data.Set (toList, fromList)

data Lgraph = Lgraph
  { states      :: [State]
  , symbols     :: [Char]
  , beginStates :: [State]
  , finalStates :: [State]
  , transitions :: [Transition]
  } deriving (Eq)

instance Show Lgraph where
  show g = "L-graph: {" ++ 
    "\n  states: {"  ++ showSet (states g) ++ "}" ++
    "\n  symbols: {" ++ showSet (symbols g) ++ "}" ++
    "\n  beginStates: {" ++ showSet (beginStates g) ++ "}" ++
    "\n  finalStates: {" ++ showSet (finalStates g) ++ "}" ++
    "\n  transitions: " ++ show (transitions g) ++
    "\n}"

-- | Show a list like a set without brackets
showSet :: [Char] -> String
showSet []      = ""
showSet (c:[])  = [c]
showSet (c:str) = [c] ++ ", " ++ showSet str

data Bracket = L Int | R Int -- ^ Left/Right bracket with index
  deriving (Eq, Show)

type State = Char

data Transition = Transition
  { oldState      :: State
  , symbol        :: Maybe Char
  , firstBracket  :: Maybe Bracket
  , secondBracket :: Maybe Bracket
  , newState      :: State
  } deriving (Eq)

instance Show Transition where
  show t = "\n    " ++ [oldState t] ++ " --{ " ++ stateMark ++ " }-> " ++ [newState t]
    where
      stateMark  = mark ++ " : " ++ fstBr ++ " : " ++ sndBr
      mark  = case symbol t of
        Nothing    -> "e"
        Just x     -> [x]
      fstBr = case firstBracket t of
        Nothing    -> "e"
        Just (L x) -> "(_" ++ show x
        Just (R x) -> ")_" ++ show x
      sndBr = case secondBracket t of
        Nothing    -> "e"
        Just (L x) -> "[_" ++ show x
        Just (R x) -> "]_" ++ show x

-- =================
-- | Test L-graphs |
-- =================

testGraph :: Lgraph
testGraph = Lgraph
  { states      = ['1', '2', '3']
  , symbols     = ['a', 'b', 'c']
  , beginStates = ['1']
  , finalStates = ['3']
  , transitions  = [l1, l2, l3, l4, l5]
  }
  where
    l1 = Transition
      { oldState      = '1'
      , symbol        = Just 'a'
      , firstBracket  = Nothing
      , secondBracket = Just (L 1)
      , newState      = '1'
      }
    l2 = Transition
      { oldState      = '1'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Just (R 1)
      , newState      = '2'
      }
    l3 = Transition
      { oldState      = '2'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Just (R 1)
      , newState      = '2'
      }
    l4 = Transition
      { oldState      = '2'
      , symbol        = Just 'c'
      , firstBracket  = Just (R 1)
      , secondBracket = Nothing
      , newState      = '3'
      }
    l5 = Transition
      { oldState      = '3'
      , symbol        = Just 'c'
      , firstBracket  = Just (R 1)
      , secondBracket = Nothing
      , newState      = '3'
      }

test1 :: Lgraph
test1 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4, l5]
  }
  where
    l1 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Just (L 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 2)
      , secondBracket = Just (L 1)
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'B'
      }
    l5 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (L 1)
      , secondBracket = Just (L 1)
      , newState      = 'B'
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
      { oldState      = 'A'
      , symbol        = Just 'a'
      , firstBracket  = Just (L 1)
      , secondBracket = Just (L 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 2)
      , secondBracket = Just (L 1)
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 2)
      , newState      = 'B'
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
      { oldState      = 'A'
      , symbol        = Just 'a'
      , firstBracket  = Nothing
      , secondBracket = Nothing
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Nothing
      , firstBracket  = Nothing
      , secondBracket = Nothing
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Nothing
      , secondBracket = Nothing
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Nothing
      , secondBracket = Nothing
      , newState      = 'B'
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
      { oldState      = 'A'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Nothing
      , firstBracket  = Nothing
      , secondBracket = Just (L 1)
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (L 1)
      , secondBracket = Just (L 1)
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 2)
      , secondBracket = Nothing
      , newState      = 'B'
      }

test5 :: Lgraph
test5 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState      = 'A'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R 1)
      , secondBracket = Nothing
      , newState      = 'B'
      }

test6 :: Lgraph
test6 = Lgraph
  { states      = ['A', 'B', 'C']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState      = 'A'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'C'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R 1)
      , secondBracket = Nothing
      , newState      = 'B'
      }

test7 :: Lgraph
test7 = Lgraph
  { states      = ['A', 'B', 'C', 'D']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'D'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'C'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Nothing
      , newState      = 'D'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R 1)
      , secondBracket = Nothing
      , newState      = 'B'
      }

test8 :: Lgraph
test8 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4, l5]
  }
  where
    l1 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Just (L 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 2)
      , secondBracket = Just (L 1)
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'B'
      }
    l5 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R 1)
      , secondBracket = Just (L 1)
      , newState      = 'B'
      }

test9 :: Lgraph
test9 = Lgraph
  { states      = ['A', 'B']
  , symbols     = ['a', 'b']
  , beginStates = ['A']
  , finalStates = ['B']
  , transitions = [l1, l2, l3, l4, l5]
  }
  where
    l1 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 1)
      , secondBracket = Just (L 1)
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L 2)
      , secondBracket = Just (L 1)
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R 1)
      , secondBracket = Just (R 1)
      , newState      = 'B'
      }
    l5 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (L 1)
      , secondBracket = Just (R 1)
      , newState      = 'B'
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

-- | Logical 'and' for a list in IO monad
andIO :: [IO Bool] -> IO Bool
andIO [] = return True
andIO (x:xs) = do
  val  <- x
  vals <- andIO xs
  return $ val && vals

-- | Logical 'or' for a list in IO monad
orIO :: [IO Bool] -> IO Bool
orIO [] = return False
orIO (x:xs) = do
  val  <- x
  vals <- orIO xs
  return $ val || vals

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

-- | Check for the determined transition from one state for two transitions
simpleTrans :: (Transition, Transition) -> IO Bool
simpleTrans (x, y) = case (symbol x, symbol y) of
  (Nothing, Nothing) -> do
    fstVal <- checkBrackets (firstBracket x) (firstBracket y)
    sndVal <- checkBrackets (secondBracket x) (secondBracket y)
    if fstVal || sndVal then
      return True
    else do
      printConflictWith x y
      return False
  (Just a, Just b) -> if a /= b then return True
    else do
      fstVal <- checkBrackets (firstBracket x) (firstBracket y)
      sndVal <- checkBrackets (secondBracket x) (secondBracket y)
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

-- | Nice output for conflict of transitions
printConflictWith :: Transition -> Transition -> IO ()
printConflictWith x y = putStrLn $ "Conflict: " ++ show x ++ "\n    with" ++ show y

-- ===========================
-- | Is the L-graph regular? |
-- ===========================

-- | Is the L-graph regular?
isLgraphRegular :: Lgraph -> IO Bool
isLgraphRegular g = andIO $ map hasRegularTransitions $ states g
  where
    hasRegularTransitions s =
      andIO $ map isTransitionRegular $ takeTransitionsWithState s $ transitions g

-- | The transition has not any brackets
isTransitionRegular :: Transition -> IO Bool
isTransitionRegular t = do
  hasFirst  <- hasFirstBracket t
  hasSecond <- hasSecondBracket t
  return $ not (hasFirst || hasSecond)

-- | The transition has a first bracket
hasFirstBracket :: Transition -> IO Bool
hasFirstBracket t
  | firstBracket t == Nothing = return False
  | otherwise            = do
    putStrLn $ show t ++ " has a first bracket"
    return True

-- | The transition has a second bracket
hasSecondBracket :: Transition -> IO Bool
hasSecondBracket t
  | secondBracket t == Nothing = return False
  | otherwise             = do
    putStrLn $ show t ++ " has a second bracket"
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

-- | The transition has not a second bracket
isTransitionCF :: Transition -> IO Bool
isTransitionCF t = do
  hasSecond <- hasSecondBracket t
  return $ not hasSecond

-- ============================
-- | Is L-graph context free? |
-- ============================

-- | Is the L-graph context sensitive?
isLgraphCS :: Lgraph -> IO Bool
isLgraphCS g = do
  firstBracketLimit  <- andIO $ map hasCSTransitionsByFirstBrackets $ states g
  secondBracketLimit <- andIO $ map hasCSTransitionsBySecondBrackets $ states g
  return (firstBracketLimit || secondBracketLimit)
  where
    hasCSTransitionsByFirstBrackets s =
      andIO $ map isLimitedByFirstBracket $ takeTransitionsWithState s $ transitions g
    hasCSTransitionsBySecondBrackets s =
      andIO $ map isLimitedBySecondBracket $ takeTransitionsWithState s $ transitions g

-- | The transition with left first bracket necessarily have a symbol
isLimitedByFirstBracket :: Transition -> IO Bool
isLimitedByFirstBracket t = do
  case firstBracket t of
    Nothing    -> return True
    Just (R _) -> return True
    Just (L _) -> case symbol t of
      Nothing -> return False
      Just _  -> return True

-- | The transition with left second bracket necessarily have a symbol
isLimitedBySecondBracket :: Transition -> IO Bool
isLimitedBySecondBracket t = do
  case secondBracket t of
    Nothing -> return True
    Just (R _) -> return True
    Just (L _) -> case symbol t of
      Nothing -> return False
      Just _  -> return True

-- =========================
-- | Delete useless states |
-- =========================

-- | Delete useless states and transitions with these states from L-graph
deleteUselessStates :: Lgraph -> IO Lgraph
deleteUselessStates g = do
  g1 <- deleteNonGeneratingStates g
  g2 <- deleteUnreachableStates g1
  return g2

-- | Delete non generating states and transitions with these states from L-graph
deleteNonGeneratingStates :: Lgraph -> IO Lgraph
deleteNonGeneratingStates g = do
  genStates      <- generatingStates g (finalStates g)
  genTransitions <- transitionsWithStates g genStates
  return g
    { states      = genStates
    , transitions = genTransitions
    }

-- | Delete unreacheble states and transitions with these states from L-graph
deleteUnreachableStates :: Lgraph -> IO Lgraph
deleteUnreachableStates g = do
  reachStates      <- reachebleStates g (beginStates g)
  reachTransitions <- transitionsWithStates g reachStates
  return g
    { states      = reachStates
    , transitions = reachTransitions
    }

-- | Get only generating states from L-graph
generatingStates :: Lgraph -> [State] -> IO [State]
generatingStates g xs = do
  oldGenStates <- return $ fromList xs
  newGenStates <- return $ fromList $ concat $ map (generatedByStates g) xs
  if oldGenStates == newGenStates
    then return (toList oldGenStates)
    else generatingStates g (toList newGenStates)

-- | Get states generating the input state
generatedByStates :: Lgraph -> State -> [State]
generatedByStates g st =
  map oldState $ filter (\t -> newState t == st) (transitions g)

-- | Get only reachable states from L-graph
reachebleStates :: Lgraph -> [State] -> IO [State]
reachebleStates g xs = do
  oldReachStates <- return $ fromList xs
  newReachStates <- return $ fromList $ concat $ map (reachedFromStates g) xs
  if oldReachStates == newReachStates
    then return (toList oldReachStates)
    else reachebleStates g (toList newReachStates)

-- | Get states reached from input state
reachedFromStates :: Lgraph -> State -> [State]
reachedFromStates g st = [st] ++
  (map newState $ filter (\t -> oldState t == st) (transitions g))

-- | Get L-graph transitions filtering initiated by input states
transitionsWithStates :: Lgraph -> [State] -> IO [Transition]
transitionsWithStates g xs = return $
  filter (\t -> newState t `elem` xs && oldState t `elem` xs ) (transitions g)

-- =====================
-- | Sequence analyzer |
-- =====================

data Analyzer = Analyzer
  { lgraph       :: Lgraph
  , currentState :: State
  , path         :: [State]
  , firstStack   :: [Bracket]
  , secondStack  :: [Bracket]
  } deriving (Eq, Show)

-- | Print current path of analyzing sequence on L-graph
printPath :: Analyzer -> IO ()
printPath analyzer = putStrLn $ showPath $ path analyzer

-- | Nice output for path :)
showPath :: [Char] -> String
showPath [x] = "(" ++ [x] ++ ")"
showPath (x:xs) = "(" ++ [x] ++ ") -> " ++ showPath xs

-- | Analyze sequence by all (determinde) analyzers
analyzeSequenceByAnalyzers :: [Char] -> [Analyzer] -> IO Bool
analyzeSequenceByAnalyzers sequence as =
  orIO $ map (analyzeSequenceByAnalyzer sequence) as

-- | Analyze sequence by (determined) analyzer
analyzeSequenceByAnalyzer :: [Char] -> Analyzer -> IO Bool
analyzeSequenceByAnalyzer [] analyzer
  | currentState analyzer `elem` (finalStates.lgraph) analyzer
    && firstStack analyzer == [] && secondStack analyzer == []
    = do
      printPath analyzer
      return True
  | firstStack analyzer == [] && secondStack analyzer == [] = return False
  | otherwise = do
    (res,updatedAnalyzer) <- analyzeBrackets analyzer
    if res
      then analyzeSequenceByAnalyzer [] updatedAnalyzer
      else return False
analyzeSequenceByAnalyzer (c:sequence) analyzer = do
  (res1,updatedAnalyzer1) <- analyzeSymbol analyzer c
  if res1
    then analyzeSequenceByAnalyzer sequence updatedAnalyzer1
    else do
      (res2,updatedAnalyzer2) <- analyzeBrackets analyzer
      if res2
        then analyzeSequenceByAnalyzer (c:sequence) updatedAnalyzer2
        else return False

-- | Analyze one symbol with brackets by (determined) L-graph and return new state of analyzer
analyzeSymbol :: Analyzer -> Char -> IO (Bool, Analyzer)
analyzeSymbol analyzer c = do
  likelyTransitions <- return $ filter
    (transitionFromStateWithSymbol (currentState analyzer) c)
    (transitions $ lgraph analyzer)
  case (findTransitionNeededFor analyzer) likelyTransitions of
    Nothing -> return (False, analyzer)
    Just t  -> return (True, analyzer
      { currentState = newState t
      , path         = path analyzer ++ [newState t]
      , firstStack   = updateStackWithBracket (firstStack analyzer) (firstBracket t)
      , secondStack  = updateStackWithBracket (secondStack analyzer) (secondBracket t)
      })

-- | Analyze only brackets without symbol by (determined) L-graph and return new state of analyzer
analyzeBrackets :: Analyzer -> IO (Bool, Analyzer)
analyzeBrackets analyzer = do
  likelyTransitions <- return $ filter
    (transitionFromStateWithoutSymbol (currentState analyzer))
    (transitions $ lgraph analyzer)
  case (findTransitionNeededFor analyzer) likelyTransitions of
    Nothing -> return (False, analyzer)
    Just t  -> return (True, analyzer
      { currentState = newState t
      , path         = path analyzer ++ [newState t]
      , firstStack   = updateStackWithBracket (firstStack analyzer) (firstBracket t)
      , secondStack  = updateStackWithBracket (secondStack analyzer) (secondBracket t)
      })

-- | Check transition by start state and symbol mark
transitionFromStateWithSymbol :: State -> Char -> Transition -> Bool
transitionFromStateWithSymbol state c t =
  (oldState t) == state && (symbol t) == (Just c)

-- | Check transition by start state and abscence of symbol
transitionFromStateWithoutSymbol :: State -> Transition -> Bool
transitionFromStateWithoutSymbol state t =
  (oldState t) == state && (symbol t) == Nothing

-- | Find correct for analyzer transition
findTransitionNeededFor :: Analyzer -> [Transition] -> Maybe Transition
findTransitionNeededFor analyzer ts =
  find (checkStacksWithTransition analyzer) ts

-- | Check transition brackets for analyzer
checkStacksWithTransition ::  Analyzer -> Transition -> Bool
checkStacksWithTransition analyzer t =
  checkFirstStack analyzer t && checkSecondStack analyzer t

-- | Check first transition brackets for analyzer
checkFirstStack :: Analyzer -> Transition -> Bool
checkFirstStack analyzer t = case firstBracket t of
  Nothing    -> True
  Just (L _) -> True
  Just (R i) -> case firstStack analyzer of
    (L i):_ -> True
    _       -> False

-- | Check second transition brackets for analyzer
checkSecondStack :: Analyzer -> Transition -> Bool
checkSecondStack analyzer t = case secondBracket t of
  Nothing    -> True
  Just (L _) -> True
  Just (R i) -> case secondStack analyzer of
    (L i):_ -> True
    _       -> False

-- | Update analyzer stack by bracket mark
updateStackWithBracket :: [Bracket] -> Maybe Bracket -> [Bracket]
updateStackWithBracket [] bracket = case bracket of
  Nothing    -> []
  Just (L i) -> [L i]
updateStackWithBracket ((L x):xs) bracket = case bracket of
  Nothing -> ((L x):xs)
  Just (L i) -> ((L i):(L x):xs)
  Just (R x) -> xs

-- | Make analyzers from all of beginning states
mkAnalyzers :: Lgraph -> IO [Analyzer]
mkAnalyzers graph = return $ map (mkAnalyzer graph) (beginStates graph)

-- | Make analyzer from one of the beginning states
mkAnalyzer :: Lgraph -> State -> Analyzer
mkAnalyzer graph state = Analyzer
  { lgraph       = graph
  , currentState = state
  , path         = [state]
  , firstStack   = []
  , secondStack  = []
  }

-- | Analyze sequence by L-graph
analyzeSequenceByDeterminedLgraph :: Lgraph -> [Char] -> IO Bool
analyzeSequenceByDeterminedLgraph graph sequence = do
  isDetermined <- isLgraphDetermined graph
  if isDetermined then do
    as <- mkAnalyzers graph
    analyzeSequenceByAnalyzers sequence as
  else do
    putStrLn "Error: L-graph is not determined!"
    return False
