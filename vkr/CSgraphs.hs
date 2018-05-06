import Data.List
import Data.Set (toList, fromList)

-- * L-graph structure
data Lgraph = Lgraph
  { states      :: [State]      -- ^ set of states
  , symbols     :: [Char]       -- ^ set of symbols
  , beginStates :: [State]      -- ^ set of begin states
  , finalStates :: [State]      -- ^ set of final states
  , transitions :: [Transition] -- ^ set of transitions
  } deriving (Eq)

instance Show Lgraph where
  show g = "L-graph: {" ++ 
    "\n  states: { "  ++ showSet (states g) ++ " }" ++
    "\n  symbols: { " ++ showSet (symbols g) ++ " }" ++
    "\n  beginStates: { " ++ showSet (beginStates g) ++ " }" ++
    "\n  finalStates: { " ++ showSet (finalStates g) ++ " }" ++
    "\n  transitions: " ++ show (transitions g) ++
    "\n}"

-- | Show a list like a set without brackets
showSet :: [Char] -> String
showSet []      = ""
showSet (c:[])  = [c]
showSet (c:str) = [c] ++ ", " ++ showSet str

data Bracket = L String | R String -- ^ Left/Right bracket with index
  deriving (Eq, Show)

type State = Char

-- * Transition structure
data Transition = Transition
  { oldState      :: State
  , symbol        :: Maybe Char
  , firstBracket  :: Maybe Bracket
  , secondBracket :: Maybe Bracket
  , newState      :: State
  } deriving (Eq)

instance Show Transition where
  show t = "\n    " ++ [oldState t] ++ " --{ " ++ stateMark ++ " }-> " ++ [newState t] ++ " "
    where
      stateMark  = mark ++ " : " ++ fstBr ++ " : " ++ sndBr
      mark  = case symbol t of
        Nothing    -> "_"
        Just x     -> [x]
      fstBr = case firstBracket t of
        Nothing     -> " _ "
        Just (L "") -> "(__"
        Just (L x)  -> "(_" ++ x
        Just (R "") -> ")__"
        Just (R x)  -> ")_" ++ x
      sndBr = case secondBracket t of
        Nothing     -> " _ "
        Just (L "") -> "[__"
        Just (L x)  -> "[_" ++ x
        Just (R "") -> "]__"
        Just (R x)  -> "]_" ++ x

-- ========================
-- | Is L-graph regular? |
-- ========================

-- | Is the L-graph regular?
isLgraphRegular :: Lgraph -> IO Bool
isLgraphRegular g = and <$> (sequence $ map hasRegularTransitions $ states g)
  where
    hasRegularTransitions s =
      and <$> (sequence $ map isTransitionRegular $ takeTransitionsWithState s $ transitions g)

-- | Take transitions which started from current state
takeTransitionsWithState :: State -> [Transition] -> [Transition]
takeTransitionsWithState s ts = filter (\t -> oldState t == s) ts

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
  | otherwise                 = do
    putStrLn $ show t ++ " has a first bracket"
    return True

-- | The transition has a second bracket
hasSecondBracket :: Transition -> IO Bool
hasSecondBracket t
  | secondBracket t == Nothing = return False
  | otherwise                  = do
    putStrLn $ show t ++ " has a second bracket"
    return True

-- ============================
-- | Is L-graph context free? |
-- ============================

-- | Is the L-graph context free?
isLgraphCF :: Lgraph -> IO Bool
isLgraphCF g = and <$> (sequence $ map hasCFTransitions $ states g)
  where
    hasCFTransitions s =
      and <$> (sequence $ map isTransitionCF $ takeTransitionsWithState s $ transitions g)

-- | The transition has not a second bracket
isTransitionCF :: Transition -> IO Bool
isTransitionCF t = do
  hasSecond <- hasSecondBracket t
  return $ not hasSecond

-- =================================
-- | Is L-graph context sensitive? |
-- =================================

-- | Is the L-graph context sensitive?
isLgraphCS :: Lgraph -> IO Bool
isLgraphCS g = do
  firstBracketLimit  <- and <$> (sequence $ map hasCSTransitionsByFirstBrackets  $ states g)
  secondBracketLimit <- and <$> (sequence $ map hasCSTransitionsBySecondBrackets $ states g)
  return (firstBracketLimit || secondBracketLimit)
  where
    hasCSTransitionsByFirstBrackets s =
      and <$> (sequence $ map isLimitedByFirstBracket  $ takeTransitionsWithState s $ transitions g)
    hasCSTransitionsBySecondBrackets s =
      and <$> (sequence $ map isLimitedBySecondBracket $ takeTransitionsWithState s $ transitions g)

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
  newGenStates <- return $ fromList $ concat $ (generatedByStates g) <$> xs
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
  newReachStates <- return $ fromList $ concat $ (reachedFromStates g) <$> xs
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

-- ==========================
-- | L-graphs concatenation |
-- ==========================

-- | Concat two L-graphs
lgraphsConcat :: Lgraph -> Lgraph -> Lgraph
lgraphsConcat g1 g2
  | states g1 `intersect` states g2 == [] = Lgraph
    { states      = states g1 `union` states g2
    , symbols     = symbols g1 `union` symbols g2
    , beginStates = beginStates g1
    , finalStates = finalStates g2
    , transitions = indexedTransitions `union` connections
    }
  | otherwise = lgraphsConcat g1 (renameLgraphStates g2 (states g1))
  where
    indexedTransitions = map (indexBy 1) (transitions g1) `union` map (indexBy 2) (transitions g2)
    connections = concat $ map (\t -> map (emptyTransition t) (beginStates g2)) (finalStates g1)

-- | Add index to both of brackets on the transition (if they exist).
indexBy :: Int -> Transition -> Transition
indexBy idx t = t
  { firstBracket = case firstBracket t of
    Nothing    -> Nothing
    Just (L x) -> Just (L (x ++ show idx))
    Just (R x) -> Just (R (x ++ show idx))
  , secondBracket = case secondBracket t of
    Nothing    -> Nothing
    Just (L x) -> Just (L (x ++ show idx))
    Just (R x) -> Just (R (x ++ show idx))
  }

-- ==================
-- | L-graphs union |
-- ==================

-- | Union of two L-graphs
lgraphsUnion :: Lgraph -> Lgraph -> Lgraph
lgraphsUnion g1 g2
  | states g1 `intersect` states g2 == [] = Lgraph
    { states      = states g1' `union` states g2'
    , symbols     = symbols g1 `union` symbols g2
    , beginStates = beginStates g1'
    , finalStates = finalStates g1'
    , transitions = transitions g1' `union` transitions g2'
    }
  | otherwise = lgraphsUnion g1 (renameLgraphStates g2 (states g1))
  where
    [beg, fin] = take 2 $
      filter (\v -> not (v `elem` (states g1 `union` states g2))) allStates
    g1' = Lgraph
      { states      = states g1 `union` [beg, fin]
      , symbols     = symbols g1
      , beginStates = [beg]
      , finalStates = [fin]
      , transitions = transitions g1 `union`
        (map (emptyTransition beg) $ beginStates g1) `union`
        (map (\v -> emptyTransition v fin) $ finalStates g1)
      }
    g2' = Lgraph
      { states      = states g2 `union` [beg, fin]
      , symbols     = symbols g2
      , beginStates = [beg]
      , finalStates = [fin]
      , transitions = transitions g2 `union`
        (map (emptyTransition beg) $ beginStates g2) `union`
        (map (\v -> emptyTransition v fin) $ finalStates g2)
      }

-- ====================
-- | CF-graph reverse |
-- ====================

-- | Reverse CF-graph
cfgraphReverse :: Lgraph -> IO (Maybe Lgraph)
cfgraphReverse g = do
  cond <- isLgraphCF g
  if cond
    then return $ Just g
      { beginStates = finalStates g
      , finalStates = beginStates g
      , transitions = map reverseTransition $ transitions g
      }
    else do
      putStrLn "\nL-graph is not context-free\n"
      return Nothing

-- | Reverse CF-graph transition
reverseTransition :: Transition -> Transition
reverseTransition t = Transition
  { oldState      = newState t
  , symbol        = symbol t
  , firstBracket  = case firstBracket t of
    Just (L x) -> Just (R x)
    Just (R x) -> Just (L x)
    Nothing    -> Nothing
  , secondBracket = Nothing
  , newState      = oldState t
  }

-- ==========================
-- | CF-graphs intersection |
-- ==========================

-- | Intersect two CF-graphs to CS-graph
cfgraphsIntersection :: Lgraph -> Lgraph -> IO (Maybe Lgraph)
cfgraphsIntersection g1 g2 = do
  cond1 <- isLgraphCF g1
  cond2 <- isLgraphCF g2
  case (cond1, cond2) of
    (False, False) -> do
      putStrLn "\nThe first and the second L-graphs are not context-free\n"
      return Nothing
    (True, False)  -> do
      putStrLn "\nThe second L-graph is not context-free\n"
      return Nothing
    (False, True)  -> do
      putStrLn "\nThe first L-graph is not context-free\n"
      return Nothing
    (True, True)   -> do
      Just g2R <- cfgraphReverse g2
      if states g1 `intersect` states g2 == []
        then return $ Just $ lgraphsConcat
          g1
            { transitions = map addLeftSndBracket $ transitions g1
            }
          g2R
            { transitions = map clearSymbol $ map addRightSndBracket $ transitions g2R
            }
        else cfgraphsIntersection g1 (renameLgraphStates g2 (states g1))

-- | Add left second bracket
addLeftSndBracket :: Transition -> Transition
addLeftSndBracket t = t
  { secondBracket = case symbol t of
    Nothing -> Nothing
    Just s  -> Just (L [s])
  }

-- | Add right second bracket
addRightSndBracket :: Transition -> Transition
addRightSndBracket t = t
  { secondBracket = case symbol t of
    Nothing -> Nothing
    Just s  -> Just (R [s])
  }

-- | Clear symbol on transition
clearSymbol :: Transition -> Transition
clearSymbol t = t
  { symbol = Nothing
  }

-- | Make a transition without symbols and brackets
emptyTransition :: State -> State -> Transition
emptyTransition p q = Transition
  { oldState      = p
  , symbol        = Nothing
  , firstBracket  = Nothing
  , secondBracket = Nothing
  , newState      = q
  }

-- | Rename L-graph states
renameLgraphStates :: Lgraph -> [State] -> Lgraph
renameLgraphStates g oldStates = g
  { states      = map getNewState (states g)
  , beginStates = map getNewState (beginStates g)
  , finalStates = map getNewState (finalStates g)
  , transitions = map renameTransitionStates (transitions g)
  }
  where
    newStates     = take (length oldStates) $
      filter (\v -> not (v `elem` oldStates)) allStates
    getNewState s = case lookup s (zip oldStates newStates) of
      Nothing -> s
      Just x  -> x
    renameTransitionStates t = t
      { oldState = getNewState (oldState t)
      , newState = getNewState (newState t)
      }

-- | All available L-graph states
allStates :: [State]
allStates = ['A' .. 'Z'] ++ ['0' .. '9'] 

-- =================
-- | Test L-graphs |
-- =================

test0 :: Lgraph
test0 = Lgraph
  { states      = ['1', '2', '3']
  , symbols     = ['a', 'b', 'c']
  , beginStates = ['1']
  , finalStates = ['3']
  , transitions = [l1, l2, l3, l4, l5]
  }
  where
    l1 = Transition
      { oldState      = '1'
      , symbol        = Just 'a'
      , firstBracket  = Nothing
      , secondBracket = Just (L "")
      , newState      = '1'
      }
    l2 = Transition
      { oldState      = '1'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Just (R "")
      , newState      = '2'
      }
    l3 = Transition
      { oldState      = '2'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Just (R "")
      , newState      = '2'
      }
    l4 = Transition
      { oldState      = '2'
      , symbol        = Just 'c'
      , firstBracket  = Just (R "")
      , secondBracket = Nothing
      , newState      = '3'
      }
    l5 = Transition
      { oldState      = '3'
      , symbol        = Just 'c'
      , firstBracket  = Just (R "")
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
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "1")
      , secondBracket = Just (L "")
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'B'
      }
    l5 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
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
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "1")
      , secondBracket = Just (L "")
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "1")
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
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Nothing
      , firstBracket  = Nothing
      , secondBracket = Just (L "")
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "1")
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
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R "")
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
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'C'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R "")
      , secondBracket = Nothing
      , newState      = 'B'
      }

test7 :: Lgraph
test7 = Lgraph
  { states      = ['E', 'F', 'C', 'D']
  , symbols     = ['a', 'b']
  , beginStates = ['E']
  , finalStates = ['F']
  , transitions = [l1, l2, l3, l4]
  }
  where
    l1 = Transition
      { oldState      = 'E'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'D'
      }
    l2 = Transition
      { oldState      = 'E'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'F'
      }
    l3 = Transition
      { oldState      = 'C'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Nothing
      , newState      = 'D'
      }
    l4 = Transition
      { oldState      = 'F'
      , symbol        = Nothing
      , firstBracket  = Just (R "")
      , secondBracket = Nothing
      , newState      = 'F'
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
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "1")
      , secondBracket = Just (L "")
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'B'
      }
    l5 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (R "")
      , secondBracket = Just (L "")
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
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
      , newState      = 'A'
      }
    l2 = Transition
      { oldState      = 'A'
      , symbol        = Just 'b'
      , firstBracket  = Just (L "")
      , secondBracket = Just (L "")
      , newState      = 'B'
      }
    l3 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'A'
      }
    l4 = Transition
      { oldState      = 'B'
      , symbol        = Just 'a'
      , firstBracket  = Just (R "")
      , secondBracket = Just (R "")
      , newState      = 'B'
      }
    l5 = Transition
      { oldState      = 'B'
      , symbol        = Nothing
      , firstBracket  = Just (L "")
      , secondBracket = Just (R "")
      , newState      = 'B'
      }
