import DCSgraph

determineLgraph :: Lgraph -> Lgraph
determineLgraph g = g
  { transitions = determineAllTransitions g
  }

determineAllTransitions :: Lgraph -> [Line]
determineAllTransitions g =
  concat $ map determineTransitionsFromState (transitions g) $ states g

determineTransitionsFromState :: [Line] -> State -> [Line]
determineTransitionsFromState ts s =
  determineTransitions $ filter (\t -> oldState t == s) ts

determineTransitions :: [Line] -> [Line]
determineTransitions []     = []
determineTransitions (t:ts) =
  case find (\x -> (not . simpleTrans) (x,t)) of
    Nothing -> t:ts
    Just y  -> determineTransitions $ newTransition t y : delete y ts

newTransition :: Line -> Line -> Line
newTransition x y
  | x == y    = x
  | otherwise = x
