import DCFL

-- creates the set of variables; n colors possible
colorVariables n = 
  map (\x -> Variable [0..n-1] 0 (initDistribution n)) $ [0..n-1]

-- constraint function for two different nodes in the network
-- specifies that their colors should not be equal
colorConstraint :: Int -> Int -> [Int] -> Bool
colorConstraint node1 node2 values = (values !! node1) /= (values !! node2)

-- this is a constraint that forces one of the nodes to have a color value of 4
nonsenseConstraint node1 node2 values = (values !! node1) == 4

-- construct the constraint set by currying
colorConstraints :: Int -> [ConstraintEl]
colorConstraints n = 
  [ConstraintEl a b $ colorConstraint a b | a <- [0..n-1], b <- [0..n-1], a /= b]

-- get the constraints associated with a certain variable index
getConstraintsFor :: Int -> [ConstraintEl] -> [[Int] -> Bool]
getConstraintsFor n constraintSet = 
  [constraint | ConstraintEl a b constraint <- constraintSet, ((a == n) || (b == n))]

-- unwrapping utility
getConstraintFromEl (ConstraintEl _ _ constraint) = constraint

-- get the constraint functions out of a list of ConstraintEls
justConstraints = map getConstraintFromEl

-- get values of the variable set
getValues variables = map (\(Variable _ val _) -> val) variables

-- randomizes the variable value of a single variable in a list of variables
randomizeSingle::Int -> [Variable] -> [IO Variable]
randomizeSingle variableIndex variables = 
  map (\x -> if (snd x) == variableIndex then randomizeVariable $ fst x
                                         else return $ (fst x)) $ zip variables [0..]

-- randomize all the variables in a list
randomize variables = map randomizeVariable variables

printVariables variables = do
  map (putStrLn . show) variables

-- either randomize or let a variable stay, depending on what the constraint
-- check tells us
update :: Int -> [Variable] -> [ConstraintEl] -> IO [Variable]
update variableIndex variables constraintSet = do
  rvariables <- sequence $ randomizeSingle variableIndex variables
  let values = getValues rvariables
      constraints = getConstraintsFor variableIndex constraintSet 
      constraintRes = evalConstraints constraints values

      -- update the variable probability based on the value of constraintRes
      appliedVars = applyAt (\var -> updateVariableProb var constraintRes) 
        variableIndex rvariables in
      return appliedVars

-- update each variable in the indices list once
updateEach' :: [Variable] -> [ConstraintEl] -> [Int] -> IO [Variable]
updateEach' variables constraintSet (i:indices)
  | length indices > 0 = do
    vars <- update i variables constraintSet
    updateEach' vars constraintSet indices
  | otherwise  = do
    return variables

-- update each variable in the variable set based on the constraint set
-- value
updateEach :: [Variable] -> [ConstraintEl] -> IO [Variable]
updateEach variables constraintSet = 
  updateEach' variables constraintSet [0 .. (length variables)]

-- update the variable set n number of times
updateEachTimes :: [Variable] -> [ConstraintEl] -> Int -> IO [Variable]
updateEachTimes variables constraintSet n
  | n > 0 = do
    rvars <- updateEach variables constraintSet
    updateEachTimes rvars constraintSet (n - 1)
  | otherwise = return variables

main = do
  rvars <- updateEachTimes vars consts 200
  putStrLn $ show consts
  putStrLn $ show rvars
  putStrLn $ show $ evalConstraint (constraint $ consts !! 0) (getValues rvars)
  putStrLn $ show $ getValues rvars 
    where vars = colorVariables 10
          consts = colorConstraints 10

