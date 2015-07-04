-- algorithm description: 
-- Set of variables (x) - part of finite set
-- Set of clauses (phi)
-- Trying to find values of x such that each phi is satisfied
-- Each process runs in parallel for every variable.
-- Maintain a probability distribution for the variable
-- Update it based on whether or not constraints are satisfied
module DCFL where
import System.Random

data Probability = Probability Double deriving Show
data Distribution = Distribution {probab::[Double]} deriving Show
data Values = Values [Integer] deriving Show
data Variable = Variable {possible::[Int], valueIndex::Int, 
  distr::Distribution} deriving (Show)
--instance Show Variable where
--  show (Variable possible valueIndex _) = show $ possible !! valueIndex
-- Variable indices describe 
data ConstraintEl = ConstraintEl {variableIndices :: [Int],
  constraint :: ([Int] -> Bool)}
data Solved = Solved {variables :: [Variable], iterationCount :: Int}

instance Show ConstraintEl where
  show (ConstraintEl variableIndices _) = 
    "Constraint " ++ (show variableIndices)

width (Distribution p) = fromIntegral $ length p

-- constants
b = 0.1 :: Double

oneIfEqual x val
  | val == x = 1
  | otherwise = 0

replicateDouble :: Int -> Double -> [Double]
replicateDouble a f
  | a == 0 = []
  | otherwise = (f :) $ replicate (a - 1) f

initDistribution width = Distribution $ 
  replicateDouble width (1.0/(fromIntegral width))

failureCurrProb width currValue = (1.0-b)*currValue
failureOtherProb width currValue = ((1.0-b)*currValue) + (b/(width-1.0))

failureProb width valueIndex currValue currIndex
  | valueIndex == currIndex = failureCurrProb width currValue
  | otherwise = failureOtherProb width currValue

-- given a distribution, update it based on the value of success
-- if successful, then set to 1, 0
-- otherwise, udate it with failureProb
updateProb dist@(Distribution p) valueIndex success
  -- if successful, we update the distribution
  | success = Distribution $ map (\x -> oneIfEqual (snd x) valueIndex) $ zip p [0..]
  | otherwise = Distribution $ map (\x -> 
    failureProb (width dist) valueIndex (fst x) (snd x)) $ zip p [0..]

updateVariableProb (Variable possib valIndex dist) success = 
  Variable possib valIndex $ updateProb dist valIndex success

cummDistributionIter dist@(Distribution p) ind curr
  | ind == length p = []
  | otherwise = newCurr : (cummDistributionIter dist (ind + 1) (newCurr)) where
    newCurr = curr + (p !! ind)

cummDistribution dist@(Distribution p) = Distribution $ cummDistributionIter dist 0 0

getValueIndex (Distribution p) randValue = 
  length $ takeWhile (\x -> randValue > (fst x)) $ zip p [0..]

randomNum :: IO Double
randomNum = do
  x <- getStdRandom (randomR (0.0, 1.0))
  return x

randomizeVariable var@(Variable p v dist) = do
  randVal <- randomNum
  let newValIndex = getValueIndex (cummDistribution dist) randVal in
    return $ Variable p newValIndex dist

evalConstraint constraint values = constraint values
evalConstraints constraints values = 
  foldr (&&) True $ map (\c -> evalConstraint c values) constraints

-- apply a function at only one index of a list
applyAt f index list = 
  map (\x -> if (snd x) == index then f (fst x)
                                 else (fst x)) $ zip list [0..]

-- get the constraints associated with a certain variable index
getConstraintsFor :: Int -> [ConstraintEl] -> [[Int] -> Bool]
getConstraintsFor n constraintSet = 
  [constraint | ConstraintEl [a, b] constraint <- constraintSet, ((a == n) || (b == n))]

-- unwrapping utility
getConstraintFromEl (ConstraintEl _ constraint) = constraint

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

-- checks if every probability in the distribution is either 0 or 1
checkDistrSolved (Distribution probab) = all (\x -> x == 0.0 || x == 1.0) probab

-- check if the constraints have been solved by looking at the distributions
-- of each 
checkSolved [] = True
checkSolved (var:vars)
  | checkDistrSolved $ distr var = checkSolved vars
  | otherwise = False



