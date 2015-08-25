-- algorithm description: 
-- Set of variables (x) - part of finite set
-- Set of clauses (phi)
-- Trying to find values of x such that each phi is satisfied
-- Each process runs in parallel for every variable.
-- Maintain a probability distribution for the variable
-- Update it based on whether or not constraints are satisfied
module Data.DCFL (
  Distribution(..),
  Values(..),
  Variable(..),
  ConstraintEl(..),
  Solved(..),

  -- * Distributions
  initDistribution,
  cummDistribution,
  checkSolved,

  -- * Variables
  randomizeSingle,
  randomize,
  printVariables,

  -- * Constraints
  getConstraintsFor,
  justConstraints,

  -- * Solving
  -- ** Serial/Single Threaded
  solve,
  update,
  updateEach,
  updateEachTimes,
  getValues,
  
  -- ** Parallelized
  solveParallel,
  updateEachTimesParallel,
  updateEachParallel
) where
import System.Random
import Control.Parallel.Strategies
import Control.DeepSeq

-- |Probability distribution; generally associated with a 'Variable'.
data Distribution = Distribution {probab::[Double]} deriving Show

instance NFData Distribution where
  rnf (Distribution p) = rnf p

-- |The integer values a 'Variable' can take on.
data Values = Values [Integer] deriving Show

-- |Each 'Variable' has a finite set of possible values, a value it holds
-- and a probability distribution over the set of possible values.
data Variable = Variable {possible::[Int], valueIndex::Int, 
  distr::Distribution} deriving (Show)

instance NFData Variable where
  rnf (Variable p v d) = rnf (p, v, d)

-- |Each constraint function ([Int] -> Bool) is associated with a certain set of
-- variables. 'ConstraintEl' represents this relationship for a given constraint
-- function.
data ConstraintEl = ConstraintEl {variableIndices :: [Int],
  constraint :: [Int] -> Bool}

-- |Return value of 'solve'.
data Solved = Solved {variables :: [Variable], iterationCount :: Int}

instance Show ConstraintEl where
  show (ConstraintEl v _) = 
    "Constraint " ++ show v

-- |Returns the number of finite values that a `Distribution` is over.
width :: Distribution -> Int
width (Distribution p) = fromIntegral $ length p

-- |Constant, as defined in the research paper "Decentralized Constraint Satisfaction"
--  Duffy, et al.
b :: Double
b = 0.1 

-- |Internally called function.
oneIfEqual :: (Eq a) => a -> a -> Int
oneIfEqual x val
  | val == x = 1
  | otherwise = 0

replicateDouble :: Int -> Double -> [Double]
replicateDouble a f
  | a == 0 = []
  | otherwise = (f :) $ replicate (a - 1) f

-- |Initialize a distribution with each possible value having the same probability.
-- For example, initDistribution 5 gives 
-- @
--  'Distribution' [0.2, 0.2, 0.2, 0.2, 0.2].
-- @
initDistribution :: Int -> Distribution
initDistribution w = Distribution $ 
  replicateDouble w (1.0/fromIntegral w)

-- |Adjust probability for the value which has just failed a constraint.
failureCurrProb :: Int -> Double -> Double
failureCurrProb _ currValue = (1.0-b)*currValue

-- |Adjust probability for values other than the one that just failed a constraint.
failureOtherProb :: Int -> Double -> Double 
failureOtherProb w currValue = ((1.0-b)*currValue) + (b/(fromIntegral w - 1.0))

-- |Adjust probability of taking on a value for a certain 'Variable' given that
-- a constraint was just failed.
failureProb :: Int -> Int -> Double -> Int -> Double
failureProb w v currValue currIndex
  | v == currIndex = failureCurrProb w currValue
  | otherwise = failureOtherProb w currValue

-- |Given a distribution, update it based on the value of success. 
-- If successful, then set the probability of the current value to 1.0 and the
-- probability for every other value to 0.0. 
-- Otherwise, update it with failureProb.
updateProb :: Distribution -> Int -> Bool -> Distribution
updateProb dist@(Distribution p) v success
  -- if successful, we update the distribution
  | success = Distribution $ 
    map (\x -> fromIntegral $ oneIfEqual (snd x) v) $ zip p [0..]
  | otherwise = Distribution $ zipWith (failureProb (width dist) v) p [0..]

-- |Same as 'updateProb', but rather than returning a 'Distribution', this function
-- returns a 'Variable'.
updateVariableProb :: Variable -> Bool -> Variable
updateVariableProb (Variable possib valIndex dist) success = 
  Variable possib valIndex $ updateProb dist valIndex success

-- |Internal iteration function used by 'cummDistribution'.
cummDistributionIter :: Distribution -> Int -> Double -> [Double]
cummDistributionIter dist@(Distribution p) ind curr
  | ind == length p = []
  | otherwise = newCurr : cummDistributionIter dist (ind + 1) newCurr where
    newCurr = curr + (p !! ind)

-- |Creates a cummulative 'Distribution' out of a given 'Distribution'.
cummDistribution :: Distribution -> Distribution
cummDistribution dist@(Distribution _) = Distribution $ cummDistributionIter dist 0 0

-- |Given a cummulative 'Distribution', this function returns the where a random
-- value should be "placed" within the 'Distribution'.
getValueIndex :: Distribution -> Double -> Int
getValueIndex (Distribution p) randValue = 
  length $ takeWhile (\x -> randValue > fst x) $ zip p [0 :: Int ..]

-- |Returns a single random number between 0.0 and 1.0.
randomNum :: IO Double
randomNum = getStdRandom (randomR (0.0, 1.0))

-- |Randomize the value of a 'Variable'.
randomizeVariable :: Variable -> IO Variable
randomizeVariable (Variable p _ dist) = do
  randVal <- randomNum
  let newValIndex = getValueIndex (cummDistribution dist) randVal in
    return $ Variable p newValIndex dist

-- |Evaluate one 'constraint' with a list of 'values'.
evalConstraint :: ([Int] -> Bool) -> [Int] -> Bool
evalConstraint c = c 

-- |Evaluate the set constraint functions 'constraints' with a list of 'values'.
evalConstraints :: [[Int] -> Bool] -> [Int] -> Bool
evalConstraints constraints values = 
  all (`evalConstraint` values) constraints

-- |Apply a function at only one index of a list. Internal function.
applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt f index list = 
  map (\x -> if snd x == index then f (fst x)
                                 else fst x) $ zip list [0..]

-- | Get the 'Constraint's associated with a 'Variable' of index 'n' in the list
-- of 'Variable's.
getConstraintsFor :: Int -> [ConstraintEl] -> [[Int] -> Bool]
getConstraintsFor n constraintSet = 
  [constraint | ConstraintEl [c, d] constraint <- constraintSet, (c == n) || (d == n)]

-- |Get the constraint functions out of a list of 'ConstraintEl's.
justConstraints :: [ConstraintEl] -> [[Int] -> Bool]
justConstraints = map constraint

-- |Get a list of values from a list of 'Variable's.
getValues :: [Variable] ->[Int]
getValues = map (\(Variable _ val _) -> val)

-- |Randomizes the value of a single 'Variable' in a list of 'Variable'.
randomizeSingle::Int -> [Variable] -> [IO Variable]
randomizeSingle variableIndex vs = 
  map (\x -> if snd x == variableIndex then randomizeVariable $ fst x
                                         else return (fst x)) $ zip vs [0..]

-- | Randomize all the variables in a list.
randomize :: [Variable] -> [IO Variable]
randomize = map randomizeVariable 

-- |Print variables.
printVariables :: [Variable] -> [IO ()]
printVariables = map print

-- |Either randomize or let a variable stay, depending on what the constraint
-- check tells us.
update :: Int -> [Variable] -> [ConstraintEl] -> IO [Variable]
update variableIndex vs constraintSet = do
  rvariables <- sequence $ randomizeSingle variableIndex vs
  let values = getValues rvariables
      constraints = getConstraintsFor variableIndex constraintSet 
      constraintRes = evalConstraints constraints values

      -- update the variable probability based on the value of constraintRes
      appliedVars = applyAt (`updateVariableProb` constraintRes) 
        variableIndex rvariables in
      return appliedVars

-- | Update each variable in the indices list once. Internal function used
-- by updateEach.
updateEach' :: [Variable] -> [ConstraintEl] -> [Int] -> IO [Variable]
-- what does this do with an empty list?
updateEach' vs constraintSet (i:indices)
  | not (null indices) = do
    vars <- update i vs constraintSet
    updateEach' vars constraintSet indices
  | otherwise  = return vs

-- |Update each variable in the variable set based on the constraint set
-- value.
updateEach :: [Variable] -> [ConstraintEl] -> IO [Variable]
updateEach vs constraintSet = 
  updateEach' vs constraintSet [0 .. (length vs)]

-- |Update the variable set 'n' number of times.
updateEachTimes :: [Variable] -> [ConstraintEl] -> Int -> IO [Variable]
updateEachTimes vs constraintSet n
  | n > 0 = do
    rvars <- updateEach vs constraintSet
    updateEachTimes rvars constraintSet (n - 1)
  | otherwise = return vs

-- |Checks if every probability in the distribution is either 0 or 1. If it is,
-- then, all constraints have been satisfied.
checkDistrSolved :: Distribution -> Bool
checkDistrSolved (Distribution p) = all (\x -> x == 0.0 || x == 1.0) p

-- |Check if the constraints have been solved by looking at the distributions
-- of each 'Variable'.
checkSolved :: [Variable] -> Bool
checkSolved [] = True
checkSolved (var:vars)
  | checkDistrSolved $ distr var = checkSolved vars
  | otherwise = False

-- |This is the moost important function within this library. Given a list of
-- 'Variable' and a list of 'ConstraintEl', the library uses the Communcation Free Learning
-- Algorithm to return a 'Solved' value. See 'solveThreaded' for a parallelized implementation.
solve :: [Variable] -> [ConstraintEl] -> IO Solved
solve vars constraints = do
  rvars <- updateEachTimes vars constraints 10
  if checkSolved rvars 
    then return $ Solved rvars 0
    else do 
      solved <- solve rvars constraints
      return $ Solved (variables solved) (iterationCount solved + 1)

updateMapF :: [Variable] -> [ConstraintEl] -> Int -> IO Variable
updateMapF vs constraints index = do
  rvars <- update index vs constraints
  return (rvars !! index)

-- |Updates each variable in the variable set a number of times and does each
-- variable's update in a separate thread.
updateEachParallel :: [Variable] -> [ConstraintEl] -> IO [Variable]
updateEachParallel vs constraints = do
  m <- mapM (updateMapF vs constraints) [0..(length vs)]
  -- evaluate the map in parallel
  let mp = m `using` parList rdeepseq in return mp

updateEachTimesParallel :: [Variable] -> [ConstraintEl] -> Int -> IO [Variable]
updateEachTimesParallel vs constraints times
  | times == 0 = return vs
  | otherwise = do
    -- this does not appear to be used...
    rvars <- updateEachParallel vs constraints
    updateEachTimesParallel vs constraints (times - 1)

-- |Solve the constraint set in parallel using Haskell threads. In order for
-- the solution to be parallelized, the program using DCFL must be compiled
-- with GHC's '-threaded' option.
solveParallel :: [Variable] -> [ConstraintEl] -> IO Solved
solveParallel vars constraints = do
  rvars <- updateEachTimesParallel vars constraints 10
  if checkSolved rvars
    then return $ Solved rvars 0
    else do
      solved <- solve rvars constraints
      return $ Solved (variables solved) (iterationCount solved + 1)
