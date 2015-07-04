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

data ConstraintEl = ConstraintEl {variableIndex1 :: Int, variableIndex2 :: Int,
  constraint :: ([Int] -> Bool)}

instance Show ConstraintEl where
  show (ConstraintEl variableIndex1 variableIndex2 _) = 
    "Constraint " ++ (show variableIndex1) ++ " " ++ (show variableIndex2)

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
