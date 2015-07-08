import Data.DCFL
import Data.List

-- creates the set of variables; n colors possible
colorVariables n = 
  map (\x -> Variable [0..n-1] 0 (initDistribution n)) $ [0..n-1]

-- create a set of m variables that can take on values between 0 and n-1
-- inclusive.
finiteVariables m n = 
  map(\x -> Variable [0..n-1] 0 (initDistribution n)) $ [0..m-1]

-- constraint function for two different nodes in the network
-- specifies that their colors should not be equal
colorConstraint :: Int -> Int -> [Int] -> Bool
colorConstraint node1 node2 values = (values !! node1) /= (values !! node2)

-- construct the constraint set by currying
colorConstraints :: Int -> [ConstraintEl]
colorConstraints n = 
  [ConstraintEl [a, b] $ 
  colorConstraint a b | a <- [0..n-1], b <- [0..n-1], a /= b]

constraintOne values = (values !! 0) + (values !! 1) == 40
constraintTwo values = 2*(values !! 0) + 3*(values !! 1) == 100
constraints = [ConstraintEl [0, 1] constraintOne, 
  ConstraintEl [0, 1] constraintTwo]
  
main = do
  rsolved <- solveThreaded 2 vars consts
  putStrLn $ show $ getValues $ variables rsolved
  where vars = finiteVariables 20 100
        consts = constraints


