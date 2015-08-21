import Data.DCFL
import Data.List

colorVariables n = 
	map (\x -> Variable [0..n-1] 0 $ initDistribution n) [0..n-1]

colorConstraint:: Int -> Int -> [Int] -> Bool
colorConstraint n1 n2 vars = (vars !! n1) /= (vars !! n2)

colorConstraints n = 
  [ConstraintEl [a, b] $ colorConstraint a b | 
    a <- [0..n-1], b <- [0..n-1], a /= b]

main = do
  rsolved <- solveParallel (colorVariables 5) (colorConstraints 5)
  putStrLn $ show $ getValues $ variables rsolved


