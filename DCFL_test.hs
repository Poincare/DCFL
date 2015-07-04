import Test.HUnit
import DCFL

basicDist = initDistribution 5
testInitDistribution = 
	TestCase (assertEqual "for init distribution:" (m !! 0) 0.2) where
		m = probab basicDist

testCummDistribution =
	TestCase (assertEqual "for cumm distribution:" (m !! 0) 0.2) where
		m = probab $ cummDistribution basicDist

tests = TestList [TestLabel "testInitDistribution" testInitDistribution,
									TestLabel "testCummDistribution" testCummDistribution]
