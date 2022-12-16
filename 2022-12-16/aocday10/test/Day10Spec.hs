{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Day10Spec where

import Test.Hspec.QuickCheck
import Test.Hspec
import Test.QuickCheck

execute :: String -> Int
execute _ = 42

data CpuState = CpuState { register :: Int, cycleCounter :: Int }
  deriving stock (Show, Eq)

initialState :: CpuState
initialState = CpuState{register = 1, cycleCounter = 0}

interpret :: [Command] -> CpuState -> CpuState
interpret (Noop:xs) cpuState@CpuState{cycleCounter} =
  let newState = cpuState { cycleCounter = cycleCounter + 1 }
      finalState = interpret xs newState
  in finalState
interpret _ state = state
interpret [] state = state

data Command = Noop | AddX Int
  deriving stock (Show, Eq)
 
instance Arbitrary Command where
  arbitrary = oneof [ pure Noop, AddX <$> arbitrary ]

newtype OnlyNoops = OnlyNoops { noops :: [Command] }
   deriving newtype (Eq, Show)

instance Arbitrary OnlyNoops where
  arbitrary = OnlyNoops <$> listOf (pure Noop)
  
noopIncrementTheCycleCounter :: OnlyNoops -> Property
noopIncrementTheCycleCounter (OnlyNoops cmds) =
  let finalState = interpret cmds initialState
      finalCounter = cycleCounter finalState
  in finalCounter === length cmds

spec :: Spec
spec = do
  describe "Integration test"  $ do
    xit "evaluate sample program" $ do
      execute testProgram `shouldBe` 13140
  describe "Unit test" $ do
    it "empty program does not change initial state" $ do
      interpret [] initialState `shouldBe` initialState
    prop "noop should imcreement the cycle counter 2" noopIncrementTheCycleCounter

testProgram :: String
testProgram = "addx 15\
 \ addx -11\
 \ addx 6\
 \ addx -3\
 \ addx 5\
 \ addx -1\
 \ addx -8\
 \ addx 13\
 \ addx 4\
 \ noop\
 \ addx -1\
 \ addx 5\
 \ addx -1\
 \ addx 5\
 \ addx -1\
 \ addx 5\
 \ addx -1\
 \ addx 5\
 \ addx -1\
 \ addx -35\
 \ addx 1\
 \ addx 24\
 \ addx -19\
 \ addx 1\
 \ addx 16\
 \ addx -11\
 \ noop\
 \ noop\
 \ addx 21\
 \ addx -15\
 \ noop\
 \ noop\
 \ addx -3\
 \ addx 9\
 \ addx 1\
 \ addx -3\
 \ addx 8\
 \ addx 1\
 \ addx 5\
 \ noop\
 \ noop\
 \ noop\
 \ noop\
 \ noop\
 \ addx -36\
 \ noop\
 \ addx 1\
 \ addx 7\
 \ noop\
 \ noop\
 \ noop\
 \ addx 2\
 \ addx 6\
 \ noop\
 \ noop\
 \ noop\
 \ noop\
 \ noop\
 \ addx 1\
 \ noop\
 \ noop\
 \ addx 7\
 \ addx 1\
 \ noop\
 \ addx -13\
 \ addx 13\
 \ addx 7\
 \ noop\
 \ addx 1\
 \ addx -33\
 \ noop\
 \ noop\
 \ noop\
 \ addx 2\
 \ noop\
 \ noop\
 \ noop\
 \ addx 8\
 \ noop\
 \ addx -1\
 \ addx 2\
 \ addx 1\
 \ noop\
 \ addx 17\
 \ addx -9\
 \ addx 1\
 \ addx 1\
 \ addx -3\
 \ addx 11\
 \ noop\
 \ noop\
 \ addx 1\
 \ noop\
 \ addx 1\
 \ noop\
 \ noop\
 \ addx -13\
 \ addx -19\
 \ addx 1\
 \ addx 3\
 \ addx 26\
 \ addx -30\
 \ addx 12\
 \ addx -1\
 \ addx 3\
 \ addx 1\
 \ noop\
 \ noop\
 \ noop\
 \ addx -9\
 \ addx 18\
 \ addx 1\
 \ addx 2\
 \ noop\
 \ noop\
 \ addx 9\
 \ noop\
 \ noop\
 \ noop\
 \ addx -1\
 \ addx 2\
 \ addx -37\
 \ addx 1\
 \ addx 3\
 \ noop\
 \ addx 15\
 \ addx -21\
 \ addx 22\
 \ addx -6\
 \ addx 1\
 \ noop\
 \ addx 2\
 \ addx 1\
 \ noop\
 \ addx -10\
 \ noop\
 \ noop\
 \ addx 20\
 \ addx 1\
 \ addx 2\
 \ addx 2\
 \ addx -6\
 \ addx -11\
 \ noop\
 \ noop\
 \ noop"
