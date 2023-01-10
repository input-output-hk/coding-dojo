module DiplomacySpec where

import Control.Monad (foldM)
import Data.Function ((&))
import Data.List (tails)
import Diplomacy (Country (..), GameMap, Order (..), Position, Province (..), Unit (..), adjacent, allCountries, allProvinces, defaultMap, neighbours, solve)
import Test.Hspec (Spec, it, pending, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Property, counterexample, elements, forAll, oneof, suchThat, (==>))
import Test.QuickCheck.Gen (Gen)

spec :: Spec
spec = do
    it "solve an order book" $ do pending
    -- let orders = [ (Austria, Move (Army Venice) Rome),
    --                (Germany, Support (Navy Tyrrhenian) (Move (Army Venice) Rome))
    --               ]
    --     oob = [ (Austria, Army Venice),
    --             (Germany, Navy Tyrrhenian),
    --             (Italy, Army Rome)
    --           ]
    --     map  = [ (Venice, Rome) , (Tuscany, Rome), (Venice, Tuscany) ]
    --  solve orders oob map `shouldBe`
    --    [ (Austria, [Army Rome, Army Tuscany])]
    it "solve an empty order book on an empty oob and map" $ do
        let noOrders = []
            emptyOob = []
            emptyMap = []
        solve noOrders emptyOob emptyMap `shouldBe` emptyOob

    it "solve an empty order book on an non-empty oob and map" $ do
        let noOrders = []
            oob = [(Austria, Army Venice)]
            map = [(Venice, Rome)]
        solve noOrders oob map `shouldBe` oob

    prop "solve a single move order on an non-empty oob and map" prop_solve_single_move_order
    prop "multiple conflicting orders cancel themselves" prop_conflicting_moves_cancel_themselves

newtype Oob = Oob [Position]
    deriving (Eq, Show)

instance Arbitrary Oob where
    arbitrary = Oob <$> foldM genUnit [] (enumFromTo minBound maxBound)
    shrink (Oob pos) = Oob <$> tails pos

instance Arbitrary Country where
    arbitrary = elements allCountries

genUnit :: [Position] -> Province -> Gen [Position]
genUnit positions province = do
    pos <- oneof [pure Nothing, Just <$> ((,) <$> arbitrary <*> genArmy province)]
    pure $ maybe positions (: positions) pos

genArmy :: Province -> Gen Unit
genArmy = pure . Army

prop_solve_single_move_order :: Oob -> Property
prop_solve_single_move_order (Oob oob) =
    not (null oob)
        ==> forAll (genOneMove oob defaultMap)
        $ \move@(Move cty (Army src) dest) ->
            let oob' = solve [move] oob defaultMap
             in (cty, Army dest) `elem` oob'
                    && (cty, Army src) `notElem` oob'
                        & counterexample ("new oob: " <> show oob')

prop_conflicting_moves_cancel_themselves :: ConflictingMoves -> Property
prop_conflicting_moves_cancel_themselves (ConflictingMoves oob move1 move2) =
    let oob' = solve [move1, move2] oob defaultMap
     in oob == oob'
            & counterexample ("new oob: " <> show oob')

data ConflictingMoves = ConflictingMoves [Position] Order Order
    deriving (Eq, Show)

instance Arbitrary Province where
    arbitrary = elements allProvinces

instance Arbitrary ConflictingMoves where
    arbitrary = do
        dest <- arbitrary
        src1 <- arbitrary `suchThat` \p -> (p /= dest) && adjacent defaultMap p dest
        src2 <- arbitrary `suchThat` \p -> (p /= dest) && adjacent defaultMap p dest && p /= src1
        cty1 <- arbitrary
        cty2 <- arbitrary
        pure $
            ConflictingMoves
                [(cty1, Army src1), (cty2, Army src2)]
                (Move cty1 (Army src1) dest)
                (Move cty2 (Army src2) dest)

genOneMove :: [Position] -> GameMap -> Gen Order
genOneMove positions map = do
    (cty, unit@(Army pos)) <- elements positions
    tgt <- elements $ neighbours pos map
    pure $ Move cty unit tgt
