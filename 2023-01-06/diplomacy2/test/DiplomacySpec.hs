module DiplomacySpec where

import Control.Monad (foldM)
import Data.Function ((&))
import Diplomacy (Country (..), GameMap, Order (..), Position, Province (..), Unit (..), solve)
import Test.Hspec (Spec, it, pending, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Property, arbitrary, counterexample, elements, forAll, oneof, (==>))
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

    prop "solve a single order on an non-empty oob and map" prop_solve_single_move_order

newtype Oob = Oob [Position]
    deriving (Eq, Show)

instance Arbitrary Oob where
    arbitrary = Oob <$> foldM genUnit [] (enumFromTo minBound maxBound)

instance Arbitrary Country where
    arbitrary = elements $ enumFromTo minBound maxBound

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
        $ \move@(cty, Move (Army src) dest) ->
            let oob' = solve [move] oob defaultMap
             in (cty, Army dest) `elem` oob'
                    && (cty, Army src) `notElem` oob'
                        & counterexample ("new oob: " <> show oob')

genOneMove :: [Position] -> GameMap -> Gen (Country, Order)
genOneMove positions map = do
    (cty, unit@(Army pos)) <- elements positions
    tgt <- elements $ neighbours pos map
    pure (cty, Move unit tgt)

neighbours :: Province -> GameMap -> [Province]
neighbours province = foldr (neighbour province) []

neighbour :: Province -> (Province, Province) -> [Province] -> [Province]
neighbour p (p1, p2) ps
    | p == p1 && p2 `notElem` ps = p2 : ps
    | p == p2 && p1 `notElem` ps = p1 : ps
    | otherwise = ps

defaultMap :: GameMap
defaultMap = [(Venice, Rome)]
