module DiplomacySpec where

import Test.Hspec(Spec, it, pending, shouldBe)
import Diplomacy(solve , Unit(..), Province(..), Country(..), Order(..))
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
        oob = [ (Austria, Army Venice) ]
        map = [ (Venice, Rome) ]
    solve noOrders oob map `shouldBe` oob

  it "solve a single order on an non-empty oob and map" $ do
    let orders = [(Austria, Move (Army Venice) Rome)]
        oob = [ (Austria, Army Venice) ]
        map = [ (Venice, Rome) ]
    solve orders oob map `shouldBe` [(Austria, Army Rome)]
