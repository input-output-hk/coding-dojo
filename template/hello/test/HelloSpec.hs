module HelloSpec where

import Test.Hspec(Spec, shouldBe)
import Hello(greetings)

spec :: Spec
spec = do
  it "say 👋 to the 🌍" $ do greetings `shouldBe` "Hello, World!"

-- Run me with `cabal test --test-show-details=direct`
