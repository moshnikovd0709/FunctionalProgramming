module Block1.Task1Spec
  ( spec,
  ) where

import Block1.Task1
  ( NameOfDay (..),
    afterDays,
    daysToParty,
    isWeekend,
    nextDay,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do

  describe "nextDay" $ do
    it "OK" $ nextDay Monday `shouldBe` Tuesday
    it "OK" $ nextDay Tuesday `shouldBe` Wednesday
    it "OK" $ nextDay Wednesday `shouldBe` Thursday
    it "OK" $ nextDay Thursday `shouldBe` Friday
    it "OK" $ nextDay Friday `shouldBe` Saturday
    it "OK" $ nextDay Saturday `shouldBe` Sunday
    it "OK" $ nextDay Sunday `shouldBe` Monday

  describe "afterDays" $ do
    it "OK" $ afterDays Monday 3 `shouldBe` Thursday
    it "OK" $ afterDays Friday 11 `shouldBe` Tuesday
    it "OK" $ afterDays Sunday 1 `shouldBe` Monday

  describe "isWeekend" $ do
    it "OK" $ isWeekend Monday `shouldBe` False
    it "OK" $ isWeekend Tuesday `shouldBe` False
    it "OK" $ isWeekend Wednesday `shouldBe` False
    it "OK" $ isWeekend Thursday `shouldBe` False
    it "OK" $ isWeekend Friday `shouldBe` False
    it "OK" $ isWeekend Saturday `shouldBe` True
    it "OK" $ isWeekend Sunday `shouldBe` True

  describe "daysToParty" $ do
    it "OK" $ daysToParty Monday `shouldBe` 4
    it "OK" $ daysToParty Tuesday `shouldBe` 3
    it "OK" $ daysToParty Wednesday `shouldBe` 2
    it "OK" $ daysToParty Thursday `shouldBe` 1
    it "OK" $ daysToParty Friday `shouldBe` 0
    it "OK" $ daysToParty Saturday `shouldBe` 6
    it "OK" $ daysToParty Sunday `shouldBe` 5