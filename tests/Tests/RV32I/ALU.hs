module Tests.RV32I.ALU where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import RV32I.Types (AluInstr(..), AluOp(..))
import RV32I.ALU (alu)

prop_aluTest :: H.Property
prop_aluTest = H.property $ do
  a <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  b <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  r1 <- H.forAll (Gen.integral (Range.linear minBound maxBound))
  r2 <- H.forAll (Gen.integral (Range.linear minBound maxBound))

  let aluInstr = AluInstr Add r1 (Left r2)

  let result = alu aluInstr a b

  result === (a + b, r1, r2)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
