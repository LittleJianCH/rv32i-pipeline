module Tests.RV32I.Regfile where

import Prelude

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import RV32I.Regfile (regfile)
import RV32I.Types (Reg, BV32)

import Data.List (mapAccumL, zip4)
import Data.Map (empty, findWithDefault, insert, Map)
import qualified Clash.Prelude as C

unzipMap4 :: ([(a, b, c, d)] -> e) -> ([a] -> [b] -> [c] -> [d] -> e)
unzipMap4 = (. zip4) . (.) . (.) . (.)

regfileSim :: [Reg] -> [Reg] -> [Reg] -> [BV32] -> [(BV32, BV32)]
regfileSim = unzipMap4 $ snd . mapAccumL step empty
  where step :: Map Reg BV32 -> (Reg, Reg, Reg, BV32) -> (Map Reg BV32, (BV32, BV32))
        step regs (addrA, addrB, addrW, dataW) = (regs', (dataA, dataB))
          where regs' = insert addrW dataW regs
                dataA = readReg addrA
                dataB = readReg addrB

                readReg    0 = 0
                readReg addr = findWithDefault 0 addr regs

prop_regfileTest :: H.Property
prop_regfileTest = H.property $ do
  let len = 100
  let rLen = Range.singleton len

  addrAs <- H.forAll $ Gen.list rLen (Gen.integral (Range.linear 0 31))
  addrBs <- H.forAll $ Gen.list rLen (Gen.integral (Range.linear 0 31))
  addrWs' <- H.forAll $ Gen.list rLen (Gen.integral (Range.linear 0 31))
  dataWs' <- H.forAll $ Gen.list rLen (Gen.integral (Range.linear minBound maxBound))

  -- since the first step of sample is reset, the first write is ignored
  let addrWs = 0 : addrWs'
  let dataWs = 0 : dataWs'

  let correctResults = regfileSim addrAs addrBs addrWs dataWs

  let sysRegfile = C.withClockResetEnable C.systemClockGen C.systemResetGen C.enableGen regfile


  let results = C.sampleN len $ sysRegfile (C.fromList addrAs)
                                           (C.fromList addrBs)
                                           (C.fromList addrWs)
                                           (C.fromList dataWs)

  results H.=== correctResults

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
