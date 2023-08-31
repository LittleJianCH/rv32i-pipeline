import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import qualified Tests.RV32I.ALU

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Example.Project.tests
  , Tests.RV32I.ALU.tests
  ]
