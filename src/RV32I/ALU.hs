module RV32I.ALU (
  alu
) where

import Clash.Prelude

import RV32I.Types (AluInstr(..), AluOp(..), Reg, BV32)
import Data.Either (fromRight, fromLeft)

alu :: AluInstr -> BV32 -> BV32 -> (BV32, Reg, Reg)
alu (AluInstr op rs1 rs2) regA regB = (result, addrA, addrB)
  where
    a = regA
    b = fromRight regB rs2

    result = case op of
      Add -> a + b
      Sub -> a - b
      Sll -> a `shiftL` toShiftI b
      Slt -> boolToBV $ a < b
      Sltu -> boolToBV $ asSign a < asSign b
      Xor -> a `xor` b
      Srl -> a `shiftR` toShiftI b
      Sra -> pack $ asSign a `shiftR` toShiftI b
      Or -> a .|. b
      And -> a .&. b

      where toShiftI = unpack . resize . slice d4 d0
            asSign = unpack :: BV32 -> Signed 32

    addrA = rs1
    addrB = fromLeft 0 rs2
