module RV32I.ALU (
  alu
) where

import Clash.Prelude

import RV32I.Types (AluInstr(AluInstr), AluOp(..), AluTarget, Reg, BV32)
import Data.Maybe (fromMaybe, isNothing)

alu :: AluInstr -> BV32 -> BV32 -> BV32 -> (BV32, BV32, Reg, Reg, AluTarget)
alu (AluInstr op rs1 rs2 imm target) pc regA regB = (result, regB, addrA, addrB, target)
  where
    a = if isNothing rs1 then pc else regA
    b = fromMaybe regB imm

    result = case op of
      Add -> a + b
      Sub -> a - b
      Sll -> a `shiftL` toShiftI b
      Slt -> boolToBV $ asSign a < asSign b
      Sltu -> boolToBV $ a < b
      Xor -> a `xor` b
      Srl -> a `shiftR` toShiftI b
      Sra -> pack $ asSign a `shiftR` toShiftI b
      Or -> a .|. b
      And -> a .&. b
      Eq -> boolToBV $ a == b
      Neq -> boolToBV $ a /= b
      Sge -> boolToBV $ asSign a >= asSign b
      Sgeu -> boolToBV $ a >= b

      where toShiftI = unpack . resize . slice d4 d0
            asSign = unpack :: BV32 -> Signed 32

    addrA = fromMaybe 0 rs1
    addrB = rs2
