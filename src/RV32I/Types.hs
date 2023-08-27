module RV32I.Types (
  AluOp(..),
  AluInstr(..),
  BV32,
  Reg
) where

import Clash.Prelude (Eq, Show, Generic, Either, BitVector, NFDataX)

type BV32 = BitVector 32
type Reg = BitVector 5

data AluOp = Add
           | Sub
           | Sll
           | Slt
           | Sltu
           | Xor
           | Srl
           | Sra
           | Or
           | And
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data AluInstr = AluInstr
  { aluOp :: AluOp
  , aluA  :: Reg
  , aluB  :: Either Reg BV32 }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

