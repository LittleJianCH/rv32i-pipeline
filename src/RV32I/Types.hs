module RV32I.Types (
  AluOp(..),
  AluTarget(..),
  AluInstr(..),
  PCInstr(..),
  MemSize(..),
  MemInstr(..),
  WBInstr(..),
  MMIOInstr(..),
  BV32,
  Reg
) where

import Clash.Prelude (Eq, Show, Generic, BitVector, NFDataX, Maybe, Bool, Ord)

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
           -- for branch
           | Eq
           | Neq
           | Sge
           | Sgeu
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data AluTarget = Rd | BranchCond | JumpTarget
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

-- the only one situation that we need rs2 and imm both is store instruction
-- so for normal instructions, if imm appears, the second operand is imm
data AluInstr = AluInstr
  { aluOp      :: AluOp
  , rs1        :: Maybe Reg -- is Nothing when rs1 is PC
  , rs2        :: Reg
  , imm        :: Maybe BV32
  , target     :: AluTarget }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data PCInstr = Branch BV32 | Jump BV32 | JumpAlu
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data MemSize =  UByte | Byte | UHalf | Half | Word
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass NFDataX

data MemInstr = ReadMem MemSize | WriteMem MemSize | PassbyMem
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data WBInstr = WriteAlu Reg | WritePC Reg | PassbyWB
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data MMIOInstr = MMIOInstr
  { mmioAddr :: BV32
  , mmioData :: Maybe BV32 } -- Nothing for read
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX
