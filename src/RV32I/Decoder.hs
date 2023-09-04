module RV32I.Decoder where

import Clash.Prelude

import RV32I.Types hiding (rs1, rs2)

decoder :: BV32 -> (AluInstr, PCInstr, MemInstr, WBInstr)
decoder i = case i of
  $(bitPattern "0000000..........000.....0110011") {- add -}
    -> (AluInstr Add (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0100000..........000.....0110011") {- sub -}
    -> (AluInstr Sub (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........100.....0110011") {- xor -}
    -> (AluInstr Xor (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........110.....0110011") {- or -}
    -> (AluInstr Or (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........111.....0110011") {- and -}
    -> (AluInstr And (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........001.....0110011") {- sll -}
    -> (AluInstr Sll (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........101.....0110011") {- srl -}
    -> (AluInstr Srl (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0100000..........101.....0110011") {- sra -}
    -> (AluInstr Sra (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........010.....0110011") {- slt -}
    -> (AluInstr Slt (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........011.....0110011") {- sltu -}
    -> (AluInstr Sltu (Just rs1) rs2 Nothing Rd, pcNext, PassbyMem, WriteAlu rd)

  $(bitPattern ".................000.....0010011") {- addi -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern ".................100.....0010011") {- xori -}
    -> (AluInstr Xor (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern ".................110.....0010011") {- ori -}
    -> (AluInstr Or (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern ".................111.....0010011") {- andi -}
    -> (AluInstr And (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........000.....0010011") {- slli -}
    -> (AluInstr Sll (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0000000..........101.....0010011") {- srli -}
    -> (AluInstr Srl (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern "0100000..........101.....0010011") {- srai -}
    -> (AluInstr Sra (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern ".................010.....0010011") {- slti -}
    -> (AluInstr Slt (Just rs1) 0 (Just sImmI) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern ".................011.....0010011") {- sltiu -}
    -> (AluInstr Sltu (Just rs1) 0 (Just uImmI) Rd, pcNext, PassbyMem, WriteAlu rd)

  $(bitPattern ".................000.....0000011") {- lb -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) Rd, pcNext, ReadMem Byte, WriteAlu rd)
  $(bitPattern ".................001.....0000011") {- lh -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) Rd, pcNext, ReadMem Half, WriteAlu rd)
  $(bitPattern ".................010.....0000011") {- lw -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) Rd, pcNext, ReadMem Word, WriteAlu rd)
  $(bitPattern ".................100.....0000011") {- lbu -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) Rd, pcNext, ReadMem UByte, WriteAlu rd)
  $(bitPattern ".................101.....0000011") {- lhu -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) Rd, pcNext, ReadMem UHalf, WriteAlu rd)

  $(bitPattern ".................000.....0100011") {- sb -}
    -> (AluInstr Add (Just rs1) rs2 (Just immS) Rd, pcNext, WriteMem Byte, PassbyWB)
  $(bitPattern ".................001.....0100011") {- sh -}
    -> (AluInstr Add (Just rs1) rs2 (Just immS) Rd, pcNext, WriteMem Half, PassbyWB)
  $(bitPattern ".................010.....0100011") {- sw -}
    -> (AluInstr Add (Just rs1) rs2 (Just immS) Rd, pcNext, WriteMem Word, PassbyWB)

  $(bitPattern ".................000.....1100011") {- beq -}
    -> (AluInstr Eq (Just rs1) rs2 Nothing BranchCond, Jump immB, PassbyMem, PassbyWB)
  $(bitPattern ".................001.....1100011") {- bne -}
    -> (AluInstr Neq (Just rs1) rs2 Nothing BranchCond, Jump immB, PassbyMem, PassbyWB)
  $(bitPattern ".................100.....1100011") {- blt -}
    -> (AluInstr Slt (Just rs1) rs2 Nothing BranchCond, Jump immB, PassbyMem, PassbyWB)
  $(bitPattern ".................101.....1100011") {- bge -}
    -> (AluInstr Sge (Just rs1) rs2 Nothing BranchCond, Jump immB, PassbyMem, PassbyWB)
  $(bitPattern ".................110.....1100011") {- bltu -}
    -> (AluInstr Sltu (Just rs1) rs2 Nothing BranchCond, Jump immB, PassbyMem, PassbyWB)
  $(bitPattern ".................111.....1100011") {- bgeu -}
    -> (AluInstr Sgeu (Just rs1) rs2 Nothing BranchCond, Jump immB, PassbyMem, PassbyWB)

  $(bitPattern ".........................1101111") {- jal -}
    -> (AluInstr Add Nothing 0 (Just 4) Rd, Jump (signExtend immJ), PassbyMem, WriteAlu rd)
  $(bitPattern ".........................1100111") {- jalr -}
    -> (AluInstr Add (Just rs1) 0 (Just sImmI) JumpTarget, JumpAlu, PassbyMem, WritePC rd)

  $(bitPattern ".........................0110111") {- lui -}
    -> (AluInstr Or (Just 0) 0 (Just immU) Rd, pcNext, PassbyMem, WriteAlu rd)
  $(bitPattern ".........................0010111") {- auipc -}
    -> (AluInstr Add Nothing 0 (Just immU) Rd, pcNext, PassbyMem, WriteAlu rd)
  _ {- undefined instruction -} -> undefined
  where rd = slice d11 d7 i
        rs1 = slice d19 d15 i
        rs2 = slice d24 d20 i
        immI = slice d31 d20 i
        sImmI = signExtend immI
        uImmI = extend immI
        immS = signExtend $ slice d31 d25 i ++# slice d11 d7 i
        immB = signExtend $ slice d31 d31 i ++# slice d7 d7 i ++# slice d30 d25 i ++# slice d11 d8 i ++# $(bLit "0")
        immU = slice d31 d12 i ++# $(bLit "000000000000")
        immJ = slice d31 d31 i ++# slice d19 d12 i ++# slice d20 d20 i ++# slice d30 d21 i ++# $(bLit "0")

        pcNext = Jump 4

