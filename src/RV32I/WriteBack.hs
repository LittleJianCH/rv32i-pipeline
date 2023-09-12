{-# LANGUAGE LambdaCase #-}

module RV32I.WriteBack (
  writeBack
) where

import Clash.Prelude

import RV32I.Types

-- this module looks like a no-op, and it is
writeBack
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom BV32
  -> Signal dom WBInstr
  -> Signal dom (Reg, BV32)
writeBack result instr = liftA2 (,) (reg <$> instr) result
  where
    reg :: WBInstr -> Reg
    reg = \case
      WriteReg r -> r
