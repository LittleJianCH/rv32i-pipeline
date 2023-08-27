{-# LANGUAGE PartialTypeSignatures #-}

module RV32I.Regfile where

import Clash.Prelude

import RV32I.Types (Reg, BV32)

regfile
  :: (HiddenClockResetEnable dom)
  => Signal dom Reg
  -> Signal dom Reg
  -> Signal dom Reg
  -> Signal dom BV32
  -> Signal dom (BV32, BV32)
regfile addrA addrB addrW dataW = view <*> regVec
  where
    regVec :: Signal _ (Vec 32 BV32)
    regVec = register (repeat 0) (replace <$> addrW <*> dataW <*> regVec)

    visit :: Reg -> Vec 32 BV32 -> BV32
    visit 0 _    = 0
    visit n regs = regs !! n

    view = uncurry (liftA2 (,)) <$> liftA2 (,) (visit <$> addrA) (visit <$> addrB)
