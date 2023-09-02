module RV32I.Regfile (
  regfile, regfileInterface
) where

import Clash.Prelude

import RV32I.Types (Reg, BV32)
import Data.Function (on)
import qualified Data.List.NonEmpty as D (unzip)

regfile
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom Reg
  -> Signal dom Reg
  -> Signal dom Reg
  -> Signal dom BV32
  -> Signal dom (BV32, BV32)
regfile addrA addrB addrW dataW = view <*> regVec
  where
    regVec :: Signal dom (Vec 32 BV32)
    regVec = register (repeat 0) (liftA3 replace addrW dataW regVec)

    visit :: Reg -> Vec 32 BV32 -> BV32
    visit 0 _    = 0
    visit n regs = regs !! n

    view :: Signal dom (Vec 32 BV32 -> (BV32, BV32))
    view = uncurry (liftA2 (,)) <$> (liftA2 (,) `on` (visit <$>)) addrA addrB

-- we use regfileInterface to handle the bypassing
regfileInterface
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom Reg
  -> Signal dom Reg
  -> Signal dom Reg
  -> Signal dom BV32
  -> Signal dom (Reg, BV32)
  -> Signal dom (Reg, Maybe BV32)
  -> Signal dom (Maybe BV32, Maybe BV32)
regfileInterface
  addrA addrB addrW dataW wbSignal memSignal =
    let (dataA', dataB') = D.unzip $ regfile addrA addrB addrW dataW
        (addrWB, dataWB) = D.unzip wbSignal
        (addrMem, dataMem) = D.unzip memSignal

        readByPass :: Signal dom Reg -> Signal dom BV32 -> Signal dom (Maybe BV32)
        readByPass addr dataR = mux (addr .==. addrMem)  dataMem $
                                mux (addr .==. addrWB)   (fmap Just dataWB)
                                                         (fmap Just dataR)
    in bundle (readByPass addrA dataA', readByPass addrB dataB')
-- written when I finished the implementation:
-- I think the argument `wbSignal` is equal to `bundle (addrW,  dataW)`,
--   but I still want to keep it

