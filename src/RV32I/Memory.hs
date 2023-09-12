{-# LANGUAGE LambdaCase #-}

module RV32I.Memory (
  memory
) where

import Clash.Prelude

import RV32I.Types

resizeData :: BV32 -> MemSize -> BV32
resizeData dataB = \case
  Byte -> signExtend . slice d7 d0 $ dataB
  UByte -> extend . slice d7 d0 $ dataB
  Half -> signExtend . slice d15 d0 $ dataB
  UHalf -> extend . slice d15 d0 $ dataB
  Word -> dataB

memory
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom MemInstr
  -> Signal dom BV32
  -> Signal dom BV32
  -> Signal dom BV32
  -> (Signal dom BV32, Signal dom MMIOInstr)
-- TODO: this interface should be changed
--       addr and dataW may both come from alu output
memory instr addr dataW mmioDataR = (mux isPassBy dataW memoryR, mmioInstr)
  where
    isPassBy :: Signal dom Bool
    isPassBy = fmap (\case {
      PassbyMem -> True ;
      _ -> False
    }) instr

    mmioInstr :: Signal dom MMIOInstr
    mmioInstr = mux (addr .<. 1024)
                    (pure defaultMMIOInstr)
                    (liftA3 (\addr_ instr_ data_ ->
                        case instr_ of {
                          ReadMem _ -> MMIOInstr addr_ Nothing ;
                          WriteMem _ -> MMIOInstr addr_ (Just data_) ;
                          _ -> defaultMMIOInstr
                        }) addr instr dataW)
      where defaultMMIOInstr = MMIOInstr 1024 Nothing

    memoryR :: Signal dom BV32
    memoryR = liftA2 resizeData (mux (addr .<. 1024) ramR mmioDataR) size
      where size = fmap (\case {
              ReadMem s -> s;
              _ -> Word -- we don't care about size when writing or passing by
            }) instr

    ramR :: Signal dom BV32
    ramR = blockRam (repeat 0 :: Vec 1024 BV32) addr wCmd
            -- this sad that multiple-scrutinee is only supported since GHC 9.4.1,
      where wCmd = liftA3 (\instr_ addr_ dataW_ ->
                     case instr_ of {
                       WriteMem s -> Just (resizeData dataW_ s, addr_) ;
                       _ -> Nothing
                   }) instr addr dataW
