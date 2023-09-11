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
memory instr addr dataW = mux isPassBy dataW ram
  where
    isPassBy :: Signal dom Bool
    isPassBy = fmap (\case {
      PassbyMem -> True ;
      _ -> False
    }) instr

    ram :: Signal dom BV32
    ram = liftA2 resizeData (blockRam (repeat 0 :: Vec 1024 BV32) addr wCmd) size
            -- this sad that multiple-scrutinee is only supported since GHC 9.4.1,
      where wCmd = liftA3 (\instr_ addr_ dataW_ ->
                     case instr_ of {
                       WriteMem s -> Just (resizeData dataW_ s, addr_) ;
                       _ -> Nothing
                   }) instr addr dataW

            size = fmap (\case {
              ReadMem s -> s;
              _ -> Word -- we don't care about size when writing or passing by
            }) instr



