{-# LANGUAGE LambdaCase, CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module RV32I.Memory (
  memory
) where

import Clash.Prelude

import RV32I.Types

#define RAM_SIZE 1024

memory
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom MemInstr
  -> Signal dom BV32
  -> Signal dom BV32
  -> Signal dom BV32
  -> (Signal dom BV32, Signal dom MMIOInstr)
memory instr aluOut dataW mmioDataR = (mux isPassBy aluOut memoryR, mmioInstr)
  where
    addr :: Signal dom BV32
    addr = aluOut

    inRAM :: Signal dom Bool
    inRAM = addr .<. RAM_SIZE

    isPassBy :: Signal dom Bool
    isPassBy = fmap (\case {
      PassbyMem -> True ;
      _ -> False
    }) instr

    mmioInstr :: Signal dom MMIOInstr
    mmioInstr = mux inRAM
                    (pure defaultMMIOInstr)
                    (liftA3 (\addr_ instr_ data_ ->
                        case instr_ of {
                          ReadMem _ -> MMIOInstr addr_ Nothing ;
                          WriteMem _ -> MMIOInstr addr_ (Just data_) ;
                          _ -> defaultMMIOInstr
                        }) addr instr dataW)
      where defaultMMIOInstr = MMIOInstr 1024 Nothing

    memoryR :: Signal dom BV32
    memoryR = mux inRAM ramR mmioDataR

#define RAM_GEN(NAME, BV, RANGE_H, RANGE_L, SIZE) \
  NAME = let wCmd = liftA3 (\instr_ addr_ dataW_ -> \
                       case instr_ of { \
                         WriteMem s | s <= SIZE \
                           -> Just (addr_, slice RANGE_H RANGE_L dataW_) ; \
                         _ -> Nothing \
                    }) instr addr dataW \
         in (blockRam (repeat 0 :: Vec RAM_SIZE (BV)) addr wCmd :: Signal dom (BV))

    RAM_GEN(ram0, BitVector 8, d7, d0, Byte)
    RAM_GEN(ram1, BitVector 8, d15, d8, Half)
    RAM_GEN(ram2, BitVector 16, d31, d16, Word)
#undef RAM_GEN

    size :: Signal dom MemSize
    size = fmap (\case {
      ReadMem s -> s;
      _ -> Word -- we don't care about size when writing or passing by
    }) instr

    ramR :: Signal dom BV32
    ramR = liftA2 (\s (r0, r1, r2) -> -- this's sad that liftA4 isn't available in Clash.Prelude
             case s of
               Byte -> signExtend r0
               UByte -> extend r0
               Half -> signExtend $ r1 ++# r0
               UHalf -> extend $ r1 ++# r0
               Word -> r2 ++# r1 ++# r0
           ) size $ bundle (ram0, ram1, ram2)

#undef RAM_SIZE
