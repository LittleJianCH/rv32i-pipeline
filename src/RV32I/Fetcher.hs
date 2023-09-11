{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module RV32I.Fetcher (
  fetcher
) where

import Clash.Prelude

import RV32I.Types (BV32)

-- TODO: program should be read from a hex file
program :: Vec _ BV32
program = $(listToVecTH ([
    0X00F56593, -- ori a1, a0, 15
    0X00C58633, -- add a2, a1, a2
    0X06460613  -- addi a2, a2, 100
  ] :: [BV32]))

fetcher :: Signal dom (Maybe BV32) -> Signal dom (Maybe BV32)
fetcher = fmap . fmap $ asyncRom program
