{-# LANGUAGE TemplateHaskell #-}

module RV32I.PC (
  pc
) where

import Clash.Prelude
import Control.Lens

import RV32I.Types
import Data.Maybe

newtype Prediction = Prediction (Bit, BV32)
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

data PCState = PCState
  { _curPC :: Maybe BV32
  , _prediction :: Maybe Prediction
  , _ifLastBranch :: Bit }
  deriving stock (Generic, Show, Eq)
  deriving anyclass NFDataX

makeLenses ''PCState

pc
  :: forall dom. HiddenClockResetEnable dom
  => Signal dom (Maybe PCInstr)
  -> Signal dom BV32
  -> Signal dom (Maybe BV32, Bit)
pc pcInstr_ aluResult_ = mealy func (PCState (Just 0) Nothing 1) (bundle (pcInstr_, aluResult_))
  where func :: PCState -> (Maybe PCInstr, BV32) -> (PCState, (Maybe BV32, Bit))
        func pcState (pcInstr, aluResult) =
            convert . checkLastPredict . predict . jumpToAlu $ pcState
          where
            jumpToAlu :: PCState -> PCState
            jumpToAlu pcs = case pcs ^. curPC of
              Nothing -> pcs & curPC ?~ aluResult
              Just _ -> pcs

            predict :: PCState -> PCState
            predict pcs = case pcInstr of
              Nothing -> pcs
              Just (Branch offset) ->
                -- curPC can't be Nothing here
                let curPC_ = fromMaybe 0 (pcs ^. curPC)
                in if pcs ^. ifLastBranch == 1
                  then pcs & curPC %~ fmap (+ offset) & prediction ?~ Prediction (1, curPC_ + 4)
                  else pcs & curPC %~ fmap (+ 4) & prediction ?~ Prediction (0, curPC_ + offset)
              Just (Jump offset) ->
                pcs & curPC %~ fmap (+ offset) & prediction .~ Nothing
              Just JumpAlu ->
                -- wait for aluResult
                pcs & curPC .~ Nothing & prediction .~ Nothing

            lastPrediction :: Maybe Prediction
            lastPrediction = pcState ^. prediction

            checkLastPredict :: PCState -> (Bit, PCState)
            checkLastPredict pcs = case lastPrediction of
              Nothing -> (0, pcs)
              Just (Prediction (lastChoice, failTarget)) ->
                let correctChoice = boolToBit (aluResult == 1)
                in if lastChoice == correctChoice
                then (0, pcs & ifLastBranch .~ correctChoice)
                else (1, pcs & curPC ?~ failTarget
                             & prediction .~ Nothing
                             & ifLastBranch .~ correctChoice)

            convert :: (Bit, PCState) -> (PCState, (Maybe BV32, Bit))
            convert (clear, pcs) = (pcs, (pcs ^. curPC, clear))
