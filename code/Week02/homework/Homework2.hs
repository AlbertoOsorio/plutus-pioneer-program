{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Homework2 where

import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx
import PlutusTx.Prelude 
import Prelude (IO)
import Utils (writePlutusFile)
import Prelude (undefined)
import Utils (wrap)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    }

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- Create a validator that unlocks the funds if MyRedemeer's flags are different
mkValidator :: () -> MyRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ (MyRedeemer x y) _ = x /= y

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrap mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])

saveVal :: IO ()
saveVal = writePlutusFile "./assets/hw2.plutus" validator