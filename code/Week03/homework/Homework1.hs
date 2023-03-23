{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, ($), (&&), traceIfFalse)
import           Utilities            (wrap)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = 
  if deadlineReached
    then traceIfFalse "Deadline has been reached. Second beneficiary's signature missing" signedByBeneficiary2
  else traceIfFalse "Deadline has not been reached. First beneficiary's signature missing" signedByBeneficiary1
  where
    info :: TxInfo
    info = scriptContextTxInfo _ctx

    signedByBeneficiary1 :: Bool
    signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

    signedByBeneficiary2 :: Bool
    signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline _dat) $ txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
