-- |
-- module: Client
--
-- This is an example client

{-# LANGUAGE LambdaCase #-}

module Client where

import           Core

data ClickTarget
    = Keys KeyTarget
    | Donate DonateTarget
    | Nav AppArea

data AppArea
    = ManageKeys
    | ModifySeed
    | ShowSeed

    | Donate

    | Start


data KeyTarget
    = EnterSeed
    | RandomSeed
    | ForgetSeed
    -- Nav ShowSeed


data SendDonation = SendDonation
    -- Nav Start


newtype Seed = Seed Text


data DataRep = SeedT | PaymentRequestT


data Stimulus
    = PushMessage (MessageTo Client)
    | Click ClickTarget


data ClientL
    = NoOp
    | Emit (MessageTo Server) (MessageTo Client -> ClientL)

    | WithInput Text (Maybe Text -> ClientL)

    | Put DataRep Value
    | Delete DataRep

    | Warning Text



interface :: Stimulus  -> ClientL
interface = \case
    PushMessage msg -> case msg of
        Ack                   -> NoOp

        PaymentRequestMsg req -> NoOp

        Confirmation idT key  -> NoOp

        Object v              -> NoOp

    Click target -> case target of
        Nav x             -> Navigate x

        Keys x            -> case x of
            EnterSeed  ->
                WithInput "keys.seedentry" $
                maybe NoOp $
                Put SeedT . toJSON . Seed

            RandomSeed ->
                WithRandomBytes 32 $
                Put . SeedT . toJSON .Seed

            ForgetSeed ->
                Warning "You are about to delete your seed!" $
                Delete SeedT

        SendDonation ->
            WithInput "donation.message" $ \text ->
            WithInput "donation.amount" $ \amount ->
            Donate <$> text <*> amount & maybe NoOp $ \donationMessage ->
            Emit donationMessage $ \case
                PaymentRequestMessage req ->
                    Install req $
                    Navigate PaymentRequests
                _ -> NoOp
