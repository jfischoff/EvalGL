{-# LANGUAGE DeriveGeneric #-}
module Command where
import GHC.Generics

type CommandResult a b = Either a b

data Command a b c = Command
    {
        command_input  :: a,
        command_result :: CommandResult b c
    }
    deriving(Show, Eq, Generic)