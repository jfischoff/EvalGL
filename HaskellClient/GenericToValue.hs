{-# LANGUAGE DeriveGeneric, KindSignatures, TemplateHaskell, 
    QuasiQuotes, FlexibleInstances, TypeOperators, TypeSynonymInstances,
    MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
    ScopedTypeVariables, EmptyDataDecls, DefaultSignatures, ViewPatterns,
    UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances #-}
module GenericToValue where
import StructCreation
import Language.Haskell.TH
import Data.Word
import GHC.Generics
import Data.DList (DList, toList)
import Data.Monoid (mappend)
import Control.Applicative






    
