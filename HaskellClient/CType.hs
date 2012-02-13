{-# LANGUAGE DeriveGeneric, KindSignatures, TemplateHaskell, 
   QuasiQuotes, FlexibleInstances, TypeOperators, TypeSynonymInstances,
   MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
   ScopedTypeVariables, EmptyDataDecls, DefaultSignatures, ViewPatterns,
   UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances,
   DeriveDataTypeable #-}
module CType where
import Data.Data
import Data.Typeable
import TypeLevel.NaturalNumber
    
data CTPrimitive = CTPInt
                 | CTPchar
                 | CTPvoid
            deriving(Show, Eq, Data, Typeable)

data CType = TStruct String [CType]
           | TUnion  String [CType]
           | TPrimitive CTPrimitive
           | TArray Int CType
           | TPointer CType
           | TVariable CType
           | TMember String CType
           | TNamed String
           | TFuncPointer [CType]
           | TEnum String [String]
           deriving(Show, Eq, Data, Typeable)
           

class ToCType a where
   to_c_type :: a -> CType
   
data FixedArray n a = FixedArray n [a]
    deriving(Eq, Show)

instance (NaturalNumber n, ToCType a) => ToCType (FixedArray n a) where
    to_c_type = const $ TArray (naturalNumberAsInt (undefined :: n)) $ to_c_type (undefined :: a)
    

instance ToCType Int where
    to_c_type = const $ TPrimitive CTPInt
    
    
    
{-

I need a way to represent a function

each command must generate a statement

it will also generate a 


-}
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    