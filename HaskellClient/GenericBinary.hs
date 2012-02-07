{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable, ForeignFunctionInterface,
    OverloadedStrings #-}
module GenericBinary where
    
import qualified Data.Binary as DB
import qualified Data.Binary.Put as DB
import qualified Data.Binary.Get as DB
import GHC.Generics


class GBinary f where
    gput :: f a -> DB.Put
    
instance GBinary U1 where
    gput x = return ()
    
instance (GBinary a, GBinary b) => GBinary (a :*: b) where
    gput (x :*: y) = do gput x; gput y
        
instance (GBinary a, GBinary b) => GBinary (a :+: b) where
  gput (L1 x) = gput x
  gput (R1 x) = gput x
  
instance (GBinary a) => GBinary (M1 i c a) where
  gput (M1 x) = gput x

instance (DB.Binary a) => GBinary (K1 i a) where
  gput (K1 x) = DB.put x
  

