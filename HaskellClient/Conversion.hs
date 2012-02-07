{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable, StandaloneDeriving #-}
module Conversion where
import GHC.Generics
--import qualified PackedSerialization as P
import StructCreation
import Data.Tuple.Select
import Data.Maybe
import Types
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS


instance ToValue TestObject where
    to_v (TestObject x y z) = VStruct [
        VMember $ VPrimitive $ PGLint x, 
        VMember $ VPrimitive $ PGLchar y,
        VMember $ VPrimitive $ PGLfloat z]
        
instance ToCType TestObject where
    to_c_type x = TStruct "TestObject" [
        TMember "x" $ TPrimitive TGLint,
        TMember "y" $ TPrimitive TGLchar,
        TMember "z" $ TPrimitive TGLfloat]
        
instance ToValue TestList where
    to_v (TestList x ys) = VStruct [
        VMember $ VPrimitive $ PGLchar x, 
        VMember $ VPointer $ map to_v ys]
        
instance ToCType TestList where
    to_c_type x = TStruct "TestList" [
        TMember "test_list_x" $ TPrimitive TGLchar,
        TMember "list"        $ TPointer $ to_c_type (undefined :: TestObject),
        TMember "count"       $ TPrimitive TGLuint]


encode :: (ToCType a, ToValue a) => a -> CEnv
encode x = result where
    (value, state) = runState (to_c_value (to_c_type x) (to_v x)) []
    result = (fromIntegral $ length state, value):state

fromTMember (TMember name x) = (name, x)

find_type :: String -> [CType] -> CType
find_type x t_members = snd $ head $ filter ((x==) . fst) $ map fromTMember t_members

--I need the description so I can properly create unions
to_c_value :: CType -> Value -> CState CValue
to_c_value (TStruct _ t_members) (VStruct v_members) = Struct <$> zipWithM to_c_value t_members v_members
to_c_value u@(TUnion _ t_members) (VUnion name x)         = do
    cv <- to_c_value (find_type name t_members) x 
    return $ Union cv u
to_c_value (TPrimitive _) (VPrimitive x)           = return $ Primitive x
to_c_value (TArray count typ) (VArray xs)          = Array <$> zipWithM to_c_value (cycle [typ]) xs
to_c_value (TPointer typ) (VPointer xs)            = do
    ys <- zipWithM to_c_value (cycle [typ]) xs 
    id <- add_array (Array ys)
    return $ Pointer $ Id id
to_c_value (TMember _ x) (VMember y)               = do
     v <- to_c_value x y
     return $ Member v 0


    
    






{-
gvalue_to_almost_cvalue x@(P.GStruct _ _) = S.Struct $ flatten_pairs x []
gvalue_to_almost_cvalue (P.GUnion x)      = S.Union (gvalue_to_almost_cvalue x) (S.TUnion "" [])
gvalue_to_almost_cvalue (P.GPrimitive x)  = S.Primitive x
gvalue_to_almost_cvalue (P.GArray x)      = S.Array $ flatten_pairs x []


flatten_pairs (P.GStruct _ g) xs = flatten_pairs g xs
flatten_pairs (P.GUnion g) xs    = flatten_pairs g xs
flatten_pairs (P.GPair x y) xs   = flatten_pairs x xs ++ flatten_pairs y xs
flatten_pairs (P.GMember _ x) xs = flatten_pairs x xs
flatten_pairs P.GEmpty xs        = xs
flatten_pairs x xs               = (S.Member (gvalue_to_almost_cvalue x) 0):xs
-}

    

