{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction #-}
module Conversion where
import GHC.Generics
--import qualified PackedSerialization as P
import StructCreation
import Data.Tuple.Select
import Data.Maybe
import GLPrimitives
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS
import TemplateReflect
import Control.Arrow
import Data.Word
import Data.Data
import Data.Typeable

v_member = VMember . VPrimitive

v_struct = VStruct . map v_member

data ManyC = Hey Int
           | You Char
           deriving(Typeable, Data)




instance ToValue TestObject 

mk_struct_member name typ = TMember name $ to_c_type typ

mk_member name typ = TMember name typ

mk_primitive_member name primitive = TMember name $ TPrimitive primitive

mk_record_ctype name members = TUnion name [TMember name $ TStruct "" members]



mk_array count typ = TArray count (to_c_type typ)

(<||>) = mk_array
infix 2 <||>

(<::>)  = mk_struct_member
infix 1 <::>

(<:::>) = mk_member
infix 1 <:::>

--this needs to be done with 
instance ToCType TestObject where
    to_c_type x = mk_record_ctype "TestObject" [
        mk_primitive_member "x" TGLint,
        mk_primitive_member "y" TGLchar,
        mk_primitive_member "z" TGLfloat]
        
instance ToValue TestList

instance ToCType TestList where
    to_c_type x = mk_record_ctype "TestList" [
        mk_primitive_member "test_list_x" TGLchar,
        TMember "list" $ TPointer $ to_c_type (undefined :: TestObject),
        mk_primitive_member "list_count" TGLuint]
        

encode :: (ToCType a, ToValue a) => a -> CEnv
encode x = result where
    (value, state) = runState (to_c_value (to_c_type x) (to_v x)) []
    result = map (second wrap_with_type_struct) ((fromIntegral $ length state, value):state)
    
wrap_with_type_struct :: CValue -> CValue
wrap_with_type_struct (Union s@(Struct xs) 0 (TUnion _ (typ:[]))) = s
wrap_with_type_struct u@(Union s index (TUnion _ (x:xs))) = Struct [
                        Primitive . PGLenum $ GLenum index,
                        u
                    ]
wrap_with_type_struct x = x

fromTMember (TMember name x) = (name, x)

find_type :: String -> [CType] -> (Word32, CType)
find_type x t_members = (\(i, (_,x)) -> (i, x)) $ head $ 
    filter ((x==) . fst . snd) $ zip [0..] $ map fromTMember t_members

--I need the description so I can properly create unions
to_c_value :: CType -> Value -> CState CValue
to_c_value (TStruct _ t_members) (VStruct v_members) = Struct <$> zipWithM to_c_value t_members v_members
to_c_value u@(TUnion _ t_members) (VUnion name x)         = do
    let (i, t) = (find_type (traceIt name) t_members)
    cv <- to_c_value t x
    return $ Union cv i u
to_c_value (TPrimitive _) (VPrimitive x)           = return $ Primitive x
to_c_value (TArray count typ) (VArray xs)          = Array <$> zipWithM to_c_value (cycle [typ]) xs
to_c_value (TPointer typ) (VArray xs)            = do
    ys <- zipWithM to_c_value (cycle [typ]) xs 
    id <- add_array (Array ys)
    return $ Struct [Pointer $ Id id, Primitive $ PGLuint $ fromIntegral $ length ys]
to_c_value (TMember _ x) (VMember y)               = do
     v <- to_c_value x y
     return $ Member v 0


    
    








    

