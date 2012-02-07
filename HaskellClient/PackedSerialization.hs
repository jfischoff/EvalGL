{-# LANGUAGE DeriveGeneric, TypeOperators, FlexibleInstances, TypeSynonymInstances,
    DefaultSignatures, FlexibleContexts, DeriveDataTypeable #-}
module PackedSerialization where
import GHC.Generics
import Types
import qualified Data.ByteString as BS
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Typeable
import Data.List
import qualified StructCreation as S
import Data.Word



data GValue = GStruct String GValue
            | GUnion GValue
            | GPrimitive S.GLPrimitive
            | GArray GValue
            | GEmpty
            | GPair GValue GValue
            | GMember String GValue
             deriving(Show, Eq)
                 
get_struct_members (GStruct _ (GUnion y)) = y
                 
                      
type Offset = Int
type Fixup = Offset

class GAsC f where
  g_to_g :: f a -> GValue

class AsC a where
    to_g :: a -> GValue
    default to_g :: (Generic a, GAsC (Rep a)) => a -> GValue
    to_g a = g_to_g (from a)
    
instance AsC GLbitfield where
    to_g = GPrimitive . S.PGLbitfield
    
instance AsC GLboolean where
    to_g = GPrimitive . S.PGLboolean
    
instance AsC GLbyte where
    to_g = GPrimitive . S.PGLbyte
    
instance AsC GLchar where
    to_g = GPrimitive . S.PGLchar
                     
instance AsC GLclampf where
    to_g = GPrimitive . S.PGLclampf
    
instance AsC GLenum where
    to_g = GPrimitive . S.PGLenum
             
instance AsC GLfloat where
    to_g = GPrimitive . S.PGLfloat
    
instance AsC GLint where
    to_g = GPrimitive . S.PGLint
    
instance AsC GLshort where
    to_g = GPrimitive . S.PGLshort
    
instance AsC GLsizei where
    to_g = GPrimitive . S.PGLsizei
    
instance AsC GLubyte where
    to_g = GPrimitive . S.PGLubyte

instance AsC GLuint where
    to_g = GPrimitive . S.PGLuint

instance AsC GLushort where
    to_g = GPrimitive . S.PGLushort

instance GAsC U1 where
    g_to_g U1 = GEmpty
    
instance (GAsC a, GAsC b) => GAsC (a :+: b) where
  g_to_g (L1 x) = g_to_g x
  g_to_g (R1 x) = g_to_g x
  
instance (GAsC a, GAsC b) => GAsC (a :*: b) where
    g_to_g (a :*: b) = GPair (g_to_g a) (g_to_g b)
  
instance (GAsC a, Datatype c) => GAsC (D1 c a) where
    g_to_g x = GStruct (datatypeName x) $ GUnion $ g_to_g $ unM1 x 
  
instance (GAsC a, Constructor c) => GAsC (C1 c a) where
    g_to_g x = GMember (conName x) $ g_to_g $ unM1 x
    
instance (GAsC a) => GAsC (S1 c a) where
    g_to_g = g_to_g . unM1

instance (AsC a) => GAsC (K1 i a) where
    g_to_g = to_g . unK1

data TestData = Constructor1 GLuint GLboolean GLchar
              | Constructor2 GLint
              deriving(Generic)

instance AsC TestData where

test = Constructor1 1 (GLboolean True) (GLchar 'd')

test_desc = to_g test

members = get_struct_members test_desc

unfold_pairs (GPair x y) = x:(unfold_pairs y)
unfold_pairs x           = x:[]

just_params = unfold_pairs $ (\(GMember _ x) -> x) members
                    

{-

I need a generic way to create the ToCValue instances
I should start with the th descriptions of a type
From there I go to the c description of a type
Based on the c description of a type I can build the to CValue

-}

--write out what the c code should look like
--make the CDescription from that and the instance of ToCDescription
--

{-
    

    typedef struct TestExpression_t {
        
    } TestExpression;

-}

--data TestExpression = Add Expression Expression
--                    | Multiply Expression Expression
--                    | Lit GLint

--instance ToCDescription TestExpression where
--    to_c_description x = [  PrimitiveD $ Enum [
--                                ("TEST_EXPRESSION_ADD", 0),
--                                ("TEST_EXPRESSION_MULTIPLY", 1),
--                                ("TEST_EXPRESSION_LIT", 2)
--                                ],
--                            StructD "TestExpression" [
--                            ,
--                            Union "" 
--                    
--                        ]]

--data PrimitiveType = Enum [(String, Word32)]
--data CDescription = StructD String [CDescription]
--                  | UnionD  String [CDescription]
--                  | PrimitiveD PrimitiveType 
--                  | VariableArray CDescription
--                  | FixedArray Word32 CDescription
--                  | MemberD String CDescription

--class ToCDescription a where
--    to_c_description :: a -> [CDescription]































