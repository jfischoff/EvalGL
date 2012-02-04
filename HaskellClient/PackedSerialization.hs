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

--each command has a id associated with it
--
--There is a general way that this serialization uses
--the members of a type have an amount of padding associated with them

--There is a general method for going from a haskell type to c type

--Every type is a struct
--if there are more then one constructors
--then the struct has a union
--and a enum for the type

--Haskell Type -> Description -> CType -> String
--Haskell Type -> Description -> PaddedDescription

--I need to think about this more


data GLPrimitive = PGLbitfield GLbitfield
                 | PGLboolean GLboolean
                 | PGLbyte GLbyte
                 | PGLchar GLchar
                 | PGLclampf GLclampf
                 | PGLenum GLenum
                 | PGLfloat GLfloat
                 | PGLint GLint
                 | PGLshort GLshort
                 | PGLsizei GLsizei
                 | PGLubyte GLubyte
                 | PGLuint GLuint
                 | PGLushort GLushort
                 deriving(Show, Eq, Data, Typeable)

data Description = StructDescription String Description
                 | UnionDescription  String Description
                 | Primitive GLPrimitive
                 | ArrayDescription Int Description
                 | EmptyDescription
                 | Pair Description Description
                 | Member String Description
                 deriving(Show, Eq, Data, Typeable)
                 
get_struct_members (StructDescription x (UnionDescription _ y)) = y
                 
data PaddedDescription = PaddedStructDescription [(String, PaddedDescription, Int)]
                       | PaddedUnionDescription  [(String, PaddedDescription)] Int
                       | PaddedPrimitive GLPrimitive
                       | PaddedArrayDescription PaddedDescription Int Int
                      
type Offset = Int
type Fixup = Offset

class GAsC f where
  g_to_c :: f a -> Description

class AsC a where
    to_c :: a -> Description
    default to_c :: (Generic a, GAsC (Rep a)) => a -> Description
    to_c a = g_to_c (from a)
    
instance AsC GLbitfield where
    to_c = Primitive . PGLbitfield
    
instance AsC GLboolean where
    to_c = Primitive . PGLboolean
    
instance AsC GLbyte where
    to_c = Primitive . PGLbyte
    
instance AsC GLchar where
    to_c = Primitive . PGLchar
                     
instance AsC GLclampf where
    to_c = Primitive . PGLclampf
    
instance AsC GLenum where
    to_c = Primitive . PGLenum
             
instance AsC GLfloat where
    to_c = Primitive . PGLfloat
    
instance AsC GLint where
    to_c = Primitive . PGLint
    
instance AsC GLshort where
    to_c = Primitive . PGLshort
    
instance AsC GLsizei where
    to_c = Primitive . PGLsizei
    
instance AsC GLubyte where
    to_c = Primitive . PGLubyte

instance AsC GLuint where
    to_c = Primitive . PGLuint

instance AsC GLushort where
    to_c = Primitive . PGLushort

instance GAsC U1 where
    g_to_c U1 = EmptyDescription
    
instance (GAsC a, GAsC b) => GAsC (a :+: b) where
  g_to_c (L1 x) = g_to_c x
  g_to_c (R1 x) = g_to_c x
  
instance (GAsC a, GAsC b) => GAsC (a :*: b) where
    g_to_c (a :*: b) = Pair (g_to_c a) (g_to_c b)
  
instance (GAsC a, Datatype c) => GAsC (D1 c a) where
    g_to_c x = StructDescription (datatypeName x) $ UnionDescription "" $ g_to_c $ unM1 x 
  
instance (GAsC a, Constructor c) => GAsC (C1 c a) where
    g_to_c x = Member (conName x) $ g_to_c $ unM1 x
    
instance (GAsC a) => GAsC (S1 c a) where
    g_to_c = g_to_c . unM1

instance (AsC a) => GAsC (K1 i a) where
    g_to_c = to_c . unK1

data TestData = Constructor1 GLuint GLboolean GLchar
              | Constructor2 GLint
              deriving(Generic)

instance AsC TestData where

test = Constructor1 1 1 (GLchar 'd')

test_desc = to_c test

members = get_struct_members test_desc

unfold_pairs (Pair x y) = x:(unfold_pairs y)
unfold_pairs x          = x:[]

just_params = unfold_pairs $ (\(Member _ x) -> x) members


                      
pack :: Command -> BS.ByteString
pack = undefined --must be a CommandList and the last command must be a loop command
--roll through and concat the fixups
--calculate the space of the head
--update the fixups
--write out the header and the body


pack' :: Command -> (BS.ByteString, [Fixup])
pack' = undefined --this for the sub commands


pad :: Description -> PaddedDescription
pad = undefined





























